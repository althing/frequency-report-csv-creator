




# libraries
if (TRUE) {

library(data.table)
library(dplyr)
library(stringr)
library(srvyr)
library(survey)
library(tidyr)


}


# options -----
if (TRUE) {
    options(scipen=999)
    options(max.print = 1000)
}


# data import -----
# put data into a list of df's.
# create a list of svydesign objects (one for each df).
if (TRUE) {
    df1 <- read.csv("./survey data/SMOKE22_US_data_wtd.csv")
    #df2 <- read.csv("./survey data/SMOKE22_WEST_EN_data_wtd.csv")
    #df3 <- read.csv("./survey data/SMOKE22_WEST_SP_data_wtd.csv")

    #my_list <- list(df1, df2, df3)
    my_list <- list(df1)

    my_list.wtd <- list()
    for (i in 1:length(my_list)) {
        x <- svydesign(ids = ~1, weights = ~weight, data = my_list[[i]])
        my_list.wtd <- append(my_list.wtd, x)
    }
}




# specify variable range -----
var.names <- names(my_list[[1]])
# a <- which(var.names == "wfsm_likely")
# b <- which(var.names == "wfsm_und")
a <- which(var.names == "wf_worry")
b <- which(var.names == "wfsm_likely")
var.names <- var.names[a:b]

# make empty matrix
# must have correct number of columns
mtrx <- matrix(, nrow = 0, ncol = 6) 

# make loop skip errors. 
# https://stackoverflow.com/questions/14748557/skipping-error-in-for-loop

for (h in 1:length(var.names)) {
    tryCatch({      # makes loop skip errors. 

    var.name.row <- t(matrix(append(var.names[h], rep("", 5)))) # must have same number of values as ncol of mtrx
    mtrx <- rbind(mtrx, var.name.row)

    for (i in 1:length(my_list)) {

        svy.obj <- svydesign(ids = ~1, weights = ~weight, data = my_list[[i]])

        x <- round(prop.table(svytable(as.formula(paste( "~" , var.names[h] )), design = svy.obj)) * 100, 2)
        y <- round(
            svymean(
                as.formula(paste( "~" , var.names[h])),
                na.rm = TRUE,
                design = svy.obj      
                ), 1)   
        
        y <- y[1]   # extracts mean (2 is SE).
        # names(y) <- "mean"  # labels col 'mean'   # matricies don't retain col names.
        x <- append(x, y[1])


        x <- t(matrix(x))

        mtrx <- rbind(mtrx, x)



    }

    }, error = function(e){})


}

df <- as.data.frame(mtrx)


# output
output_table_name <- "my table"
fwrite(df, paste0("./csv outputs/", output_table_name, ".csv"), col.names = FALSE)


#! check this about linter problem:
# https://stackoverflow.com/questions/67481802/a-question-about-line-length-linter-80-characters
# https://github.com/r-lib/lintr#available-linters
# https://www.rdocumentation.org/packages/lintr/versions/1.0.3
