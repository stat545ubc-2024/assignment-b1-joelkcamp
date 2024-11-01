### UBC STAT 545B - Assignment B1
##### Author: Joel Campbell

#### Description

This github repository is for STAT 545B assignment b1.

This assignment consists of the creation of a function that generates scatter plots for all possible combination of numerical variables from a passed in data frame or table. The remainder of the assignment involves generating some examples, and automated testing, for this function.

#### Contents

- assignment_b1.Rmd: The R markdown file that contains the generate_scatter_plots functions and all related examples and testing.
- assignment_b1.md: The file produced by knitting the .Rmd file.
- assignment_b1/figure-gfm: the folder containing .png files for all plots produced in the function examples.

#### How-to

To view the output for assignment_b1 open assignment_b1.md in the repo root folder, or knit the assignment_b1.Rmd file.

To replicate the output for the assignment_b1.Rmd file, first ensure that the following commands have been run in your R terminal:

- install.packages("devtools")
- devtools::install_github("UBC-MDS/datateachr")
- install.packages("tidyverse")
- install.packages("testthat")

Next, clone the repository, and follow through the mini-project-1.Rmd file, executing each code chunk in the order they are encountered.

Note the following information regarding R version and packages:

- the code was initially written and run using R version 4.4.1
- required packages:
    - tidyverse:
        - purrr
        - ggplot2
        - dplyr
    - testthat
    - the data analyzed is from UBC-MDS/datateachr, or built in R data sets