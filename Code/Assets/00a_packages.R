
## Initialize R session
rm(list = ls()); gc()
cat("\14")
options(digits = 2)

## Packages
if(!require("pacman")) install.packages("pacman")

pacman::p_load(haven, data.table, tidyverse, e1071, visdat,
               texreg, rockchalk, labelled, mltools, gt, gtExtras,
               DescTools)
