# Have most up to date version of R installed: https://cran.r-project.org/bin/windows/base/

if(!require("readxl")){
  install.packages("readxl",dependencies = TRUE)
  library(readxl)
}
if(!require("ggplot2")){
  install.packages("ggplot2",dependencies = TRUE)
  library(ggplot2)
}
if(!require("psych")){
  install.packages("psych",dependencies = TRUE)
  library(psych)
}
if(!require("dplyr")){
  install.packages("dplyr",dependencies = TRUE)
  library(dplyr)
}
if(!require("lsr")){
  install.packages("lsr",dependencies = TRUE)
  library(lsr)
}
if(!require("car")){
  install.packages("car",dependencies = TRUE)
  library(car)
}
if(!require("effsize")){
  install.packages("effsize",dependencies = TRUE)
  library(effsize)
}
if(!require("data.table")){
  install.packages("data.table",dependencies = TRUE)
  library(data.table)
}
if(!require("tidyr")){
  install.packages("tidyr",dependencies = TRUE)
  library(tidyr)
}