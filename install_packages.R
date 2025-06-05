# install_packages.R for version 4.3.1

required_packages <- c("tidyverse", "readr", "dplyr", "ggplot2", "tidytext", "xgboost", "randomForest", "caret", "Matrix", "modelsummary", "Matrix", "DescTools", "gofcat",  "VGAM", "fastDummies", "stargazer", "sandwich", "zoo", "lmtest", "MASS",  "pscl", "car", "ucminf", "ordinal", "reshape", "generalhoslem", "oglmx", "aod", "brant", "corrplot", "texreg")

install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
}

invisible(lapply(required_packages, install_if_missing))
