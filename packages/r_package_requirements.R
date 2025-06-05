# R version: 4.3.1
# This script installs specific versions of R packages for reproducibility

# Load required package to install specific versions
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

# Install packages with specified versions
# We use remotes::install_version() to fetch packages from CRAN 
# archives at the exact versions.
remotes::install_version("tidyverse", version = "2.0.0")
remotes::install_version("readr", version = "2.1.5")
remotes::install_version("dplyr", version = "1.1.4")
remotes::install_version("ggplot2", version = "3.5.1")
remotes::install_version("tidytext", version = "0.4.2")
remotes::install_version("xgboost", version = "1.7.8.1")
remotes::install_version("randomForest", version = "4.7-1.2")
remotes::install_version("caret", version = "7.0.1")
remotes::install_version("Matrix", version = "1.6-4")
remotes::install_version("modelsummary", version = "2.3.0")
remotes::install_version("DescTools", version = "0.99.53")
remotes::install_version("gofcat", version = "0.1.2")
remotes::install_version("VGAM", version = "1.1-13")
remotes::install_version("fastDummies", version = "1.7.4")
remotes::install_version("stargazer", version = "5.2.3")
remotes::install_version("sandwich", version = "3.1.0")
remotes::install_version("zoo", version = "1.8-12")
remotes::install_version("lmtest", version = "0.9-40")
remotes::install_version("MASS", version = "7.3-60")
remotes::install_version("pscl", version = "1.5.9")
remotes::install_version("car", version = "3.1-2")
remotes::install_version("ucminf", version = "1.2.1")
remotes::install_version("ordinal", version = "2023.12-4")
remotes::install_version("reshape", version = "0.8.9")
remotes::install_version("generalhoslem", version = "1.3.4")
remotes::install_version("oglmx", version = "3.0.0.0")
remotes::install_version("aod", version = "1.3.3")
remotes::install_version("brant", version = "0.3.0")
remotes::install_version("corrplot", version = "0.92")
remotes::install_version("texreg", version = "1.39.4")
