# Reproducible Research Project

This repository contains the original R script and an adequate version in Jupyter Notebook of the Econometrics project. It analyzes and models the IMDb ratings of streaming shows using various econometric and machine learning tools. 

## Dataset Information

- **Source:** Kaggle  
- **Title:** Web Series Ultimate Edition  
- **Link:** [Kaggle Dataset](https://www.kaggle.com/datasets/amritvirsinghx/web-series-ultimate-edition)

## Repository Structure

- `data/` folder consists of the dataset `All_Streaming_Shows.csv` used in the analysis
- `scripts/` folder containing both scripts
- `Original Script - Econometrics Project.r` is the original R script that we aim to reproduce in Python
- `Updated Script - Python.ipynb` is the Jupyter Notebook version of the original script
- `Comparison_of_results.pdf` which contains comparisons of the model's results between R and Python
- `install_packages.r` contains R file for the installation of necessary packages
- `requirements.txt` contains Python package requirements

## Setup Instructions

Before running the code:

1. **Verify your environment:**
   - R version (for `.r` script)
   - Python version (for `.ipynb` notebook)
  
2. **Install required packages:**
   - Run `install_packages.r` for R
   - Run `pip install -r requirements.txt` for Python

3. **Check data paths:**  
   Ensure the dataset is located in the `data/` folder and file paths in the scripts point to the correct location.

4. **Run scripts:**
   - Use RStudio or an R terminal for the R script.
   - Use Jupyter Notebook or a compatible IDE for the Python version.
     
## Notes

- Each step in the analysis is documented within the code files using in-line comments.
- Both R and Python versions aim to provide a consistent and reproducible research workflow.
