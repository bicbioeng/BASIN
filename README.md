# BASIN
An R package that utilizes Shiny to provide a user interface
for statistical analysis of two-dimensional confocal microscope images. Users
can upload two images directly or a folder of images with the help of a
user-generated csv file, edit their experimental design, create tables and
graphs for analysis results, and generate a fully-formatted report of their
experiment.

## BASIN Overview and Quick Tutorial
A simplified version of BASIN is available through shinyApps at [http://bicbioeng.shinyapps.io/tryBASIN](http://bicbioeng.shinyapps.io/tryBASIN). This
version only takes in 2 images, but the workflow is nearly identical to the complete version and serves as a gentle tutorial to most of BASIN's features.
 Note that for the full version of BASIN requires the user to download a csv
 containing the names of the images uploaded and assign 'control' and 'test'
 bioconditions manually, in addition to experiment number(s), which must be positive integers only.

## Installation and Usage

### Option 1: Shiny Application

#### Python Setup:
1. Install Anaconda on your local machine:
  1. Quick Setup - install Miniconda using the following link: https://docs.conda.io/en/latest/miniconda.html
  2. If any successive steps don’t work, uninstall Miniconda and install Anaconda instead using the following link: https://docs.anaconda.com/anaconda/install/
2. Open the Anaconda terminal (Anaconda Prompt) and switch to the folder containing the “full_environment.yml” file using ` cd path\to\folder\... `
3. Install the BASIN python environment using the command ` conda env create -f full_environment.yml ` - this will take a few minutes
4. Ensure the installation worked by executing the following commands in the terminal:
  1. Activate the environment using “conda activate basin”
  2. Run cellpose using “python -m cellpose”
  3. If the cellpose GUI appears, your installation has been successful

#### R Setup:
1. Install the required R and Bioconductor packages:
  `install.packages(c("purrr", "plyr", "shiny", "shinyBS", "shinyjs",
    "shinydashboard", "shinycssloaders", "shinythemes", "shinyWidgets",
    "DT", "stringi", "ggpubr", "tcltk", "autothresholdr"))`

  `if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager") #installs Bioconductor
    BiocManager::install("EBImage") #installs EBImage`
    
2. Install the reticulate, keras, and tensorflow packages in RStudio using

  `install.packages(c(“reticulate”, “keras”, “tensorflow”))`

3. Test the ability for the packages to connect to the Python environment:
  1. Run the following commands in R and check for errors:
  `library(reticulate)
  env <- conda_list()$name == "basin"
  envPath <- conda_list()[env,]$python
  envPath <- stringi::stri_replace(envPath,"",regex = "python.exe")
  reticulate::use_condaenv(envPath, required=TRUE)
  keras::use_condaenv(envPath, required=TRUE)
  tensorflow::use_condaenv(envPath, required=TRUE)`
  
  2. Restart your R session and run the BASIN app from the server.R or ui.R files inside of the shinyBASIN folder.

### Option 2: Package (No Python, No ML Segmentation)
Details on the features and functionality of BASIN can be found in the BASIN
vignette, which is accessible through the package itself. To install BASIN
in R, download the tarball file BASIN_0.99.0.tar.gz into your local machine
and use the command `install.packages("path/to/BASIN")`, replacing the
"path/to/BASIN" with the location of the file. Once installed, load the
package using library(BASIN). The package vignette can be accessed using the
command `browseVignettes("BASIN")` and will contain further instructions on
using the package.
