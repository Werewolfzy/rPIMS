# Welcome to rPIMS
A Shiny GUI Package for Rapid Breed Identification Using Machine Learning

## Description

The rPIMS package has been developed to provide breed conservationists with a comprehensive and efficient workflow for constructing breed identification models utilizing a wide array of machine learning techniques. This package is capable of processing genomic data and tabular text files, integrating diverse analytical modules to ensure robust and accurate breed identification, including: (1) a Data Input Module (DATA), which supports the import of genomic, breed, and geographic sampling information; (2) a Dimensionality Reduction Module (DimRed), which provides dimensionality reduction tools to aid in the visualization and interpretation of complex datasets; (3) a Phylogenetic Tree Construction Module (PhyloTree), which constructs evolutionary trees to visualize genetic relationships among breeds; (4) a Population Structure Analysis Module (Structure), which reveals genetic structure and historical patterns of population stratification; (5) a Machine Learning Model Training Module (TrainModel), which supports multiple machine learning algorithms to train efficient breed classification models and generate molecular identity cards for individual breeds; and (6) a Model Usage Module (PredNewind), which allows users to apply trained models for accurate breed identification. Together, these modules provide a seamless process for rapid and accurate breed identification, contributing to effective breed conservation efforts.

## Getting Started

A manual providing instructions and examples for using rPIMS can be found [here!](https://github.com/Werewolfzy/rPIMS/blob/master/doc/manual.pdf)
And a video tutorial is linked [here!](https://github.com/Werewolfzy/rPIMS/blob/master/doc/rPIMS_manual_video.wmv)

### Installing

`install.packages("BiocManager")`

`install.packages(c("shiny", "shinythemes", "shinyjs", "DT", "pwr", "dplyr", "FNN", "shinyWidgets", "colourpicker", "RColorBrewer", "ggplot2", "data.table", "sommer", "ape", "pbapply", "infotheo", "caret", "class", "randomForest", "xgboost", "ranger", "ROCR", "smacof", "umap", "phangorn", "shinyalert", "leaflet", "e1071", "kernlab"))`

`BiocManager::install("LEA")`

`BiocManager::install("ggtree")`

### Launch rPIMS

We provide the rPIMS package on GitHub, available for download in the "Releases" section. You can find the rPIMS_0.1.0.tar.gz file  (https://github.com/Werewolfzy/rPIMS/releases). This package is designed to streamline your data analysis and visualization tasks, offering a user-friendly interface for a range of functionalities.
To install the package locally, use the following command in R:

`install.packages("path/to/rPIMS_0.1.0.tar.gz", repos = NULL, type = "source")`

Once installed, simply run the package by executing:

`rPIMS.GUI()`
