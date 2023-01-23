# sleepIPD_analysis
Analysis for the sleep and physical activity pooled study (https://osf.io/gzj9w/)

This repository contains the code and analysis for the individual participant meta-analysis on the relationship between sleep and physical activity. The project is set up using the `targets` package, and packages are managed using `renv`.

## Getting Started

To get started with this project, follow these steps:

1. Install the renv package, if you haven't already:
```r
install.packages("renv")
```
2. Clone the repository to your local machine
3. Install the required packages using `renv`:
```r
renv::restore()
```

### Getting the Data

Data for the project are stored in cloudstor. To save `targets` checking the data every time, the data are downloaded to the `data` folder using the `fetch_data()` function. You will need to authenticate [`cloudstoR`](https://cran.r-project.org/package=cloudstoR) to access your cloudstor account if you have not used it before.

1. Load the `fetch_data()` function (`devtools::load_all()` will work)
2. Run `fetch_data()` to download the data to the `data` folder (you may need to change base_folder: `fetch_data(base_folder = "Shared/Motivation and Behaviour Program/Pooled Sleep Study")`)

### Running the Pipeline

Run the pipeline with `targets::tar_make()`.

## Contributing

This project has a protected `main` branch. To contribute to the project, please create a new branch and submit a pull request to merge into `main`.
