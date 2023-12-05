# sleepIPD_analysis
Analysis for the sleep and physical activity pooled study (https://osf.io/gzj9w/)

This repository contains the code and analysis for the individual participant meta-analysis on the relationship between sleep and physical activity.
The project is set up using the `targets` package, and packages are managed using `renv`.

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

Data for the project are stored on Sharepoint.
To save `targets` checking the data every time, the data are downloaded to the `data` folder using the `fetch_data()` function.
You will need to authenticate [`Microsoft365R`](https://cran.r-project.org/package=Microsoft365R) to access your Sharepoint account if you have not used it before.
You also need access to the data folder.

1. Load the `fetch_data()` function (`devtools::load_all()` will work)
2. Run `fetch_data()` to download the data to the `data` folder.

### Running the Pipeline

There are two ways to run the full pipeline:

1. In an R session, run the pipeline with `targets::tar_make()`.
2. In the terminal, run `Rscript make.R`.
This is slightly faster because it runs some sections of the pipeline in parallel.

### Accessing Google Cloud Storage

The pipeline uses Google Cloud Storage to store the results of the pipeline.
This means that very long running pipelines can be run on a remote machine, and the results can be accessed from anywhere.
To access these pre-built targets, you will need to have owner access to the Google Cloud Storage bucket.

For all of these, use the project `SleepIPD`.

#### Download Client ID File

You need two credentials to access the Google Cloud Storage bucket.
The first is a client ID file, and the second is a JSON service account key.

To get the client ID file, follow these steps (provided by `googleCloudStorageR`):

* Go to [Google Cloud Console OAuth Creation Page](https://console.cloud.google.com/apis/credentials/oauthclient)
* Desktop app > Name > Create > Download JSON

Or to access an existing ID:

* Go to [Google Cloud Console List of Credentials](https://console.cloud.google.com/apis/credentials)
• OAuth 2.0 Client IDs > Click Download Arrow to the right

Either way, you need to save this file to somewhere secure on your machine.

#### Download Service Account Key

To get the service account key, take advantage of `googleCouldStorageR` can generate it for you:

```r
library(googleCloudStorageR)
gcs_setup()
```

Select 1 for creating the JSON key, then follow the prompts.
You'll need to tell it the location of the client ID file you downloaded in the previous step, and then ask it to generate a service account key.
It will need to authenticate with your Google account, and then it will download the service account key to your machine.
It saves the json file to the project directory, but I'd recommend moving it somewhere more secure.

You then need to rerun `gcs_setup()` and select 2 to authenticate with the service account key.

Then restart your R session.
Running `Sys.getenv("GCS_AUTH_FILE")` should return the location of the service account key.

## Contributing

This project has a protected `main` branch. To contribute to the project, please create a new branch and submit a pull request to merge into `main`.
