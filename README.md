# pgs_aseba_project

This study investigates sex differences in the association between an autism polygenic score and an autism subscale of the ASEBA questionnaires. These questionnaires were taken in a general population. Rater effects are investigated as the same questionnaire was taken via self-, parent- and teacher-reports (YSR, TRF and CBCL respectively).


```
# Recreate analysis

The data was requested from NTR following the data_request_1.json file. The data was downloaded from the NTR website and is stored in the `data/raw` folder. The data is not included in this repository due to its size and privacy concerns. The data can be requested from NTR by following the instructions on their website: https://ntr-data-request.psy.vu.nl/.


The required dependencies are in renv.lock. The necessary installments are as follows:

- [`R`](https://cran.r-project.org/) (v4.5.0)
- [`renv`](https://rstudio.github.io/renv/)


To recreate the analyses, generate plots and create excel file data dumps, run numbered script files (/scripts folder) in order.

## Project Organization

The project is structured according to the [Cookiecutter Data Science](https://drivendata.github.io/cookiecutter-data-science/) template. This template provides a logical, reasonably standardized, but flexible project structure for doing and sharing data science work.

FILE STRUCTURE
├── LICENSE            <- Open-source license (MIT)
├── README.md          <- you are reading me now!
├── data
│   ├── processed      <- The final, canonical data sets for modelling.
│   └── raw            <- The original, immutable data dump.
│
├── docs               <- Contains all files for the data request to the NTR
│
├── results            <- Results from analyses. Includes figures, fitted models, excel files from post-hoc investigations
│
├── scripts            <- All R scripts necessary to run the project. Run in order. Some loose files include collection of xlsx 
│   │                    files,  power calculations and simulation modelling to determine to use ordinal regression.
│   └── Measurement invariance            <- contains the mplus input files necessary to run the measurement invariance testing.
