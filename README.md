# pgs_aseba_project

<a target="_blank" href="https://cookiecutter-data-science.drivendata.org/">
    <img src="https://img.shields.io/badge/CCDS-Project%20template-328F97?logo=cookiecutter" />
</a>

This study investigates sex differences in the association between an autism polygenic score and an autism subscale of the ASEBA questionnaires. These questionnaires were taken in a general population. Rater effects are investigated as the same questionnaire was taken via self-, parent- and teacher-reports (YSR, TRF and CBCL respectively).

# Recreate analysis

In order to recreate the analysis, we need to install the necessary dependencies for Python and R.

The required dependencies are in pyproject.toml and renv.lock. The necessary installments are as follows:

- [`make`](https://www.gnu.org/software/make/)
- [`R`](https://cran.r-project.org/) (v4.5.0)
- [`Python`](https://www.python.org/) (v3.13)
- [`renv`](https://rstudio.github.io/renv/)
- [`uv`](https://github.com/astral-sh/uv)

Next to recreate the project, in a terminal in the project root type:

```
make
```
This will create a virtual environment and install the required Python dependencies. It will also create an R environment and install the required R dependencies.

To recreate the analyses, run scripts/main.R.

## Project Organization

```
├── LICENSE            <- Open-source license if one is chosen
├── Makefile           <- Makefile with convenience commands like `make data` or `make train`
├── README.md          <- The top-level README for developers using this project.
├── data
│   ├── external       <- Data from third party sources.
│   ├── interim        <- Intermediate data that has been transformed.
│   ├── processed      <- The final, canonical data sets for modeling.
│   └── raw            <- The original, immutable data dump.
│
├── docs               <- A default mkdocs project; see www.mkdocs.org for details
│
├── models             <- Trained and serialized models, model predictions, or model summaries
│
├── notebooks          <- Jupyter notebooks. Naming convention is a number (for ordering),
│                         the creator's initials, and a short `-` delimited description, e.g.
│                         `1.0-jqp-initial-data-exploration`.
│
├── pyproject.toml     <- Project configuration file with package metadata for 
│                         pgs_aseba_project and configuration for tools like black
│
├── references         <- Data dictionaries, manuals, and all other explanatory materials.
│
├── reports            <- Generated analysis as HTML, PDF, LaTeX, etc.
│   └── figures        <- Generated graphics and figures to be used in reporting
│
├── requirements.txt   <- The requirements file for reproducing the analysis environment, e.g.
│                         generated with `pip freeze > requirements.txt`
│
├── setup.cfg          <- Configuration file for flake8
│
└── pgs_aseba_project   <- Source code for use in this project.
    │
    ├── __init__.py             <- Makes pgs_aseba_project a Python module
    │
    ├── config.py               <- Store useful variables and configuration
    │
    ├── dataset.py              <- Scripts to download or generate data
    │
    ├── features.py             <- Code to create features for modeling
    │
    ├── modeling              
    │   ├── __init__.py 
    │   ├── predict.py          <- Code to run model inference with trained models        
    │   └── train.py            <- Code to train models
    │
    └── plots.py                <- Code to create visualizations
```

---
