# Cockey Investments

## Overview
A comprehensive stock pricing analysis project for STAT 542, examining investment opportunities across multiple sectors including Automotive, Financials, Healthcare, and Media/Entertainment.

## Webpage Link
 - https://cyrusjones159.github.io/Cockey-Investments/

## Project Structure

### Sector Analysis Directories
- **Automotive/** - Consumer Discretionary & Automotive sector analysis
- **Financials/** - Financial sector investment screening
- **Healthcare/** - Healthcare sector analysis and screening results
- **Media/** - Media and Entertainment sector screening results

### Key Resources
- **Stage1Filtering/** - Initial screening results across all sectors
- **PresentationDocuments/** - Project presentation materials and documentation

## Contents
### Automotive 
 - **Automotive Consumer Discretionary/** - [ConsumerDiscretionaryAutoscreener_results.xls](https://github.com/user-attachments/files/26467949/ConsumerDiscretionaryAutoscreener_results.xls)
 - **Automotive Industrial Screener/** - [IndustrialsScreener_results.xls](https://github.com/user-attachments/files/26467957/IndustrialsScreener_results.xls)
### Financials
 - **Financials Screener/** - [FinancialsScreener_results.xls](https://github.com/user-attachments/files/26467974/FinancialsScreener_results.xls)
### Media 
 - **Media Entertainment Screener/** - [MediaEntertainmentScreener_results.xls](https://github.com/user-attachments/files/26468009/MediaEntertainmentScreener_results.xls)
 - **Second Media Screener/** - [screener_results.xls](https://github.com/user-attachments/files/26468012/screener_results.xls)

## Shiny App
The repository now includes a Shiny dashboard in [app.R](app.R) that uses the Stage 3 financial summary layout as the display model.

The app lets you switch between sectors and then choose the Excel file you want to inspect.

Available sector folders include:
- [Automotive/](Automotive/)
- [Financials/](Financials/)
- [Healthcare/](Healthcare/)
- [Media/](Media/)

To run it from R:
```r
install.packages(c("shiny", "bslib", "dplyr", "readxl", "DT", "ggplot2", "scales"))
shiny::runApp("app.R")
```

The app can read the Excel files in those folders, including the saved financial summary file at [Financials/financials.result.xlsx](Financials/financials.result.xlsx).

## Filtering Setup

### Stage 1 Filtering
Stage 1 is a manual screening step based on market cap, stock price, and dividend stability. No project-specific R or Python libraries are required for this stage.

### Stage 2 Filtering
Stage 2 uses both Python and R.

Python packages:
```bash
pip install yfinance pandas requests
```

Python imports:
```python
import yfinance as yf
import pandas as pd
import requests
```

R packages:
```r
install.packages(c("quantmod", "dplyr", "purrr", "tidyverse", "readr", "readxl", "wget", "sqldf", "janitor"))
```

R libraries:
```r
library(quantmod)
library(dplyr)
library(purrr)
library(tidyverse)
library(readr)
library(readxl)
library(wget)
library(sqldf)
library(janitor)
```

### Stage 3 Filtering
Stage 3 uses R.

R packages:
```r
install.packages(c("writexl", "lubridate", "tidyr", "quantmod", "purrr", "tidyverse", "readr", "readxl", "wget", "dplyr", "sqldf", "janitor"))
```

R libraries:
```r
library(writexl)
library(lubridate)
library(tidyr)
library(quantmod)
library(purrr)
library(tidyverse)
library(readr)
library(readxl)
library(wget)
library(dplyr)
library(sqldf)
library(janitor)
```

## About This Project
This project utilizes statistical analysis and stock screening techniques to identify potential investment opportunities across diverse market sectors.
