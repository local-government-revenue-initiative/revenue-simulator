# Property Tax Revenue Simulator

A tool for policy makers to explore how adjustments to property tax configurations impact property value estimates and revenue.

## Overview

This Shiny web application provides a comprehensive simulator for property tax analysis, consisting of five interconnected modules:

1. **Module 1: Data Input & Preprocessing** - Upload and process property, payment, and business data
2. **Module 2: Parameter Configuration** - Set tax rates and property valuation weights
3. **Module 3: Revenue Calculations** - Calculate potential tax revenue
4. **Module 4: Tax Burden Analysis** - Analyze tax distribution across property value quantiles
5. **Module 5: GIS Visualization** - Visualize tax data geographically

## Installation

### Prerequisites

- R (version 4.0 or higher)
- RStudio (recommended)

### Required R Packages

```r
install.packages(c(
  "shiny",
  "shinydashboard",
  "DT",
  "tidyverse",
  "readr",
  "fastDummies"
))