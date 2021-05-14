<!-- badges: start -->
  [![R-CMD-check](https://github.com/tadhg-moore/SimstratR/workflows/R-CMD-check/badge.svg)](https://github.com/tadhg-moore/SimstratR/actions)
  <!-- badges: end -->

SimstratR
====


R package for basic [Simstrat](https://github.com/Eawag-AppliedSystemAnalysis/Simstrat) model running. `SimstratR` holds the version compiled or downloaded from the [website](https://github.com/Eawag-AppliedSystemAnalysis/Simstrat/releases) on 2020-06-08 (version 2.4.1) and should run virtually on any Windows, Linux and macOS system.  This package does not contain the source code for the model, only the executable. This package was inspired by [GLMr](https://github.com/GLEON/GLMr).

## Installation

You can install SimstratR from Github with:

```{r gh-installation, eval = FALSE}
# install.packages("devtools")
devtools::install_github("aemon-j/SimstratR")
```
## Usage
Compatible with Windows, Linux (tested on Ubuntu 18.04.3 LTS) and macOS X.

### Run

```{r example, eval=FALSE}
library(SimstratR)
sim_folder <- system.file('extdata', package = 'SimstratR')
run_simstrat(sim_folder, par_file = 'langtjern.par')
```

### Output
```{r example, eval = FALSE}
library(LakeEnsemblR)
par_file = file.path(sim_folder, 'langtjern.par')

out_file <- file.path(sim_folder, 'Results', 'T_out.dat')
### Extract output
sim_out <- read.table(out_file, header = T, sep=",", check.names = F)

### Convert decimal days to yyyy-mm-dd HH:MM:SS
timestep <- get_json_value(par_file, "Simulation", "Timestep s")
reference_year <- get_json_value(par_file, "Simulation", "Start year")
# Convert date
sim_out[,1] <- as.POSIXct(sim_out[,1]*3600*24, origin = paste0(reference_year,"-01-01"))
# In case sub-hourly time steps are used, rounding might be necessary
sim_out[,1] <- lubridate::round_date(sim_out[,1], unit = lubridate::seconds_to_period(timestep))

# First column datetime, then depth from shallow to deep
sim_out <- sim_out[,c(1,ncol(sim_out):2)]

# Remove columns without any value
sim_out <- sim_out[,colSums(is.na(sim_out))<nrow(sim_out)]
# Extract depths for colnames
depths <- abs(as.numeric(colnames(sim_out)[-1]))
colnames(sim_out) <- c("datetime", paste0('wtr_',abs(depths)))

# Plot heatmap 
rLakeAnalyzer::wtr.heat.map(sim_out)

```

Suite of tools for working with Simstrat output coming soon...
