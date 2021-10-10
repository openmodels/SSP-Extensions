# SSP-Extensions
A Bayesian model for extending the SSP socioeconomics.

This repository provides code and documentation for generating CSVs
that describe the extensions to the SSP socioeconomics past 2100. This
work is currently used by MimiPAGE2020
(https://github.com/openmodels/MimiPAGE2020.jl) and META-2021
(https://github.com/openmodels/META-2021).

The process is described in `docs/main.pdf`.

The repository contains two parallel sets of code and results. One uses data from the PAGE-ICE model (PAGE version), pre-aggregated to the PAGE region level; the other uses the SSP 2.0 database (https://tntcat.iiasa.ac.at/SspDb/dsd?Action=htmlpage&page=about) directly (SSP 2.0 version).

The process starts with SSP projected growth rates from 2015 to 2100,
either according to the regional definitions used by PAGE (PAGE version) or by country (SSP 2.0 version). These files are stored in `data/`.

The script `src/bayesfit.R` (PAGE version) or `src/bayesfit-ssp2.0.R` (SSP 2.0 version) applies a Bayesian model to fit the
convergent growth equations.  It does this in such a way that it (1)
recalculates the mean growth rate (for convergence) every year, (2)
fits the convergence and decay rates under that yearly process, and
(3) keeps track of the full range of Bayesian uncertainty during
prediction.

The results of this process are stored in the `results/` directory. For the SSP 2.0 version, results are provided separately for the IIASA GDP and OECD Env-Growth models.

For the PAGE version, the `prepare-growthrates.R` script can be used to translate these files
into the form needed by Mimi PAGE.

The `project.R` script (PAGE version) or `project-ssp2.0.R` script (SSP 2.0 version) applies the estimate growth rates to baseline
population and GDP per capita levels to project these out to 2300 by
region. For the PAGE version, GDP per capita is reported in 2015 USD and population is
reported in millions. For the SSP 2.0 version, GDP per capita is reported in 2005 USD and population is reported in millions.
