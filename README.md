# SSP-Extensions
A Bayesian model for extending the SSP socioeconomics.

This repository provides code and documentation for generating CSVs
that describe the extensions to the SSP socioeconomics past 2100. This
work is currently used by MimiPAGE2020
(https://github.com/openmodels/MimiPAGE2020.jl) and META-2021
(https://github.com/openmodels/META-2021).

The process is described in `docs/main.pdf`.

The process starts with SSP projected values from 2015 to 2100,
according to the regional definitions used by PAGE. These are
extracted from PAGE-ICE and stored in `data/`.

The script `src/bayesfit.R` applies a Bayesian model to fit the
convergent growth equations.  It does this in such a way that it (1)
recalculates the mean growth rate (for convergence) every year, (2)
fits the convergence and decay rates under that yearly process, and
(3) keeps track of the full range of Bayesian uncertainty during
prediction.

The results of this process are stored in the `results/` directory.

Finally, the `prepare-growthrates.R` script translates these files
into the form needed by Mimi PAGE.
