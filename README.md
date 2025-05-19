
# Assumption-lean weak limits and tests for two-stage adaptive experiments

This repository reproduces the results reported in the following paper:

Z. Niu and Z. Ren. “Assumption-lean weak limits and tests for two-stage
adaptive experiments.” ([arXiv](https://arxiv.org/pdf/2505.10747))

# Get started

First, clone the `AdaInf-manuscript` repository onto your machine.

    git clone git@github.com:ZiangNiu6/AdaInf-manuscript.git

Then enter the `AdaInf-project.Rproj` and activate the `renv` package by
the following code.

    renv::activate(); renv::restore()

# Create the figures for simulation results

## Type-I error and power simulation results

Simulation results generating Figure 3 and 4 in the main text and Figure
7 and 8 in the appendix can be found in
`simulation-code/main-simulation/Bernoulli` and
`simulation-code/main-simulation/gaussian`. These figures can be
reproduced by running the following code.

    Rscript simulation-code/main-simulation/plot.R

## Power comparison results

Simulation results generating Figure 9 in the appendix can be found in
`simulation-code/power-comparison`. The figure can be reproduced by
running the following code.

    Rscript simulation-code/power-simulation/plot.R

## Auxiliary figures

Figure 1 and 2 in the main text can be reproduced by running the
following code.

    Rscript simulation-code/sampling-asymptotic-distributions/sampling_distribution.R
    Rscript simulation-code/sampling-asymptotic-distributions/asymptotic_distribution.R

Figure 6 in the appendix can be reproduced by running the following
code.

    Rscript simulation-code/failure-normal/failure_normal.R

# Create the figures for real data analysis

Simulation results generating Figure 5 in the main text and Figure 10 in
the appendix can be found in `realdata-code/results`. The figure can be
reproduced by running the following code.

    Rscript realdata-code/plot.R

All the produced figures can be found in
`manuscript/figures-and-tables`.

If you would like to rerun the simulations from scratch, follow the
steps in the next section.

# Reproduce the results for simulation and real data analysis

## Run simulation

For the commands below, you may execute it in your cluster.

    qsub run_main_simulatioin.sh
    qsub run_power_comparison_simulation.sh

## Run real data analysis

One can use the following command to reproduce the real data analysis
results.

    qsub run_real_data.sh

## Create figures

After the simulation and real data analysis finish, you can use the code
in the above section to reproduce the figures.
