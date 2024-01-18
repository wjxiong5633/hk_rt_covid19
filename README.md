# hk_rt_covid19
Codes for generating results in the paper "Nowcasting time-varying effective reproductive number of COVID-19 in Hong Kong".

## data
In the data folder,  we have provided data of daily aggregate values (including confirm cases, true $R_t$ and $R_{t}^{k}$, meterological predictors and policy indices predictors).

## generate_rt
Contains codes for generate $R_t$ and temporal $R_{t}^{k}$.

## run_model
Contains codes for running models.
* 1_Omicron - training and testing on Omicron variant data (wave 5 and wave 6).
* 2_ancestral_strain - explore the robustness, testing on ancestral strain data (wave 4).

## model_res
Contains results for running models.

## tidy_result
Contains codes and tidy results for tables and figures.

## simulation_synthetic
We conducted simulation with synthetic data based on Gostic, Katelyn M., et al. "Practical considerations for measuring the effective reproductive number, Rt." PLoS computational biology 16.12 (2020): e1008409. https://doi.org/10.1371/journal.pcbi.1008409

* 01-simulate-data - simulate synthetic data
* 02-simulate - generate Rt from report-shift, RIDE and BP method.
* 03-sim-censored - generate Rt from censored data.

## figure_script
Contains codes for generating main Figure 1-5 and Supplementary Figure s1-s9.

[Operated under R version 4.2.3 (R Development Core Team, 2021)]
