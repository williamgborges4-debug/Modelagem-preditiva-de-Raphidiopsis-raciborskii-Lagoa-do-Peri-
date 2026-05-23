Predictive modeling of Raphidiopsis raciborskii
Lagoa do Peri, Florianópolis/SC
Author: William Gabriel Borges
E-mail: williamgborges4@gmail.com 
Introduction
This repository provides the data and R scripts needed to reproduce all analyses and results for the study on the population dynamics of Raphidiopsis raciborskii in Lagoa do Peri, Florianópolis, SC, Brazil. The dataset consists of a monthly time series from 2009 to 2026. All analyses were conducted in R 4.3.1 (R Core Team, 2021).
Downloading the Files
All files required to reproduce the analyses are available in this repository. To get started, download all scripts (.R files) and data files (.xlsx) listed below. Before running any script, open it and fill in the pasta_dados and pasta_results variables at the top with the paths to your local folders.
Description of Code Files
1a.imputacao_dados.R – Imputation of missing values in limnological variables using linear interpolation (isolated gaps with available neighbors) and seasonal mean (consecutive blocks or series extremes). Reads D0.xlsx and exports L1.xlsx.

1b.analise_exploratoria.R – Exploratory data analysis of the full time series. Covers the distribution of the response variable, temporal trends, seasonality, STL decomposition, ACF/PACF for autocorrelation diagnosis, and correlation analyses between predictors and the response across two time periods (2009–2017 vs 2018–2026). Exports figures to Results/.
1c.modelagem_aula.R – Simplified modelling script developed as a partial course deliverable. Fits a Gaussian GAM with three predictors (tempo_cont, zon_afot, alca) selected from preliminary EDA. 
How to Use
Run the scripts in the following order:

1.	1a.imputacao_dados.R 
2.	1b.analise_exploratoria.R 
3.	1c.modelagem_aula.R 
Data Files
–	D0.xlsx – Raw data, monthly time series 2009–2026.
–	L1.xlsx – Cleaned data after imputation, used as input for exploratory data analysis and modelling.


Reference
R Core Team. (2021). R: A language and environment for statistical computing [Software]. R Foundation for Statistical Computing. https://www.R-project.org/
