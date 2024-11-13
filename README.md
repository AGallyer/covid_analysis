# COVID-19 Excess Mortality Analysis in Public Health District 3 of Idaho

This repository contains code for an analysis of the impact of COVID-19 on excess mortality in Idaho's Public Health District 3 (PHD3). The analysis uses both publicly available and local, private data. Using the death rate from 2000 Jan. to 2020 Feb., the analysis estimates excess deaths in PHD3 between 2020 Mar. and 2021 Dec. The analysis then examines two groups of correlates as competing explanations for the death rate: COVID-19 cases, and administration of COVID-19 vaccines. In short, between 2020 and 2021, PHD3 saw an estimated 1146 excess deaths. COVID-19 cases were positively associated with the death rate, while COVID-19 vaccines were negatively associated with the death rate. This suggests that COVID-19 cases, whether directly or indirectly, likely caused the excess deaths, while vaccines have a protective effect.

## Data Sources

-   **Idaho Bureau of Vital Records and Health Statistics** - This data set includes monthly mortality counts for each county in PHD3 (i.e., Adams, Canyon, Gem, Owyhee, Payette, & Washington) from 2000 Jan. to 2021 Dec. It also includes yearly death counts for all leading causes of death since 2000 to 2021.
-   **Idaho's Immunization Reminder Information System** - This data set includes each instance of COVID-19 vaccine administration within PHD3. For this analysis, only counts of mRNA vaccines were used.
-   **National Base System** - This data set includes all COVID-19 cases in PHD3 since the beginning of the pandemic.
-   **National Syndromic Surveillance Program** - This data set includes monthly counts of emergency room visits for COVID-like illness. This data set only includes reports from Canyon, Payette, and Washington counties.
-   **Vaccine Adverse Events Reporting System** - This data set includes monthly counts of adverse events associated with COVID-19 vaccines administration for the entire state of Idaho. Note that there are many precautions that should be taken when interpreting or using this data. These precautions can be found here: (<https://vaers.hhs.gov/data/dataguide.html>).

## Analysis Methods

Excess mortality is defined as the difference between observed deaths and expected deaths during a given period of time. To calculate expected deaths, a seasonal multiple regression model was built using the monthly death rate (defined as $\frac{deaths}{population} \times 100000$) from 2000 Jan. to 2020 Feb. This model was defined as:

$y_{deathrate} = \beta_{0} + \beta_{1} x_{time} + \beta_2 x_{month} + residual$

This model was used to forecast the expected deaths between 2020 Mar. and 2021 Dec., with 95% confidence intervals. Then, the observed number of deaths in that time period was subtracted from:

1.  The mean predicted number of deaths.
2.  The upper bound of the 95% confidence interval for predicted deaths.
3.  The lower bound of the 95% confidence interval for predicted deaths.

This created an average estimate of the excess deaths during this time period, along with 95% confidence intervals.

After establishing that PHD3 experienced excess deaths from 2020 Mar. to 2021 Dec., the next step was to examine correlates of the death rate. Based on the concerns of our Board, constituents, and experts at our organization, I examined two groups of correlates:

1.  COVID-19 Cases
2.  COVID-19 Vaccines

Within each group I used two variables to represent that group. For COVID-19 cases, they were: the number of COVID-19 cases that month, and the number of COVID-like illness ER visits that month. For COVID-19 vaccines, I examined: the number of COVID-19 mRNA vaccines administered that month, and the number of adverse event reports associated with a COVID-19 vaccine. By selecting one variable from each group for each model, this results in four different models:

1.  $y_{deathrate} = \beta_{0} + \beta_{1} x_{time} + \beta_2 x_{month} + \beta_3 x_{cases} + \beta_4 x_{vaccines} + residual$
2.  $y_{deathrate} = \beta_{0} + \beta_{1} x_{time} + \beta_2 x_{month} + \beta_3 x_{cases} + \beta_4 x_{AEs} + residual$
3.  $y_{deathrate} = \beta_{0} + \beta_{1} x_{time} + \beta_2 x_{month} + \beta_3 x_{CLI} + \beta_4 x_{vaccines} + residual$
4.  $y_{deathrate} = \beta_{0} + \beta_{1} x_{time} + \beta_2 x_{month} + \beta_3 x_{CLI} + \beta_4 x_{AEs} + residual$

Note that $\beta_2 x_{month}$ actually represents a set of dummy-coded variables representing the months of the year. Thus, the full equation for model 1 would be:

$y_{deathrate} = \beta_{0} + \beta_{1} x_{time} + \beta_2 x_{feb} + \beta_3 x_{mar} + \beta_4 x_{apr} + \beta_5 x_{may} + \beta_6 x_{jun} + \beta_7 x_{jul} + \\ \beta_8 x_{aug} + \beta_9 x_{sep} + \beta_{10} x_{oct} + \beta_{11} x_{nov} + \beta_{12} x_{dec} +\beta_{13} x_{cases} + \beta_{14} x_{vaccines} + residual$

## License

The project is licensed under the GPL 3.0 license - see COPYING.txt for details.

## Contact

For questions or comments about the analysis, please contact James Gallyer at [agallyer\@gmail.com](mailto:agallyer@gmail.com).
