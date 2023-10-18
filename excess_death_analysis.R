library(tidyverse)
library(here)
library(readxl)
library(janitor)
library(lubridate)
library(tsibble)
library(fpp3)
library(gganimate)
library(gifski)
source("get_data.R")

# Import Data -------------------------------------------------------------

death_data <- get_data_path(
  data_type = "raw",
  data_name = "District 3 Resident Deaths by Year and Month 2000-2021.xlsx"
) %>%
  read_excel(sheet = "county year month", range = "A3:N141") %>%
  select(
    "county_name" = `County of Residence and Year`,
    "year" = ...2,
    everything()
  ) %>%
  filter(year != "Total")

# Import census from 2000 to 2009
county_pop_0009 <- get_data_path(
  data_type = "raw",
  data_name = "co-est00int-01-16.xls"
) %>%
  read_excel(range = "A4:L49")

# Import census from 2010 to 2019
county_pop_1019 <- get_data_path(
  data_type = "raw",
  data_name = "co-est2019-annres-16.xlsx"
) %>%
  read_excel(range = "A4:M49")

# Import county census from 2020
county_pop_2021 <- get_data_path(
  data_type = "raw",
  data_name = "co-est2021-pop-16.xlsx"
) %>%
  read_excel(range = "A4:D49")

# Import COVID case data
covid_cases <- get_data_path(
  data_type = "processed",
  data_name = "covid_case_data.csv"
) %>%
  read_csv()

# Import COVID vaccine data
covid_vaccine <- get_data_path(
  data_type = "raw",
  data_name = "IRIS-Pared.xlsx"
) %>%
  read_excel() %>%
  clean_names()

# Import ER COVID-like illness data
cli_data <- get_data_path(
  data_type = "raw",
  data_name = "er_cli_month_2020to2021.xlsx"
) %>%
  read_excel(range = "B3:H25") %>%
  clean_names()

# Import VAERS data
vaers_data <- get_data_path(
  data_type = "processed",
  data_name = "vaers_2020to2021.csv"
) %>%
  read_csv()

# Clean Data --------------------------------------------------------------

for (county in (unique(death_data$county_name))) {
  if (is.na(county)) {
    next
  }
  index <- which(death_data$county_name == county)
  row_index <- c(0:21) + index
  death_data[row_index, 1] <- county
}

county_pop_0009 <- county_pop_0009 %>%
  select("county" = ...1, "2000" = ...2, `2001`:`2009`) %>%
  filter(county != "Idaho") %>%
  mutate("county_name" = str_to_lower(str_extract(county, "(?<=\\.)[A-Za-z]+"))) %>%
  select(-county) %>%
  select("county_name", `2000`:`2009`)

county_pop_1019 <- county_pop_1019 %>%
  select("county" = ...1, "2010" = Census, `2011`:`2019`) %>%
  filter(county != "Idaho") %>%
  mutate("county_name" = str_to_lower(str_extract(county, "(?<=\\.)[A-Za-z]+"))) %>%
  select(-county) %>%
  select(county_name, `2010`:`2019`)

county_pop_2021 <- county_pop_2021 %>%
  select("county" = ...1, "2020" = ...2, `2021`) %>%
  filter(county != "Idaho") %>%
  mutate("county_name" = str_to_lower(str_extract(county, "(?<=\\.)[A-Za-z]+"))) %>%
  select(-county) %>%
  select(county_name, everything())


# Merge all three census datasets
county_pop <- county_pop_0009 %>%
  left_join(county_pop_1019) %>%
  left_join(county_pop_2021) %>%
  filter(county_name %in% c(
    "adams",
    "canyon",
    "gem",
    "owyhee",
    "payette",
    "washington"
  )) %>%
  pivot_longer(
    cols = c(`2000`:`2021`),
    names_to = "year",
    values_to = "population"
  ) %>%
  mutate("year" = as.numeric(year))

# Initialize full dates
full_dates <- tibble("county_name" = NA, "year" = NA, "month" = NA)

for (this_county in unique(county_pop$county_name)) {
  for (this_year in 2000:2021) {
    for (this_month in 1:12) {
      print(paste("Adding", this_county, this_year, this_month))
      missing_row <- tibble(
        "county_name" = this_county,
        "year" = this_year,
        "month" = this_month
      )
      full_dates <- bind_rows(full_dates, missing_row)
    }
  }
}

full_dates <- drop_na(full_dates)

population_data <- county_pop %>%
  full_join(full_dates, by = c("county_name", "year"), multiple = "all") %>%
  group_by(year, month) %>%
  summarize("total_population" = sum(population)) %>%
  mutate("day" = 1) %>%
  unite("date", year, month, day, sep = "-") %>%
  mutate(date = ymd(date))


clean_data <- death_data %>%
  pivot_longer(`Jan`:`Dec`, names_to = "month", values_to = "num_deaths") %>%
  mutate("day" = 1) %>%
  unite("date", year, month, day, sep = "-") %>%
  mutate(date = ymd(date)) %>%
  group_by(date) %>%
  summarize("total_deaths" = sum(num_deaths)) %>%
  left_join(population_data, by = "date") %>%
  mutate(
    "pandemic" = case_when(
      date < ymd("2020-03-01") ~ 0,
      date >= ymd("2020-03-01") ~ 1
    ),
    "month" = month(date),
    "death_rate" = (total_deaths / total_population) * 100000
  )

# Create static version of animated plot below

clean_data %>%
  filter(date <= date("2020-02-01")) %>%
  ggplot(aes(x = date, y = death_rate)) +
  geom_line(color = "#00447c", linewidth = 1) +
  ylim(0, 150) +
  xlim(date("2000-01-01"), date("2021-12-01")) +
  theme_classic() +
  ylab("Death Rate (per 100,000 people)") +
  xlab("Date (by month)") +
  theme(text = element_text(size = 25))

ggsave('death_static.tiff', units = 'in', width = 16, height = 8, 
       dpi = 800)

# Create animated plot of death rate through 2020 Feb.

# death_animated <- clean_data %>%
#   filter(date <= date("2020-02-01")) %>%
#   ggplot(aes(x = date, y = death_rate)) +
#   geom_line(color = "#00447c", linewidth = 1) +
#   ylim(0, 150) +
#   xlim(date("2000-01-01"), date("2021-12-01")) +
#   theme_classic() +
#   ylab("Death Rate (per 100,000 people)") +
#   xlab("Date (by month)") +
#   transition_reveal(date) +
#   theme(text = element_text(size = 25))
# 
# 
# animate(death_animated,
#   duration = 13, fps = 20,
#   renderer = gifski_renderer(loop = FALSE),
#   units = "in",
#   width = 16, height = 8, res = 150
# )
# anim_save("death_time.gif")

# Clean cli data
cli_clean <- cli_data %>%
  select("date" = x1, everything()) %>%
  mutate(
    "year_month" = yearmonth(date),
    "total_cli" = id_adams + id_canyon + id_gem + id_owyhee + id_payette +
      id_washington
  ) %>%
  select(year_month, total_cli)


# Clean vaers data
vaers_clean <- vaers_data %>%
  mutate("date" = mdy(onset_date)) %>%
  group_by(date = floor_date(date, unit = "month")) %>%
  summarize("total_aes" = n()) %>%
  filter(!is.na(date) & date >= "2020-12-01") %>%
  mutate("year_month" = yearmonth(date)) %>%
  as_tsibble(index = year_month) %>%
  select(year_month, total_aes)

# Clean suicide data
suicide_clean <- suicide_data %>%
  select("year" = ...2, "suicide" = Suicide) %>%
  filter(year != "Total") %>%
  group_by(year) %>%
  summarize("total_suis" = sum(suicide)) %>%
  mutate(year = as.numeric(year))
# Model Data --------------------------------------------------------------

pre_pandemic_data <- clean_data %>%
  filter(date <= "2020-02-01") %>%
  mutate(
    month = yearmonth(date)
  ) %>%
  as_tsibble(index = month) %>%
  select(total_deaths, month)

post_pandemic_dates <- clean_data %>%
  filter(date > "2020-02-01") %>%
  mutate(
    month = yearmonth(date)
  ) %>%
  as_tsibble(index = month) %>%
  select(month)

linear_model <- pre_pandemic_data %>%
  model(TSLM(total_deaths ~ trend() + season()))

fc_death <- forecast(linear_model, new_data = post_pandemic_dates)

cis <- fc_death %>%
  hilo(level = 95) %>%
  mutate(
    "lower" = `95%`$lower,
    "upper" = `95%`$upper
  ) %>%
  select("mean" = .mean, lower, upper, month)

post_pandemic_data <- clean_data %>%
  filter(date > "2020-02-01") %>%
  mutate(
    month = yearmonth(date)
  ) %>%
  as_tsibble(index = month) %>%
  select(month, total_deaths)


negpos <- function(observed, predicted, upr, lwr, date) {
  obs.diff <- observed - predicted

  lwr.diff <- observed - upr

  upr.diff <- observed - lwr

  data.frame(
    excess.lwr = sum(lwr.diff, na.rm = TRUE),
    excess = sum(obs.diff, na.rm = TRUE),
    excess.upr = sum(upr.diff, na.rm = TRUE)
  )
}

excess_calc <- negpos(
  post_pandemic_data$total_deaths,
  cis$mean,
  cis$upper,
  cis$lower,
  post_pandemic_data$month
)

fc_death %>%
  autoplot(pre_pandemic_data) +
  autolayer(post_pandemic_data)

# Do same thing for death rate

negpos_rate <- function(observed, predicted, upr, lwr, date) {
  obs.diff <- observed - predicted

  lwr.diff <- observed - upr

  upr.diff <- observed - lwr

  data.frame(
    excess.lwr = sum(lwr.diff, na.rm = TRUE) / length(date),
    excess = sum(obs.diff, na.rm = TRUE) / length(date),
    excess.upr = sum(upr.diff, na.rm = TRUE) / length(date)
  )
}

pre_pandemic_data <- clean_data %>%
  filter(date <= "2020-02-01") %>%
  mutate(
    month = yearmonth(date)
  ) %>%
  as_tsibble(index = month) %>%
  select(death_rate, month)

post_pandemic_dates <- clean_data %>%
  filter(date > "2020-02-01") %>%
  mutate(
    month = yearmonth(date)
  ) %>%
  as_tsibble(index = month) %>%
  select(month)

linear_model <- pre_pandemic_data %>%
  model(TSLM(death_rate ~ trend() + season()))

fc_death <- forecast(linear_model, new_data = post_pandemic_dates)

linear_model %>%
  gg_tsresiduals()

cis <- fc_death %>%
  hilo(level = 95) %>%
  mutate(
    "lower" = `95%`$lower,
    "upper" = `95%`$upper
  ) %>%
  select("mean" = .mean, lower, upper, month)


# Build on previous animation by adding forecast

cis <- cis %>%
  select("death_rate" = mean, everything())

# death_animated2 <- clean_data %>%
#   filter(date <= date("2020-02-01")) %>%
#   ggplot(aes(x = date, y = death_rate)) +
#   geom_line(color = "#00447c", linewidth = 1) +
#   geom_line(
#     data = cis,
#     aes(x = date(month), y = death_rate),
#     color = "#fdd404",
#     linewidth = 1
#   ) +
#   geom_ribbon(
#     data = cis, aes(x = date(month), ymin = lower, ymax = upper),
#     fill = "#fdd404", alpha = 0.2
#   ) +
#   ylim(0, 150) +
#   xlim(date("2000-01-01"), date("2021-12-01")) +
#   theme_classic() +
#   ylab("Death Rate (per 100,000 people)") +
#   xlab("Date (by month)") +
#   theme(text = element_text(size = 25)) +
#   transition_reveal(date(month),
#     range = c(date("2020-03-01"), date("2021-12-01"))
#   )
# 
# animate(death_animated2,
#   duration = 5, fps = 20,
#   renderer = gifski_renderer(loop = FALSE),
#   units = "in",
#   width = 16, height = 8, res = 150
# )
# 
# anim_save("death_forecast.gif")

post_pandemic_data <- clean_data %>%
  filter(date > "2020-02-01") %>%
  mutate(
    month = yearmonth(date)
  ) %>%
  as_tsibble(index = month) %>%
  select(month, death_rate)

excess_rate_calc <- negpos_rate(
  post_pandemic_data$death_rate,
  cis$death_rate,
  cis$upper,
  cis$lower,
  post_pandemic_data$month
)

fc_death %>%
  autoplot(pre_pandemic_data) +
  autolayer(post_pandemic_data)

# Create animation showing real death rate starting in March

# post_pan_real <- clean_data %>%
#   filter(date >= date("2020-03-01")) %>%
#   select("date2" = date, "death_rate2" = death_rate, everything())
# 
# death_animated3 <- clean_data %>%
#   filter(date < date("2020-03-01")) %>%
#   ggplot(aes(x = date, y = death_rate)) +
#   geom_line(color = "#00447c", linewidth = 1) +
#   geom_line(
#     data = cis, aes(x = date(month), y = death_rate), color = "#fdd404",
#     linewidth = 1
#   ) +
#   geom_ribbon(
#     data = cis, aes(x = date(month), ymin = lower, ymax = upper),
#     fill = "#fdd404", alpha = 0.2
#   ) +
#   geom_line(
#     data = post_pan_real, aes(x = date2, y = death_rate2),
#     color = "#00447c",
#     linewidth = 1
#   ) +
#   ylim(0, 150) +
#   xlim(date("2000-01-01"), date("2021-12-01")) +
#   theme_classic() +
#   ylab("Death Rate (per 100,000 people)") +
#   xlab("Date (by month)") +
#   theme(text = element_text(size = 25)) +
#   transition_reveal(date2)
# 
# animate(death_animated3,
#   duration = 5, fps = 20,
#   renderer = gifski_renderer(loop = FALSE),
#   units = "in",
#   width = 16, height = 8, res = 150
# )
# 
# anim_save("actual_forecast.gif")

# Time-series correlations ------------------------------------------------
# Get data in right format
corr_data <- covid_cases %>%
  mutate(
    "month" = month(mdy_hms(case_date)),
    "year" = year(mdy_hms(case_date))
  ) %>%
  group_by(year, month) %>%
  summarize(
    "cases" = n(),
    "deaths" = sum(died)
  ) %>%
  mutate("day" = 1) %>%
  unite("date", year, month, day, sep = "-") %>%
  mutate(date = ymd(date))

vaccine_data <- covid_vaccine %>%
  filter(str_detect(trade_name, "Moderna|Pfizer")) %>%
  mutate(
    vaccine_brand = str_extract(trade_name, "Moderna|Pfizer"),
    "date" = ymd(vaccination_date)
  ) %>%
  select(date) %>%
  group_by(date = floor_date(date, unit = "month")) %>%
  summarize(num_vaccines = n()) %>%
  filter(date >= "2020-12-01")

corr_data <- corr_data %>%
  full_join(vaccine_data) %>%
  filter(date <= "2021-12-01" & date > "2020-02-01")

corr_data$num_vaccines[is.na(corr_data$num_vaccines)] <- 0

analysis_data <- clean_data %>%
  full_join(corr_data) %>%
  mutate(
    year_month = yearmonth(date)
  ) %>%
  as_tsibble(index = year_month) %>%
  select(death_rate, cases, num_vaccines, pandemic, year_month, month, total_deaths) %>%
  full_join(cli_clean) %>%
  full_join(vaers_clean) %>% 
  mutate('future_death_rate' = lead(death_rate, n = 5))

analysis_data[is.na(analysis_data)] <- 0

# examine assumptions

analysis_data %>%
  ACF(death_rate) %>%
  autoplot() # Significant autocorrelation

analysis_data %>%
  ACF(difference(death_rate)) %>%
  autoplot() # Difference eliminates it

analysis_data %>%
  ACF(cases) %>%
  autoplot()

analysis_data %>%
  ACF(difference(cases)) %>%
  autoplot()

analysis_data <- analysis_data %>%
  mutate("difference_death" = difference(death_rate), 
         'difference_future_death' = difference(future_death_rate)) 

plot_data <- analysis_data %>% 
  filter(year_month >= yearmonth('2020-03-01'))

future_analysis_data <- analysis_data %>% 
  filter(year_month <= max(year_month) - 4)

# Analyze the data
## Model 1: number of cases and number of mrna vaccines
cases_vaccines <- lm(
  difference_death ~ year_month +
    factor(month) +
    cases +
    num_vaccines,
  data = analysis_data
)

summary(cases_vaccines)

cases_vaccines_future <- lm(
  difference_future_death ~ year_month + 
    factor(month) + 
    num_vaccines, 
  data = future_analysis_data
)

summary(cases_vaccines_future)

## Model 2: CLI and number of mrna vaccines
cli_vaccines <- lm(
  difference_death ~ year_month +
    factor(month) +
    total_cli +
    num_vaccines,
  data = analysis_data
)

summary(cli_vaccines)

## Model 3: number of cases and total AEs
cases_vaers <- lm(
  difference_death ~ year_month +
    factor(month) +
    cases +
    total_aes,
  data = analysis_data
)

summary(cases_vaers)

## Model 4: CLI and total AEs
cli_vaers <- lm(
  difference_death ~ year_month +
    factor(month) +
    total_cli +
    total_aes,
  data = analysis_data
)

summary(cli_vaers)

### Create scatter plots of only post march 2020 relationships
ggplot(plot_data, aes(x = cases, y = difference_death)) +
  geom_point(color = "#00447c", size = 3) +
  theme_classic() + 
  theme(text = element_text(size = 25)) + 
  ylab('Death Rate (per 100,000; differenced)') +
  xlab('COVID-19 Cases') + 
  geom_smooth(method = 'lm', se = FALSE, color = "#00447c")

# ggsave('cases_death_scatter.tiff', units = 'in', width = 10, height = 10,
#        dpi = 800)

ggplot(plot_data, aes(x = cases, y = death_rate)) +
  geom_point(color = "#00447c", size = 3) +
  theme_classic() + 
  theme(text = element_text(size = 25)) + 
  ylab('Death Rate (per 100,000)') +
  xlab('COVID-19 Cases') + 
  geom_smooth(method = 'lm', se = FALSE, color = "#00447c")

ggsave('cases_deathrate_scatter.tiff', units = 'in', width = 10, height = 10, 
       dpi = 800)

ggplot(plot_data, aes(x = num_vaccines, y = difference_death)) +
  geom_point(color = "#00447c", size = 3) +
  theme_classic() + 
  theme(text = element_text(size = 25)) + 
  ylab('Death Rate (per 100,000; differenced)') +
  xlab('mRNA Vaccines Administered') + 
  geom_smooth(method = 'lm', se = FALSE, color = "#00447c")

# ggsave('vaccines_death_scatter.tiff', units = 'in', width = 10, height = 10,
#        dpi = 800)

vaccine_available_data <- plot_data %>% 
  filter(num_vaccines != 0)

ggplot(vaccine_available_data, aes(x = num_vaccines, y = death_rate)) +
  geom_point(color = "#00447c", size = 3) +
  theme_classic() + 
  theme(text = element_text(size = 25)) + 
  ylab('Death Rate (per 100,000)') +
  xlab('mRNA Vaccines Administered') + 
  geom_smooth(method = 'lm', se = FALSE, color = "#00447c")

ggsave('vaccines_deathrate_scatter.tiff', units = 'in', width = 10, height = 10, 
       dpi = 800)

ggplot(plot_data, aes(x = total_cli, y = difference_death)) +
  geom_point(color = "#00447c", size = 3) +
  theme_classic() + 
  theme(text = element_text(size = 25)) + 
  ylab('Death Rate (per 100,000; differenced)') +
  xlab('Total COVID-like Illness ER Visits')

# ggsave('cli_death_scatter.tiff', units = 'in', width = 10, height = 10, 
#        dpi = 800)

ggplot(plot_data, aes(x = total_aes, y = difference_death)) +
  geom_point(color = "#00447c", size = 3) +
  theme_classic() + 
  theme(text = element_text(size = 25)) + 
  ylab('Death Rate (per 100,000; differenced)') +
  xlab('Total Vaccine Adverse Events Reported') + 
  geom_smooth(method = 'lm', se = FALSE, color = "#00447c")

# ggsave('aes_death_scatter.tiff', units = 'in', width = 10, height = 10,
#        dpi = 800)

ggplot(vaccine_available_data, aes(x = total_aes, y = death_rate)) +
  geom_point(color = "#00447c", size = 3) +
  theme_classic() + 
  theme(text = element_text(size = 25)) + 
  ylab('Death Rate (per 100,000)') +
  xlab('Total Vaccine Adverse Events Reported') + 
  geom_smooth(method = 'lm', se = FALSE, color = "#00447c")

ggsave('aes_deathrate_scatter.tiff', units = 'in', width = 10, height = 10, 
       dpi = 800)

# Future analysis ---------------------------------------------------------
time_lead <- 1

future_analysis_data <- clean_data %>%
  full_join(corr_data) %>%
  mutate(
    year_month = yearmonth(date)
  ) %>%
  as_tsibble(index = year_month) %>%
  select(death_rate, cases, num_vaccines, pandemic, year_month, month) %>%
  full_join(cli_clean) %>%
  full_join(vaers_clean) %>% 
  mutate('future_death_rate' = lead(death_rate, n = time_lead), 
         'difference_future_death' = difference(future_death_rate)) %>% 
  filter(year_month <= max(year_month) - time_lead & year_month > min(year_month) & num_vaccines > 0)

future_analysis_data[is.na(future_analysis_data)] <- 0

vaccines_future <- lm(
  difference_future_death ~ year_month + 
    num_vaccines, 
  data = future_analysis_data
)

summary(vaccines_future)
