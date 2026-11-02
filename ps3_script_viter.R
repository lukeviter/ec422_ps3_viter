# 2.2
library(tidyverse)
retail_raw <- read_csv("data/retail_sales_raw.csv")
head(retail_raw)

# 2.2a, it did guess right :)

# 2.2b
retail_raw <- read_csv(
  "data/retail_sales_raw.csv",
  na = ".",
  col_types = cols(
    observation_date = col_date(format = "%Y-%m-%d"),
    RSAFS = col_double(),
    RSAFSNA = col_double()
  )
)

# 2.3
retail_raw_long <- retail_raw |>
  select(observation_date, RSAFS, RSAFSNA) |>
  pivot_longer(cols = c(RSAFS, RSAFSNA), names_to = "series_id", values_to = "sales")

head(retail_raw_long)

# Series_id in its own column!

# 2.4
library(fpp3)

retail_ts <- retail_raw_long |>
  mutate(Month = yearmonth(observation_date)) |>
  as_tsibble(index = Month, key = series_id)

head(retail_ts)

has_gaps(retail_ts) # No gaps!

# 2.5
head(canadian_gas)

key_vars(canadian_gas)
index_var(canadian_gas)

can_gas <- canadian_gas |>
  select(Month, Volume)

has_gaps(can_gas)

head(can_gas)

# 2.6
can_gas_recent <- can_gas |>
  filter(Month >= yearmonth("1990 Jan"))

head(can_gas_recent)

can_gas_recent <- can_gas_recent |>
  mutate(year = year(Month),
         month = month(Month, label = TRUE),
         quarter = quarter(Month))

can_gas_recent |>
  as_tibble() |>
  group_by(month) |>
  summarize(avg_volume = mean(Volume))

# December has the highest production, June has the lowest

can_gas_recent |>
  mutate(next_month           = Month + 1,
         same_month_last_year = Month - 12) |>
  head()

can_gas_q <- can_gas |>
  filter(Month <+ yearmonth("2004 Dec")) |>
  index_by(Quarter = yearquarter(Month)) |>
  summarize(Volume = sum(Volume))

head(can_gas_q)

can_gas_q |> autoplot(Volume)

# We should sum the monthly gas production values because we are looking at total production per quarter, and changing the frequency to match. Based on the autoplot, there are strong seasonal patterns in the quarterly graph as well.

# 2.7
retail_ts |> autoplot(sales)

retail_ts |>
  autoplot(sales) +
  labs(
    title = "U.S. Retail and Food Services Sales",
    subtitle = "Seasonally adjusted vs. not seasonally adjusted",
    x = NULL, y = "Millions of dollars", color = NULL
  ) +
  theme_minimal()

# The seasonal adjustment dramatically reduces the spikes present in the line and creates a smoother slope. In the NSA series, there is a sharp spike followed by a drop around Decemnber-January each year, but the SA series does not present with such dramatic swings. However, the SA series still captures economy-wide swings, such as recessions, and these are represented by sharp drops in the line. 

can_gas |>
  autoplot(Volume) +
  labs(
    title = "Canadian Gas Production",
    x = NULL, y = "Millions of cubic meters"
  ) +
  theme_minimal() +
  geom_smooth()

# I added geom_smooth() to the plot of can_gas because I wanted to more clearly see the overarchings trend of the data. This allowed me to see through the seasonal patterns a bit more clearly.

# 3.1

# 3.1a
# There is definitely an increase in the size of the swings in can_gas, although it looks like the size grows and then shrinks again. They appear to be largest in the 1980s.

can_gas <- can_gas |>
  mutate(log_volume = log(Volume))

can_gas |>
  autoplot(log_volume)

# The plot now slopes upward much more quickly before leveling out in the 80s and then sloping back upward. The size of the swings is, additionally, much more consistent than in the plot of just can_gas. Any growth/shrinkage is the size of the swings is less pronounced.

# I do think that logs are a good idea for can_gas because they help to isolate the underlying trends of the data without the influence of changes in level. 

retail_nsa <- retail_ts |>
  filter(series_id == "RSAFSNA") |>
  mutate(log_sales = log(sales))

retail_nsa |>
  autoplot(log_sales)

retail_nsa |>
  autoplot(sales)

# The case for logging seems to be a little weaker here, as the plot of the log of sales reduces the size of the swings, but only slightly. It does not appear to be immensely helpful.

# 3.2

can_gas <- can_gas |>
  mutate(
    growth_direct = 11 * difference(Volume) / lag(Volume),
    growth_logdiff = 100 * difference(log_volume)
  )

can_gas |>
  autoplot(growth_direct) +
  theme_minimal()

can_gas <- can_gas |>
  mutate(growth_gap = abs(growth_direct - growth_logdiff))

# The log difference approximation might be good in this case because it allows a more direct comparison of data across time, allowing us to look at percentage changes rather than changes in levels which can be affected by growth in levels.

# 3.3

retail_ts <- retail_ts |>
  group_by_key() |>
  mutate(growth_mom = 100 * difference(log(sales))) |>
  ungroup()

retail_ts |>
  autoplot(growth_mom) +
  labs(
    title = "Month-over-Month Growth: SA vs. NSA Retail Sales",
    x = NULL, y = "Growth rate (%)", color = NULL
  ) +
  theme_minimal()

# The growth rate of the SA series is significantly more consistent, and the not seasonally adjust data presents with a lot more noise. The NSA data has significantly larger spikes compared to the SA data. Like we found in part 2.7b, seasonally adjusted data smooths out the information and enables us to see underlying trends with more clarity.

retail_ts |>
  filter(series_id == "RSAFSNA") |>
  gg_season(sales) +
  labs(
    title = "Seasonal Pattern in NSA Retail Sales",
    y = "Millions of dollars"
  ) +
  theme_minimal()

retail_ts |>
  filter(series_id == "RSAFS") |>
  gg_season(sales) +
  labs(
    title = "Seasonal Pattern in SA Retail Sales",
    y = "Millions of dollars"
  ) +
  theme_minimal()

# The seasonal pattern in the NSA data is very pronounced, with each line curving upward. The NSA data on the other hand, is more consistent, with the lines being much straighter and more consistently so. There is still a pronounced drop at COVID, but this is expected.

can_gas |>
  gg_season(log_volume) +
  labs(
    title = "Seasonal Pattern in Canada Gas Production",
    y = "Billions of cubic meters"
  ) +
  theme_minimal()

# Months such as January, November, and December seem to have the highest production, while the summer months have lower production—this makes sense given that gas is probably used very frequently in the winter months to heat houses.

can_gas |>
  gg_subseries(log_volume) +
  labs(
    title = "Subseries Plot: Canadian Gas Production",
    y = "log(Volume)"
  )

# 4.3

retail_nsa_x13 <- retail_ts |>
  filter(series_id == "RSAFSNA") |>
  model(x13 = X_13ARIMA_SEATS(sales)) |>
  components()

retail_nsa_x13 |> autoplot()

# The seasonal factor appears to be the highest in December, where sales are rougly 20% above trend. However, the size of the seasonal swing does appear to shrink marginally as time progresses, though not to any extreme degree.

sa_compare <- retail_nsa_x13 |>
  as_tibble() |>
  select(Month, mine = season_adjust) |>
  left_join(
    retail_ts |>
      filter(series_id == "RSAFS") |>
      as_tibble() |>
      transmute(Month, census = sales),
    by = "Month"
  ) |>
  mutate(pct_diff = 100 * (log(mine) - log(census)))

sa_compare |>
  ggplot(aes(x = Month, y = pct_diff)) +
  geom_line() +
  labs(
    title = "My X-13 Seasonal Adjustment Minus the Census Bureau's",
    x = NULL, y = "Difference (%)"
  ) +
  theme_minimal()

# The general trend of the difference is relatively even and appears to be about centered at zero, but there are large swings present where the difference is significantly greater. These spikes appear especially great following January 2020 and persist for a few cycles, likely a result of the COVID-19 pandemic.

# 4.4

can_gas_fit <- can_gas |>
  model(TSLM(log_volume ~ trend() + season()))

report(can_gas_fit)

can_gas_fit |>
  tidy() |>
  select(term, estimate, std.error, statistic, p.value) |>
  knitr::kable(digits = 3)

# The estimated average monthly growth rate of gas production is 3.973e-03, and the month of June has the most significant seasonal effect in terms of largest t-stat.

can_gas_fit |>
  augment() |>
  ggplot(aes(x = Month)) +
  geom_line(aes(y = log_volume, color = "Actual")) +
  geom_line(aes(y = .fitted, color = "Fitted")) +
  labs(title = "Linear Trend + Seasonal Dummies Fit: Canadian Gas Production",
       y = "log(Volume)", color = NULL) +
  theme_minimal()

# The fitted values follow the actual values decently well from about 1990 onward until roughly 2020, but in the period before this the fitted model underestimates the actual values by a decent margin.

can_gas_fit |> gg_tsresiduals()

can_gas_fit |>
  augment() |>
  features(.innov, ljung_box, lag = 24)

# The residuals do not appear like white noise, as there is a general downward trend that likely contains more forecastable information. Moreover, the p-value of the Ljung-Box test is 0, meaning that at the 5% level we reject the null hypothesis of white noise.

# 5.1

gas_cv_fc <- can_gas |>
  select(Month, Volume) |>
  stretch_tsibble(.init = 240, .step = 1) |>
  model(snaive = SNAIVE(Volume),
        tslm   = TSLM(log(Volume) ~ trend() + season())) |>
  forecast(h = 1)

gas_cv_fc |>
  accuracy(can_gas, measures = list(MAE = MAE, RMSE = RMSE))








