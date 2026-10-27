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





