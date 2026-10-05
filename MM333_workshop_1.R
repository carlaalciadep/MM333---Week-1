# set working directory:
setwd("/Users/carlaalcaidepuchades/Desktop/uni - reading/year 4/MM333 - Advanced Data Analytics/week1")

# import library 
library(readr)

# import data set - change actual name to df to work easier
df <- read_csv("MM333_customer_marketing_week1.csv")
View(MM333_customer_marketing_week1)

# import packages
install.packages(c("tidyverse", "janitor", "psych", "effectsize", "broom"))

# load packages
library(tidyverse)
library(janitor)
library(psych)
library(effectsize)
library(broom)

# Place csv in the working directory
customers <- read_csv("MM333_customer_marketing_week1.csv", show_col_types = FALSE) |>
  clean_names()

# To analyse the dataset, this gives us 
customers |> glimpse()
customers |> names()
customers |> summary()
customers |> summarise(
  rows = n(),
  missing_values = sum(is.na(across(everything())))
)

# Check variable classes
customers |> summarise(across(everything(), class))

# Check duplicate customer IDs
customers |>
  count(customer_id) |>
  filter(n > 1)

# Calculate group-wise descriptive statistics
customers |>
  group_by(campaign) |>
  summarise(
    n = n(),
    mean_spend = mean(monthly_spend_gbp, na.rm = TRUE),
    sd_spend = sd(monthly_spend_gbp, na.rm = TRUE),
    median_spend = median(monthly_spend_gbp, na.rm = TRUE),
    min_spend = min(monthly_spend_gbp, na.rm = TRUE),
    max_spend = max(monthly_spend_gbp, na.rm = TRUE)
  )

# Alternative code for group-wise descriptive statistics
customers |>
  dplyr::group_by(campaign) |>
  dplyr::group_modify(
    ~ psych::describe(.x$monthly_spend_gbp) |>
      tibble::as_tibble()
  )

# To visualize outcome
ggplot(customers, aes(x = campaign, y = monthly_spend_gbp, fill = campaign)) +
  geom_boxplot(alpha = 0.7, show.legend = FALSE) +
  geom_jitter(width = 0.1, alpha = 0.25, show.legend = FALSE) +
  labs(
    x = "Campaign exposure",
    y = "Monthly spending (£)",
    title = "Monthly spending by campaign exposure"
  ) +
  theme_minimal()

# CHECKING ASSUMPTIONS
# Inspect distributions
ggplot(customers, aes(x = monthly_spend_gbp)) +
  geom_histogram(bins = 20, colour = "white") +
  facet_wrap(~ campaign) +
  theme_minimal()

# Use Q-Q Plots
ggplot(customers, aes(sample = monthly_spend_gbp)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ campaign) +
  theme_minimal()

# Formal variance, just to provide supporting evidence
customers |>
  summarise(
    variance_campaign = var(monthly_spend_gbp[campaign == "Campaign"], na.rm = TRUE),
    variance_control = var(monthly_spend_gbp[campaign == "Control"], na.rm = TRUE)
  )
# Outcome - first column = campaign, second column = control. Threshold ratio of 2.

# Comparing the two campaign groups
# Welch's independent sample t-test 
t_test_result <- t.test(
  monthly_spend_gbp ~ campaign,
  data = customers,
  var.equal = FALSE,
  alternative = "two.sided"
)
t_test_result

# Calculate a standardised effect size
effectsize::cohens_d(
  monthly_spend_gbp ~ campaign,
  data = customers,
  pooled_sd = FALSE
)

# Tidy up the result
broom::tidy(t_test_result)

# Mann-Whitney U test
# Use if assumptions are seriously compromised
wilcox.test(
  monthly_spend_gbp ~ campaign,
  data = customers,
  exact = FALSE,
  alternative = "two.sided"
)

# Fit a multiple linear regression model predicting monthly spending
model_1 <- lm(
  monthly_spend_gbp ~ campaign + website_visits + loyalty_status + age,
  data = customers
)
summary(model_1)

# Create a tidy coefficient table
broom::tidy(model_1, conf.int = TRUE)

# Predicative analysis with Linear Regression
# Regression diagnostics
par(mfrow = c(2, 2))
plot(model_1)
par(mfrow = c(1, 1))
# If plot window is not big enough, error message will show up

# Fitting the regression model with an addition: interaction
model_2 <- lm(
  monthly_spend_gbp ~ campaign * loyalty_status + website_visits + age,
  data = customers
)
summary(model_2)

