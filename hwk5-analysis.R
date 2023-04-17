## Title: ECON 470 HW4
## Author: Ben Yang
## Date Created: 3/22/2023
## Date Edited: 4/16/2023
## Descriptions: This file renders/runs all relevant R code for the assignment

## Preliminaries ---------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, ggpubr, cobalt, dplyr, broom, cobalt, MatchIt,
               knitr, data.table, kableExtra, tinytex, scales,  
               lubridate, stringr, gdata,
               readxl, 
               rdrobust, rddensity, estimatr,
               modelsummary, fixest, AER)

## Read data and set workspace for knitr ---------------------------------------

final.data <- read_tsv("data/output/acs_medicaid.txt")

## Question 1 Share of insured individuals with direct purchase ----------------

q1.data <- final.data %>%
  mutate(insured = ins_employer + ins_direct + ins_medicare + ins_medicaid) %>%
  group_by(year) %>%
  summarise(total_ins = sum(insured), 
            total_ins_direct = sum(ins_direct),
            share = total_ins_direct / total_ins)

q1.plot <- q1.data %>%
  ggplot(aes(x = year, y = share)) +
  geom_line(size = 1, color = "dodgerblue4") + geom_point(color = "dodgerblue4") +
  scale_x_continuous(breaks = c(2012:2019)) +
  geom_vline(xintercept = 2013.5, size = 1, color = "firebrick", linetype = "longdash") + 
  geom_text(label = round(q1.data$share,3), size = 3, nudge_x = 0, nudge_y = 0.002, check_overlap = TRUE) +
  labs(x = "Year", y = "Share of Direct Purchase", Title = "Share of Insured Individuals with Direct Purchase Health Insurance from 2012 to 2019") +
  theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  theme(
    plot.title = element_text(size = 12, color = "black", hjust = 0.5),
    legend.title = element_text(size = 10, color = "black"),
    legend.position = "top",
    axis.title = element_text(size = 10, color = "black"),
    axis.text.x = element_text(size = 10, angle = 0, color = "black"),
    axis.text.y = element_text(size = 10, angle = 0, color = "black"))

## Question 3 Share of insured individuals with Medicaid -----------------------

q3.data <- final.data %>%
  mutate(insured = ins_employer + ins_direct + ins_medicare + ins_medicaid) %>%
  group_by(year) %>%
  summarise(total_ins = sum(insured), 
            total_ins_medicaid = sum(ins_medicaid),
            share = total_ins_medicaid / total_ins)

q3.plot <- q3.data %>%
  ggplot(aes(x = year, y = share)) +
  geom_line(size = 1, color = "dodgerblue4") + geom_point(color = "dodgerblue4") +
  scale_x_continuous(breaks = c(2012:2019)) +
  geom_vline(xintercept = 2013.5, size = 1, color = "firebrick", linetype = "longdash") + 
  geom_text(label = round(q3.data$share,3), size = 3, nudge_x = 0, nudge_y = 0.002, check_overlap = TRUE) +
  labs(x = "Year", y = "Share of Medicaid", Title = "Share of Insured Individuals with Medicaid from 2012 to 2019") +
  theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  theme(
    plot.title = element_text(size = 12, color = "black", hjust = 0.5),
    legend.title = element_text(size = 10, color = "black"),
    legend.position = "top",
    axis.title = element_text(size = 10, color = "black"),
    axis.text.x = element_text(size = 10, angle = 0, color = "black"),
    axis.text.y = element_text(size = 10, angle = 0, color = "black"))

## Question 4 Share of uninsured individuals by if expanded in 2014 ------------

q4.data <- final.data %>%
  filter(expand_year %in% c(NA, 2014), !is.na(expand_ever)) %>%
  group_by(year, expand_ever) %>%
  summarise(total_pop = sum(adult_pop), 
            total_unins = sum(uninsured),
            share = total_unins / total_pop)

q4.plot <- q4.data %>%
  ggplot(aes(x = year, y = share, color = expand_ever)) +
  geom_line(size = 1) + geom_point() +
  scale_x_continuous(breaks = c(2012:2019)) +
  scale_color_manual(name = "Expand in 2014", values = c("dodgerblue4", "dodgerblue1")) +
  geom_vline(xintercept = 2013.5, size = 1, color = "firebrick", linetype = "longdash") + 
  geom_text(label = round(q4.data$share,3), size = 3, nudge_x = 0, nudge_y = 0.01, check_overlap = TRUE, show_guide = FALSE) +
  geom_text(data = q4.data %>% filter(year == 2018), size = 5, aes(label = c("Non-expansion", "Expansion"), x = year + 0.5, y = share + 0.025), show_guide = FALSE) +
  labs(x = "Year", y = "Share of Uninsured", Title = "Share of Uninsured Individuals by If Expanded in 2014 from 2012 to 2019") +
  theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  theme(
    plot.title = element_text(size = 12, color = "black", hjust = 0.5),
    legend.title = element_text(size = 10, color = "black"),
    legend.position = "top",
    axis.title = element_text(size = 10, color = "black"),
    axis.text.x = element_text(size = 10, angle = 0, color = "black"),
    axis.text.y = element_text(size = 10, angle = 0, color = "black"))

## Question 5 Avg % of uninsured in 2012/2015, for expansion/non-expansion -----

q5.data <- final.data %>%
  filter(expand_year == 2014 | is.na(expand_year), !is.na(expand_ever), year %in% c(2012, 2015)) %>%
  mutate(uninsured_rate = uninsured / adult_pop) %>%
  group_by(year, expand_ever) %>%
  summarise(avg_uninsured_rate = mean(uninsured_rate)) %>%
  pivot_wider(names_from = year, values_from = avg_uninsured_rate) %>%
  arrange(desc(expand_ever))

## Question 6 Standard DD Regression Estimator ---------------------------------

reg.dat <- final.data %>%
  filter(expand_year == 2014 | is.na(expand_year), !is.na(expand_ever)) %>%
  mutate(perc_unins = uninsured / adult_pop,
         post = (year >= 2014),
         treat = post * expand_ever)

m.dd <- lm(formula = perc_unins ~ post + expand_ever + post * expand_ever, data = reg.dat)

## Question 7 Include Fixed Effects --------------------------------------------

m.twfe <- feols(fm = perc_unins ~ treat | State + year, data = reg.dat)

## Question 8 Include Fixed Effects, Include All States ------------------------

reg.dat.all <- final.data %>%
  filter(!is.na(expand_ever)) %>%
  mutate(perc_unins = uninsured / adult_pop,
         post = (year >= 2014),
         treat = post * expand_ever,
         time_to_treat = ifelse(expand_ever == FALSE, 0, year - expand_year),
         time_to_treat = ifelse(time_to_treat < -3, -3, time_to_treat))

m.twfe.all <- feols(fm = perc_unins ~ treat | State + year, data = reg.dat.all)

## Question 9 Event Study ------------------------------------------------------

mod.twfe <- feols(fm = perc_unins ~ i(year, expand_ever, ref = 2013) | State + year,
                  cluster = ~State,
                  data = reg.dat)

q9.data <- data.frame(mod.twfe$coeftable) %>%
  select(estimate = Estimate, std.error = Std..Error) %>%
  mutate(year = c(2012, 2014:2019)) %>%
  rbind(., "year::2013:expand_ever" = c(0, 0, 2013))

q9.plot <- q9.data %>%
  ggplot(aes(x = year, y = estimate)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "black", linetype = 2) +
  geom_errorbar(aes(x = year, ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error), width = .1, position = position_dodge(width = 0.5)) +
  labs(x = "Time to Treatment", y = "Estimate and 95 Percent Confidence Interval", Title = "Event Study for Effects of Medicaid Expansion - States that Expanded in 2014 or Never Expanded") +
  theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  theme(
    plot.title = element_text(size = 12, color = "black", hjust = 0.5),
    legend.title = element_text(size = 10, color = "black"),
    legend.position = "top",
    axis.title = element_text(size = 10, color = "black"),
    axis.text.x = element_text(size = 10, angle = 0, color = "black"),
    axis.text.y = element_text(size = 10, angle = 0, color = "black"))

## Question 10 Event Study, Include All States ---------------------------------

mod.twfe.all <- feols(fm = perc_unins ~ i(time_to_treat, expand_ever, ref = -1) | State + year,
                      cluster = ~State,
                      data = reg.dat.all)

q10.data <- data.frame(mod.twfe.all$coeftable) %>%
  select(estimate = Estimate, std.error = Std..Error) %>%
  mutate(year = c(-3:-2, 0:5)) %>%
  rbind(., "time_to_treat::-1:expand_ever" = c(0, 0, -1))

q10.plot <- q10.data %>%
  ggplot(aes(x = year, y = estimate)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "black", linetype = 2) +
  geom_errorbar(aes(x = year, ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error), width = .1, position = position_dodge(width = 0.5)) +
  labs(x = "Time to Treatment", y = "Estimate and 95 Percent Confidence Interval", Title = "Event Study for Effects of Medicaid Expansion - All States") +
  theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  theme(
    plot.title = element_text(size = 12, color = "black", hjust = 0.5),
    legend.title = element_text(size = 10, color = "black"),
    legend.position = "top",
    axis.title = element_text(size = 10, color = "black"),
    axis.text.x = element_text(size = 10, angle = 0, color = "black"),
    axis.text.y = element_text(size = 10, angle = 0, color = "black"))

## Save data for markdown ------------------------------------------------------

rm(list=c("final.data"))
save.image("Hwk5_workspace.Rdata")