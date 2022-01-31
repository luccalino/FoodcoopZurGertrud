# Loading packages (install if not yet installed)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, informR, hrbrthemes, openxlsx, lubridate, RColorBrewer, knitr, rmarkdown, tidygeocoder, rgdal, grid, gridExtra)

# Set WD
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Projects/foodcoop_zurgertrud/")

# Reading files
members <- read_delim("members/members_export_b7206ee42e/subscribed_members_export_b7206ee42e.csv", delim = ",", locale = locale(encoding = "UTF-8"))
unsubsrciped_members <- read_delim("members/members_export_b7206ee42e/unsubscribed_members_export_b7206ee42e.csv", delim = ",", locale = locale(encoding = "UTF-8"))

# Date format
members$OPTIN_TIME <- as.POSIXct(members$OPTIN_TIME, format = "%Y/%m/%d %H:%M:%S")
members$sub_time <- as.Date(members$OPTIN_TIME)
unsubsrciped_members$UNSUB_TIME <- as.POSIXct(unsubsrciped_members$UNSUB_TIME, format = "%Y/%m/%d %H:%M:%S")
unsubsrciped_members$unsub_time <- as.Date(unsubsrciped_members$UNSUB_TIME)

# Member identification
members$member <- 0
members$member[grepl("Mitglied", members$TAGS) == TRUE & 
                 grepl("Mitgliederbeitrag offen", members$TAGS) == FALSE] <- 1
members$pending_member <- 0
members$pending_member[grepl("Mitgliederbeitrag offen", members$TAGS) == TRUE] <- 1
members$newsletter <- 0
members$newsletter[grepl("Newsletter", members$TAGS) == TRUE & 
                     grepl("Mitgliederbeitrag offen", members$TAGS) == FALSE & 
                     grepl("Mitglied", members$TAGS) == FALSE] <- 1

# Mitgliederentwicklung
sub_ts <- members %>%
  arrange(sub_time) %>%
  mutate(sub_members = cumsum(member),
         pend_members = cumsum(pending_member),
         newsletter_only = cumsum(newsletter)) %>%
  pivot_longer(cols = c(sub_members, pend_members, newsletter_only)) 

sub_ts$value_test <- 0
for (i in 1:nrow(unsubsrciped_members)) {
  sub_ts$value_test[sub_ts$sub_time >= unsubsrciped_members$unsub_time[i] & sub_ts$name == "sub_members"] <- -1
  sub_ts$value <- sub_ts$value + sub_ts$value_test
} 

ggplot() +
  geom_vline(xintercept = as.Date("2020-08-12"), linetype = "longdash", alpha = 0.4) +
  geom_vline(xintercept = as.Date("2020-08-21"), linetype = "longdash", alpha = 0.4) +
  geom_vline(xintercept = as.Date("2020-11-05"), linetype = "longdash", alpha = 0.4) +
  geom_vline(xintercept = as.Date("2020-11-15"), linetype = "longdash", alpha = 0.4) +
  geom_vline(xintercept = as.Date("2021-02-06"), linetype = "longdash", alpha = 0.4) +
  geom_vline(xintercept = as.Date("2021-02-16"), linetype = "longdash", alpha = 0.4) +
  geom_vline(xintercept = as.Date("2021-05-08"), linetype = "longdash", alpha = 0.4) +
  geom_vline(xintercept = as.Date("2021-05-18"), linetype = "longdash", alpha = 0.4) +
  geom_vline(xintercept = as.Date("2021-08-14"), linetype = "longdash", alpha = 0.4) +
  geom_vline(xintercept = as.Date("2021-08-24"), linetype = "longdash", alpha = 0.4) +
  geom_vline(xintercept = as.Date("2021-11-06"), linetype = "longdash", alpha = 0.4) +
  geom_vline(xintercept = as.Date("2021-11-16"), linetype = "longdash", alpha = 0.4) +
  geom_vline(xintercept = as.Date("2022-02-05"), linetype = "longdash", alpha = 0.4) +
  geom_vline(xintercept = as.Date("2022-02-15"), linetype = "longdash", alpha = 0.4) +
  geom_line(data = sub_ts, aes(x = sub_time, y = value, color = name)) +
  ylab("Mitgliederentwicklung") +
  scale_color_discrete(name = "Mitgliederstatus", labels = c("Newsletter","Mitgliederbeitrag offen","Mitglied")) +
  xlab("") +
  theme_bw()