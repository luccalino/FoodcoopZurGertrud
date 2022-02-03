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

sub_ts$value_temp <- 0
for (i in 1:nrow(unsubsrciped_members)) {
  sub_ts$value_temp[sub_ts$sub_time > unsubsrciped_members$unsub_time[i] & sub_ts$name == "sub_members"] <- -1
  sub_ts$value <- sub_ts$value + sub_ts$value_temp
} 

sub_ts <- subset(sub_ts, name != "newsletter_only")

dates <- c("2020-08-21","2020-11-15","2021-02-16","2021-05-18","2021-08-24","2021-11-16")
bgs <- c(17,33,36,53,37,51)
values <- c(9930.31,12123.66,14562.80,19577.56,12487.91,16738.45)
labels1 <- c("","","","","","Bestellwert (r)")
labels2 <- c("","","","","","Bestellgruppen (l)")
volumes <- data.frame(date = as.Date(dates), bg = bgs, value = values, label1 = labels1, label2 = labels2)



p <- ggplot() +
  geom_vline(xintercept = as.Date("2020-08-12"), linetype = "longdash", alpha = 0.4) +
  geom_vline(xintercept = as.Date("2020-08-21"), linetype = "longdash", alpha = 0.4) +
  geom_vline(xintercept = as.Date("2020-11-05"), linetype = "longdash", alpha = 0.4) +
  geom_vline(xintercept = as.Date("2020-11-15"), linetype = "longdash", alpha = 0.4) +
  geom_vline(xintercept = as.Date("2021-02-06"), linetype = "longdash", alpha = 0.4) +
  geom_vline(xintercept = as.Date("2021-02-16"), linetype = "longdash", alpha = 0.4) +
  geom_vline(xintercept = as.Date("2021-05-08"), linetype = "longdash", alpha = 0.4) +
  geom_vline(xintercept = as.Date("2021-05-18"), linetype = "longdash", alpha = 0.4) +
  geom_vline(xintercept = as.Date("2021-08-14"), linetype = "longdash", alpha = 0.4) +
  geom_vline(xintercept = as.Date("2021-08-14"), linetype = "solid",color = "red", alpha = 1, size = 0.75) +
  geom_vline(xintercept = as.Date("2021-08-24"), linetype = "longdash", alpha = 0.4) +
  geom_vline(xintercept = as.Date("2021-11-06"), linetype = "longdash", alpha = 0.4) +
  geom_vline(xintercept = as.Date("2021-11-16"), linetype = "longdash", alpha = 0.4) +
  geom_vline(xintercept = as.Date("2022-02-05"), linetype = "longdash", alpha = 0.4) +
  geom_vline(xintercept = as.Date("2022-02-15"), linetype = "longdash", alpha = 0.4) +
  annotate("text", x = as.Date("2021-08-14"), y = -2.5, label = "Neuer Webshop", color = "red", size = 3, hjust = -0.1) +
  geom_line(data = sub_ts, aes(x = sub_time, y = value, color = name)) +
  geom_point(data = volumes, aes(x = date, y = bg), alpha = 0.8) +
  geom_point(data = volumes, aes(x = date, y = value/200), alpha = 0.8) +
  geom_line(data = volumes, aes(x = date, y = bg), alpha = 0.25) +
  geom_text(data = volumes, aes(x = date, y = bg, label = label2), hjust=-0.1, vjust=0) +
  geom_line(data = volumes, aes(x = date, y = values/200), alpha = 0.25) +
  geom_text(data = volumes, aes(x = date, y = values/200, label = label1), hjust=-0.1, vjust=0) +
  ylab("Mitgliederentwicklung") +
  scale_color_discrete(name = "Mitgliederstatus (linke Achse)", labels = c("Mitgliederbeitrag offen","Mitglied")) +
  xlab("") +
  theme_bw() +
  scale_y_continuous(name="Personen", sec.axis = sec_axis(~ 200*., name="Bestellwert pro Runde (CHF)")) +
  theme(panel.grid.major.x = element_blank())

ggsave(p, filename = "/Users/laz/FoodcoopZurGertrud/plots/development.png", width = 30, height = 20, units = "cm")
