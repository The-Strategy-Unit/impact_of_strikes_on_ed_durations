

# colours
c_jr_col <- "#E41A1C"
jdoc_col <- "#377EB8"
cons_col <- "#4DAF4A"
para_col <- "#984EA3"
nurs_col <- "#FF7F00"

oth_col1 <- "#A65628"
oth_col2 <- "#FFFF33"
oth_col3 <- "#F781BF"

grey_col <- "lightgrey"


strike_dates_df <- readRDS(here("_targets/objects", "strike_dates_df"))
unusual_dates_df <- readRDS(here("_targets/objects", "unusual_dates_df"))
strike_dts <- readRDS(here("_targets/objects", "strike_dts"))


cal_start <- ymd("2022-01-01")
cal_end <- ymd("2024-12-31")


simple_calendar_strike_df <- 
  data.frame(
    date = seq.Date(cal_start, cal_end, by = "day")) |> 
  mutate(year = year(date) - 2000,
         month = month(date, label = TRUE, abbr = TRUE),
         week = floor(interval(cal_start, date) / days(1) / 7) + 1,
         day = day(date),
         weekday = substr(weekdays(date, abbr = TRUE), 1, 2)) |> 
  mutate(weekday = factor(weekday,
                          levels = c("Sa", "Su", "Mo", "Tu", "We", "Th", "Fr"))) |> 
  mutate(month_year = paste(month, year, sep = "-")) |> 
  mutate(month_year = factor(month_year,
                             levels = c("Jan-22", "Feb-22", "Mar-22", "Apr-22", "May-22", "Jun-22", 
                                        "Jul-22", "Aug-22", "Sep-22", "Oct-22", "Nov-22", "Dec-22",
                                        "Jan-23", "Feb-23", "Mar-23", "Apr-23", "May-23", "Jun-23", 
                                        "Jul-23", "Aug-23", "Sep-23", "Oct-23", "Nov-23", "Dec-23",
                                        "Jan-24", "Feb-24", "Mar-24", "Apr-24", "May-24", "Jun-24", 
                                        "Jul-24", "Aug-24", "Sep-24", "Oct-24", "Nov-24", "Dec-24"))) |> 
  group_by(month_year) |> 
  mutate(week_month = dense_rank(week)) |> 
  ungroup() |> 
  left_join(strike_dates_df |> 
              mutate(day_type = "strike day") |> 
              mutate(date = as_date(date)), 
            join_by(date)) |> 
  mutate(day_type = ifelse(is.na(day_type), "other day", day_type)) |> 
  mutate(day_type = factor(day_type,
                           levels = c("strike day", "other day")))



detailed_calendar_strike_df <- 
  data.frame(
    date = seq.Date(cal_start, cal_end, by = "day")) |> 
  mutate(year = year(date) - 2000,
         month = month(date, label = TRUE, abbr = TRUE),
         week = floor(interval(cal_start, date) / days(1) / 7) + 1,
         day = day(date),
         weekday = substr(weekdays(date, abbr = TRUE), 1, 2)) |> 
  mutate(weekday = factor(weekday,
                          levels = c("Sa", "Su", "Mo", "Tu", "We", "Th", "Fr"))) |> 
  mutate(month_year = paste(month, year, sep = "-")) |> 
  mutate(month_year = factor(month_year,
                             levels = c("Jan-22", "Feb-22", "Mar-22", "Apr-22", "May-22", "Jun-22", 
                                        "Jul-22", "Aug-22", "Sep-22", "Oct-22", "Nov-22", "Dec-22",
                                        "Jan-23", "Feb-23", "Mar-23", "Apr-23", "May-23", "Jun-23", 
                                        "Jul-23", "Aug-23", "Sep-23", "Oct-23", "Nov-23", "Dec-23",
                                        "Jan-24", "Feb-24", "Mar-24", "Apr-24", "May-24", "Jun-24", 
                                        "Jul-24", "Aug-24", "Sep-24", "Oct-24", "Nov-24", "Dec-24"))) |> 
  group_by(month_year) |> 
  mutate(week_month = dense_rank(week)) |> 
  ungroup() |> 
  left_join(strike_dates_df |> 
              mutate(date = as_date(date)) |> 
              pivot_wider(names_from = measure,
                          values_from = value,
                          values_fill = 0) |> 
              mutate(day_type = case_when(`consultants industrial action (hrs/24)` > 0 &  
                                            `jr doctors industrial action (hrs/24)` > 0 ~ "strike - consultants & jr doctors",
                                          `ambulance industrial action (hrs/24)` > 0 ~ "strike - ambulance",
                                          `consultants industrial action (hrs/24)` > 0 ~ "strike - consultants",
                                          `jr doctors industrial action (hrs/24)` > 0 ~ "strike - jr doctors",
                                          `nurses industrial action (hrs/24)` > 0 ~ "strike - nurses",
                                          
                                          TRUE ~ "check")) |> 
              dplyr::select(date, day_type), 
            join_by(date)) |> 
  mutate(day_type = ifelse(is.na(day_type), "other day", day_type)) |> 
  mutate(day_type = factor(day_type,
                           levels = c("strike - consultants & jr doctors", 
                                      "strike - consultants", 
                                      "strike - jr doctors",
                                      "strike - nurses",
                                      "strike - ambulance",
                                      "other day")))



detailed_calendar_strike_df |> 
  filter(date >= "2022-12-01",
         date <= "2024-7-31")  |> 
  ggplot() +
  geom_tile(aes(x = weekday, 
                y = week_month,
                fill = day_type)) +
  geom_text(
    aes(x = weekday,
        y = week_month,
        label = day),
    size = 3) +
  lemon::facet_rep_wrap( ~ month_year, ncol = 5, repeat.tick.labels = 'left') +
  scale_x_discrete(name = "") +
  scale_y_reverse(name = "",
                  breaks = NULL) +
  scale_fill_manual(labels = c("consultants & resident (junior) doctors", 
                               "consultants", 
                               "resident (junior) doctors",
                               "nurses",
                               "paramedics",
                               "other day"),
                    values = c(c_jr_col, 
                               cons_col, 
                               jdoc_col, 
                               nurs_col,  
                               para_col, 
                               grey_col)) +
  theme(legend.title = element_blank(),
        strip.background = element_rect(fill = "#b4c6e6"),
        legend.position = "bottom",
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        linewidth = 0.5, linetype = "solid"),
        axis.ticks.x = element_blank())



ggsave(filename = here("charts", "strikes_calendar.jpg"),
       device = "jpg",
       dpi = 300,
       units = "cm",
       width = 25,
       height = 17) 
