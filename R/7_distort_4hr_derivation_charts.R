model_df <- readRDS(here("data", "model_df_cut_50pc.RDS"))



chart_df <-
  model_df |> 
  group_by(duration_ed) |> 
  summarise(n = n())

n_10_before <-
  chart_df |> 
  filter(duration_ed >= 231 & duration_ed <= 240) |> 
  summarise(n_10_before = sum(n) / 10) |> 
  pull()

n_10_after <-
  chart_df |> 
  filter(duration_ed >= 241 & duration_ed <= 250) |> 
  summarise(n_10_before = sum(n) / 10) |> 
  pull()

n_10_before_after <-
  chart_df |> 
  filter(duration_ed >= 231 & duration_ed <= 250) |> 
  summarise(n_10_before = sum(n) / 20) |> 
  pull()



# expected
chart_df |> 
  ggplot() +
  #geom_point(aes(x = duration_ed, y = n), size = 1) +
  geom_smooth(aes(x = duration_ed, y = n),
              method = "loess",
              formula = "y ~ x",
              span = 0.6,
              se = FALSE) +
  geom_vline(aes(xintercept = 240), colour = "black") +
  # annotate("rect", 
  #          xmin = 231, xmax = 240, ymin = 0, ymax = 20100,
  #          fill = "red",
  #          alpha = 0.2) +
  # annotate("segment",
  #          x = 231, xend = 240, y = n_10_before, yend = n_10_before,
  #          colour = "red",
  #          linewidth = 2) +
  # annotate("rect", 
  #          xmin = 241, xmax = 250, ymin = 0, ymax = 20100,
  #          fill = "green",
  #          alpha = 0.2) +
  # annotate("segment",
  #          x = 241, xend = 250, y = n_10_after, yend = n_10_after,
  #          colour = "green",
  #          linewidth = 2) +
  # annotate("segment",
  #          x = 241, xend = 250, y = n_10_before_after, yend = n_10_before_after,
  #          colour = "black",
  #          linewidth = 2) +
  scale_y_continuous(name = "frequency of attendances",
                     limits = c(0, 20100)) +
  scale_x_continuous(name = "duration in ED (minutes)",
                     limits = c(0, 720)) +
  theme(axis.text.y = element_blank())



# expected and actual
chart_df |> 
  ggplot() +
  geom_point(aes(x = duration_ed, y = n), size = 1) +
  geom_smooth(aes(x = duration_ed, y = n),
              method = "loess",
              formula = "y ~ x",
              span = 0.6,
              se = FALSE) +
  geom_vline(aes(xintercept = 240), colour = "black") +
  # annotate("rect", 
  #          xmin = 231, xmax = 240, ymin = 0, ymax = 20100,
  #          fill = "red",
  #          alpha = 0.2) +
  # annotate("segment",
  #          x = 231, xend = 240, y = n_10_before, yend = n_10_before,
  #          colour = "red",
  #          linewidth = 2) +
  # annotate("rect", 
  #          xmin = 241, xmax = 250, ymin = 0, ymax = 20100,
  #          fill = "green",
  #          alpha = 0.2) +
  # annotate("segment",
  #          x = 241, xend = 250, y = n_10_after, yend = n_10_after,
  #          colour = "green",
  #          linewidth = 2) +
  # annotate("segment",
  #          x = 241, xend = 250, y = n_10_before_after, yend = n_10_before_after,
  #          colour = "black",
  #          linewidth = 2) +
  scale_y_continuous(name = "frequency of attendances",
                     limits = c(0, 20100)) +
  scale_x_continuous(name = "duration in ED (minutes)",
                     limits = c(0, 720)) +
  theme(axis.text.y = element_blank())



# expected and actual + segments
chart_df |> 
  ggplot() +
  geom_point(aes(x = duration_ed, y = n), size = 1) +
  geom_smooth(aes(x = duration_ed, y = n),
              method = "loess",
              formula = "y ~ x",
              span = 0.6,
              se = FALSE) +
  geom_vline(aes(xintercept = 240), colour = "black") +
  annotate("rect",
           xmin = 231, xmax = 240, ymin = 0, ymax = 20100,
           fill = "red",
           alpha = 0.2) +
  annotate("segment",
           x = 231, xend = 240, y = n_10_before, yend = n_10_before,
           colour = "red",
           linewidth = 2) +
  annotate("rect",
           xmin = 241, xmax = 250, ymin = 0, ymax = 20100,
           fill = "green",
           alpha = 0.2) +
  annotate("segment",
           x = 241, xend = 250, y = n_10_after, yend = n_10_after,
           colour = "green",
           linewidth = 2) +
  # annotate("segment",
  #          x = 241, xend = 250, y = n_10_before_after, yend = n_10_before_after,
  #          colour = "black",
  #          linewidth = 2) +
  scale_y_continuous(name = "frequency of attendances",
                     limits = c(0, 20100)) +
  scale_x_continuous(name = "duration in ED (minutes)",
                     limits = c(0, 720)) +
  theme(axis.text.y = element_blank())



