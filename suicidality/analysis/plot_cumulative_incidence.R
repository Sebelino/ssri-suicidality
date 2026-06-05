# plot_cumulative_incidence.R
# Plot cumulative incidence of suicidal behavior events over 52 weeks
# by treatment group (SSRI initiators vs non-initiators)

library(dplyr)
library(ggplot2)
library(here)
here::i_am("suicidality/analysis/plot_cumulative_incidence.R")

# =============================================================================
# LOAD DATA
# =============================================================================

data <- readRDS(here("suicidality", "extraction", "output", "rds", "main_52wks_28.rds"))

cat("N:", nrow(data), "\n")
cat("Events (sb52_itt):", sum(data$sb52_itt == 1), "\n")

# Calculate days to event or censoring
data <- data %>%
  mutate(
    days = as.integer(fu_end_itt - fu_start),
    event = sb52_itt
  )

# =============================================================================
# CUMULATIVE EVENT COUNTS BY WEEK
# =============================================================================

max_weeks <- 52
weeks <- 1:max_weeks

# Count cumulative events by week and treatment group
cum_events <- data %>%
  filter(event == 1) %>%
  mutate(event_week = ceiling(days / 7)) %>%
  group_by(cc) %>%
  arrange(event_week) %>%
  mutate(cum_events = row_number()) %>%
  # Expand to all weeks
  group_by(cc, event_week) %>%
  summarise(cum_events = max(cum_events), .groups = "drop")

# Fill in all weeks (carry forward)
full_grid <- expand.grid(cc = c(0, 1), week = weeks) %>%
  as_tibble()

cum_events_full <- full_grid %>%
  left_join(cum_events, by = c("cc", "week" = "event_week")) %>%
  group_by(cc) %>%
  arrange(week) %>%
  tidyr::fill(cum_events, .direction = "down") %>%
  mutate(cum_events = ifelse(is.na(cum_events), 0, cum_events)) %>%
  ungroup() %>%
  mutate(
    treatment = factor(cc, levels = c(0, 1), labels = c("No SSRI", "SSRI"))
  )

# =============================================================================
# PLOT: CUMULATIVE EVENT COUNT
# =============================================================================

output_dir <- here("suicidality", "analysis", "output")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

p <- ggplot(cum_events_full, aes(x = week, y = cum_events, colour = treatment)) +
  geom_step(linewidth = 0.8) +
  geom_vline(xintercept = 12, linetype = "dashed", colour = "grey50", linewidth = 0.4) +
  annotate("text", x = 12.5, y = max(cum_events_full$cum_events) * 0.95,
           label = "12 weeks", hjust = 0, size = 3, colour = "grey40") +
  scale_colour_manual(values = c("No SSRI" = "#F4A582", "SSRI" = "#4393C3")) +
  scale_x_continuous(breaks = seq(0, 52, by = 4)) +
  labs(
    title = "Cumulative Suicidal Behavior Events Over 52 Weeks",
    x = "Weeks since baseline",
    y = "Cumulative number of events",
    colour = "Treatment"
  ) +
  theme_bw(base_size = 13) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold")
  )

ggsave(file.path(output_dir, "cumulative_incidence_52wks.pdf"),
       p, width = 8, height = 5)

cat("Saved: cumulative_incidence_52wks.pdf\n")

# =============================================================================
# PLOT: CUMULATIVE INCIDENCE RATE (%)
# =============================================================================

n_by_group <- data %>% count(cc)

cum_events_rate <- cum_events_full %>%
  left_join(n_by_group, by = "cc") %>%
  mutate(rate_pct = 100 * cum_events / n)

p2 <- ggplot(cum_events_rate, aes(x = week, y = rate_pct, colour = treatment)) +
  geom_step(linewidth = 0.8) +
  geom_vline(xintercept = 12, linetype = "dashed", colour = "grey50", linewidth = 0.4) +
  annotate("text", x = 12.5, y = max(cum_events_rate$rate_pct) * 0.95,
           label = "12 weeks", hjust = 0, size = 3, colour = "grey40") +
  scale_colour_manual(values = c("No SSRI" = "#F4A582", "SSRI" = "#4393C3")) +
  scale_x_continuous(breaks = seq(0, 52, by = 4)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title = "Cumulative Incidence of Suicidal Behavior Over 52 Weeks",
    x = "Weeks since baseline",
    y = "Cumulative incidence (%)",
    colour = "Treatment"
  ) +
  theme_bw(base_size = 13) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold")
  )

ggsave(file.path(output_dir, "cumulative_incidence_rate_52wks.pdf"),
       p2, width = 8, height = 5)

cat("Saved: cumulative_incidence_rate_52wks.pdf\n")

# =============================================================================
# PLOT: WEEKLY INCIDENCE (NEW EVENTS PER WEEK)
# =============================================================================

weekly_events <- data %>%
  filter(event == 1) %>%
  mutate(event_week = ceiling(days / 7)) %>%
  count(cc, event_week, name = "events") %>%
  rename(week = event_week)

# Fill missing weeks with 0
weekly_full <- expand.grid(cc = c(0, 1), week = weeks) %>%
  as_tibble() %>%
  left_join(weekly_events, by = c("cc", "week")) %>%
  mutate(
    events = ifelse(is.na(events), 0, events),
    treatment = factor(cc, levels = c(0, 1), labels = c("No SSRI", "SSRI"))
  )

p3 <- ggplot(weekly_full, aes(x = week, y = events, colour = treatment)) +
  geom_line(linewidth = 0.6, alpha = 0.4) +
  geom_smooth(method = "loess", span = 0.3, se = FALSE, linewidth = 1) +
  geom_vline(xintercept = 12, linetype = "dashed", colour = "grey50", linewidth = 0.4) +
  annotate("text", x = 12.5, y = max(weekly_full$events) * 0.95,
           label = "12 weeks", hjust = 0, size = 3, colour = "grey40") +
  scale_colour_manual(values = c("No SSRI" = "#F4A582", "SSRI" = "#4393C3")) +
  scale_x_continuous(breaks = seq(0, 52, by = 4)) +
  labs(
    title = "Weekly New Suicidal Behavior Events Over 52 Weeks",
    x = "Weeks since baseline",
    y = "New events per week",
    colour = "Treatment"
  ) +
  theme_bw(base_size = 13) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold")
  )

ggsave(file.path(output_dir, "weekly_incidence_52wks.pdf"),
       p3, width = 8, height = 5)

cat("Saved: weekly_incidence_52wks.pdf\n")
