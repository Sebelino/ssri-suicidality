# predi_diff_distribution.R
# Distribution of time from diagnosis to SSRI initiation among initiators

library(ggplot2)
library(dplyr)
library(here)
here::i_am("suicidality/analysis/predi_diff_distribution.R")

source(here("suicidality", "analysis", "common.R"))

# predi_diff distribution describes the complete-case analysis cohort, so
# initiator counts agree with Table 2 and the headline §3.1 prose.
data <- filter_complete_cases(read_rds_file("main_12wks_28.rds"))

initiators <- data %>%
  filter(cc == 1) %>%
  mutate(predi_diff = as.integer(prescr - diagn_date))

p <- ggplot(initiators, aes(x = predi_diff)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  scale_x_continuous(breaks = seq(0, 28, by = 7)) +
  labs(
    title = "Time from depression diagnosis to SSRI initiation",
    subtitle = sprintf("Among %s SSRI initiators", format(nrow(initiators), big.mark = ",")),
    x = "Days from diagnosis to SSRI dispensation",
    y = "Number of individuals"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold")
  )

ggsave(output_path("predi_diff_distribution.pdf"), p, width = 8, height = 5)
cat("Saved: predi_diff_distribution.pdf\n")
