descriptive_stats_tbl <- descriptive_stats %>%
  gtsummary::tbl_summary(
    by = Statistic,  # Grouping by the Statistic column
    type = list(Value ~ "continuous"),  # All values are treated as continuous
    missing = "no"
  )

descriptive_stats_tbl

descriptive_stats_tbl %>%
  as_gt() %>%
  gt::gt_preview()


# Overlapping histograms with adjusted transparency and color
ggplot(carehome_data, aes(x = age_at_recording, fill = as.factor(carehome_id))) +
  geom_histogram(aes(y = ..density..), binwidth = 1, alpha = 0.6, position = "identity") +
  scale_fill_brewer(palette = "Set1", name = "Care Home") +  # Use a Brewer palette for distinct colors
  theme_minimal() +
  labs(title = "Overlapping Histograms of Age at Recording by Care Home",
       x = "Age at Recording",
       y = "Density") +
  theme(legend.position = "right")  # Adjust legend position as needed







