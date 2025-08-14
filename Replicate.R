# R script for reproducing charts in Sturgis et al (2025) SOCbot: Using Large Language Models to measureand classify occupations in surveys

# Load required libraries
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)

# Set working directory
setwd("Your-working-directory")

# Load CSV data
df <- read.csv("SOCbot_survey_replicate.csv", stringsAsFactors = FALSE)

# Produce Figure 2

# Define odd-numbered probe variables (v1, v3, ..., v19)
probe_vars <- paste0("X_v", seq(1, 19, by = 2))

# Determine how many probes were needed before first CGPT code appears
df$num_probes <- sapply(1:nrow(df), function(i) {
  for (j in seq_along(probe_vars)) {
    val <- as.character(df[i, probe_vars[j]])
    if (isTRUE(nzchar(val)) && grepl("^\\s*CGPT587[:\\-]", val)) return(j - 1)
  }
  return(NA_integer_)
})

# Bar plot of number of probes needed
freq_df <- df %>%
  filter(!is.na(num_probes)) %>%
  count(num_probes) %>%
  mutate(Percent = round(100 * n / sum(n), 1),
         Label = paste0(Percent, "%"))

ggplot(freq_df, aes(x = factor(num_probes), y = Percent)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = Label), vjust = -0.5, size = 3) +
  labs(
    # title = "Number of Probes Needed to Code to SOC",
    x = "Number of probes",
    y = "Percentage (%)"
  ) +
  coord_cartesian(ylim = c(0, 60)) +  # Set Y-axis limit
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# Produce Figure 3 

# Convert blank strings to NA across all probe_code columns (do this first)
df <- df %>%
  mutate(across(starts_with("probe_code"),
                ~ na_if(trimws(.), "")))

# Create long-format data frame and clean missing codes
df_long <- df %>%
  select(responseid, probe_code1, probe_code2, probe_code3) %>%
  pivot_longer(cols = starts_with("probe_code"),
               names_to = "probe_num",
               values_to = "code") %>%
  filter(!is.na(code), code != "NA") %>%  # Clean both NA and string "NA"
  mutate(probe_num = case_when(
    probe_num == "probe_code1" ~ "Probe 1",
    probe_num == "probe_code2" ~ "Probe 2",
    probe_num == "probe_code3" ~ "Probe 3"
  ))

# Calculate percentages
df_summary <- df_long %>%
  group_by(probe_num, code) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(probe_num) %>%
  mutate(percent = 100 * n / sum(n))

# Plot of probe type by probe number
ggplot(df_summary, aes(x = probe_num, y = percent, fill = code)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(
    aes(label = paste0(round(percent, 1), "%")),
    position = position_dodge(width = 0.9),
    vjust = -0.3,
    size = 2.5
  ) +
  labs(
    x = "Probe number",
    y = "Percentage",
    fill = "Probe code",
    #  title = "Distribution of Probe Type by Probe Number"
  ) +
  scale_fill_manual(values = c(
    "clarification" = "#6A5ACD",      # Slate Blue
    "industry" = "#556B2F",           # Dark Olive Green
    "job tasks" = "#4682B4",          # Steel Blue
    "other" = "#D2691E",              # Dark Orange
    "qualifications" = "#C71585"      # Medium Violet Red
  )) +
  coord_cartesian(ylim = c(0, 80)) +  
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )

# Produce Figure 4

# --- Libraries ---
library(dplyr)
library(tidyr)
library(ggplot2)

# --- Small helpers ---
truncate_soc <- function(x, digits) substr(as.character(x), 1, digits)

pair_agree_pct <- function(a, b) {
  # Returns % agreement (0–100) with NA-safe handling
  mean(a == b, na.rm = TRUE) * 100
}

# --- Sanity check for required columns ---
required_cols <- c("SOCbot", "SOCbot_probe", "human")
stopifnot(all(required_cols %in% names(df)))

# --- Build agreement table for 1–4 digit levels ---
agreement_long <- lapply(1:4, function(d) {
  dd <- df %>%
    mutate(
      SOCbot_d       = truncate_soc(SOCbot, d),
      SOCbot_probe_d = truncate_soc(SOCbot_probe, d),
      human_d        = truncate_soc(human, d)
    )
  
  tibble::tibble(
    digit_level = paste0(d, "-digit"),
    `SOCbot vs SOCbot_probe` = pair_agree_pct(dd$SOCbot_d,       dd$SOCbot_probe_d),
    `SOCbot vs Human`        = pair_agree_pct(dd$SOCbot_d,       dd$human_d),
    `SOCbot_probe vs Human`  = pair_agree_pct(dd$SOCbot_probe_d, dd$human_d)
  )
}) %>%
  dplyr::bind_rows() %>%
  tidyr::pivot_longer(
    cols = -digit_level,
    names_to = "comparison",
    values_to = "percent_agreement"
  ) %>%
  mutate(
    comparison = factor(
      comparison,
      levels = c(
        "SOCbot vs SOCbot_probe",
        "SOCbot vs Human",
        "SOCbot_probe vs Human"
      )
    )
  )

# --- Plot ---
p <- ggplot(agreement_long,
            aes(x = digit_level, y = percent_agreement, fill = comparison)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_text(aes(label = sprintf("%.1f%%", percent_agreement)),
            position = position_dodge(width = 0.8), vjust = -0.3, size = 2.8) +
  labs(
    x = "SOC digit level",
    y = "Percent agreement",
    fill = "Coder pair"
  ) +
  ylim(0, 100) +
  theme_minimal() +
  theme(panel.grid = element_blank())

print(p)
