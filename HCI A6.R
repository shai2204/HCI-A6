# Load your data
library(readxl)
library(dplyr)
library(ggplot2)

# Read in the Excel file
df <- read_excel(""HCI Experiment Data .xlsx"", skip = 2)  # skip top two rows

# Clean column names
names(df) <- trimws(names(df))

# Helper: convert time strings like "2 mins 30 secs" to seconds
convert_to_seconds <- function(time_str) {
  if (is.na(time_str)) return(NA)
  mins <- as.numeric(stringr::str_extract(time_str, "\\d+(?=\\s*min)"))
  secs <- as.numeric(stringr::str_extract(time_str, "\\d+(?=\\s*sec)"))
  mins[is.na(mins)] <- 0
  secs[is.na(secs)] <- 0
  return(mins * 60 + secs)
}

# Apply conversions
df$session_time_secs <- sapply(df$`session time`, convert_to_seconds)

# Extract first click time from string (before first comma)
first_click_time <- sapply(df$`clicked notes - times`, function(x) {
  if (is.na(x) || grepl("n/a", tolower(x))) return(NA)
  first_time <- unlist(strsplit(x, ","))[1]
  convert_to_seconds(first_time)
})
df$first_click_time_secs <- first_click_time

# Clean chat interaction count
df$chat_interactions <- as.numeric(df$`# chat interactions (count of chat bubbles)`)

# Subset relevant and complete rows
df_clean <- df %>%
  filter(!is.na(variant), !is.na(session_time_secs), !is.na(first_click_time_secs), !is.na(chat_interactions))

# Shapiro-Wilk normality tests
shapiro.test(df_clean$first_click_time_secs[df_clean$variant == "variant 1"])
shapiro.test(df_clean$first_click_time_secs[df_clean$variant == "variant 2"])

shapiro.test(df_clean$session_time_secs[df_clean$variant == "variant 1"])
shapiro.test(df_clean$session_time_secs[df_clean$variant == "variant 2"])

shapiro.test(df_clean$chat_interactions[df_clean$variant == "variant 1"])
shapiro.test(df_clean$chat_interactions[df_clean$variant == "variant 2"])

# Run t-tests (based on normality)
t.test(first_click_time_secs ~ variant, data = df_clean)
t.test(session_time_secs ~ variant, data = df_clean)
t.test(chat_interactions ~ variant, data = df_clean)
