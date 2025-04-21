library(readxl)     
library(dplyr)     

df <- read_excel("HCI Experiment Data .xlsx", skip = 2)  
names(df) <- trimws(names(df)) 


# Helper function to convert to seconds

convert_to_seconds <- function(time_str) {
  if (is.na(time_str)) return(NA)
  mins <- as.numeric(stringr::str_extract(time_str, "\\d+(?=\\s*min)"))  
  secs <- as.numeric(stringr::str_extract(time_str, "\\d+(?=\\s*sec)"))  
  mins[is.na(mins)] <- 0
  secs[is.na(secs)] <- 0
  return(mins * 60 + secs)
}


# Convert full session time to seconds
df$session_time_secs <- sapply(df$`session time`, convert_to_seconds)

# Extract first clicked note time from comma-separated string and convert to seconds
first_click_time <- sapply(df$`clicked notes - times`, function(x) {
  if (is.na(x) || grepl("n/a", tolower(x))) return(NA)
  first_time <- unlist(strsplit(x, ","))[1]
  convert_to_seconds(first_time)
})
df$first_click_time_secs <- first_click_time

# Convert chat interaction count to numeric
df$chat_interactions <- as.numeric(df$`# chat interactions (count of chat bubbles)`)


# Filter valid, complete rows 

df_clean <- df %>%
  filter(!is.na(variant),                       
         !is.na(session_time_secs),             
         !is.na(first_click_time_secs),       
         !is.na(chat_interactions))             

# Independent Samples t-tests


# 1. Time to first click
t.test(first_click_time_secs ~ variant, data = df_clean)

# 2. Session duration
t.test(session_time_secs ~ variant, data = df_clean)

# 3. Number of chat interactions
t.test(chat_interactions ~ variant, data = df_clean)
