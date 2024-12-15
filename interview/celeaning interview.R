library(tidyverse)
library(here)


text <-  read_delim("closed_caption.txt", delim = "\n") %>%
  rename(raw_text = "14:28:02 Bye.")

text_clean <- text %>%
  mutate(raw_text = str_remove_all(raw_text, "\\d+:\\d+:\\d+"))


write_lines(
  apply(text_clean, 1, paste, collapse = "\n"), # Combine columns with a space or another separator
  "output_file.txt"
)