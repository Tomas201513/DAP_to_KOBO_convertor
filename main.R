library(readxl)
library(tidyr)
library(dplyr)
library(stringr)

# Step 1: Import the Excel file
data <- read_excel("C:/Users/test/Downloads/V6_REACH HSM_DAP_10Dec2024 (1).xlsx", sheet = "Data Analysis Plan - Dec 2024")
data <- data %>% select(`# IN`, Question, `Answer Options`,`Instructions`)

# Step 2: Split multiline cells into separate rows
# Assuming the columns are named as 'Question' and 'Options'

data_long <- data %>%
  mutate(Options = str_split(`Answer Options`, "\n")) %>%  # Split lines based on '\n'
  unnest(Options) %>%                            # Unnest the list into separate rows
  fill(Question, .direction = "down")            # Fill down the Question column

# View(data_long)


# create "option_name"" column by removing "'" from the "Options" column
data_long <- data_long %>%
  mutate(option_name = str_replace_all(Options, "'|’", ""))

# View(data_long)

# replace "/" with "_or_" in the "option_name" column
data_long <- data_long %>%
  mutate(option_name = str_replace_all(option_name, "/", "_or_"))


# View(data_long)



# replace all "do not know, donot know, don't know, dont know,I don't know, I do not know, I don't know, I dont know" with "dnk" in the "option_name" column

data_long <- data_long %>%
  mutate(option_name = str_replace_all(option_name, "do not know|donot know|don't know|dont know|I don't know|I do not know|I don't know|I dont know", "dnk"))

# View(data_long)


# replace all "prefer not to answer, i prefer not to answer, i prefer not to answer, prefer not to answer, prefer not to answer, prefer not to answer" with "pna" in the "option_name" column

data_long <- data_long %>%
  mutate(option_name = str_replace_all(option_name, "prefer not to answer|i prefer not to answer|Prefer not to answer|Prefer not to answer", "pnta"))

# View(data_long)


data_long <- data_long %>%
  mutate(option_name = str_replace_all(option_name, "Other (specify)|Other |Others|other|Other (please specify)|Other,please specify", "other"))

View(data_long)


# make the "option_name" column all small letters
data_long <- data_long %>%
  mutate(option_name = tolower(option_name))

View(data_long)




# create "option_name"" column which is by making the "Options" column all small letters and replacing spaces with "_" and removing special characters
data_long <- data_long %>%
  mutate(option_name = tolower(Options)) %>%
  mutate(option_name = str_replace_all(option_name, "[^[:alnum:]]", "_")) %>%
  mutate(option_name = str_replace_all(option_name, "_+", "_")) %>%
  mutate(option_name = str_remove_all(option_name, "_$")) %>%
  mutate(option_name = str_remove_all(option_name, "^_"))

View(data_long)
