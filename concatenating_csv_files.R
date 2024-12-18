###################################################################################
#                Concatenating multiple csv files R script
# This R script concatenates all csv files in a directory path
#
# For this example I had 3 separate csv files: test1, test2, test3.
#  The resulting script creates a new csv file, with data from the 3 files.
#  The master csv file is saved in the specified output directory
#  
#
# (c) Lucy Jackson (2024)/https://lucy-jackson.github.io/
###################################################################################
# Load in packages
library(dplyr)
library(readr)
library(purr)


# Creates a variable to store all csv files stored in X folder & their directory paths
dpath <- list.files(path = "directorypath/",
                    pattern = "\\.csv$", full.names = TRUE)

# Print dpath to make sure all your csv files that you want to merge are there
print(dpath)


# Concatenating all the csv files using the map_dfr function from purrr package
    # this creates a data frame of the concatenated csv files
master_data <- dpath %>%
  map_dfr(~ read_csv(.x, show_col_types = FALSE))

# Previewing the data
head(master_data)
tail(master_data)

# Write the 'master_data' data frame to a CSV file
write_csv(master_data, "outputdirectorypath/master.csv")
