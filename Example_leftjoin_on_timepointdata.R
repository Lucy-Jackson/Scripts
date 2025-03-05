######################################################################################################################
#                Left-Join Example R Script
# This R script exemplifies how to perform a left-join on two .csv files
#
# Example: 
# Two datasets:
#        data1: contains blank columns for speaker and timepoints of the target and primer words in the sound file: 
#        _____________________________________________________________________________
#       fileName       | speaker | targetOnset|	targetOffset|	PrimerOnset	| PrimerOffset
#       ------------------------------------------------------------------------------
#       sp1_apple.wav      NA           NA           NA            NA           NA
#       ------------------------------------------------------------------------------
#
#       data2: contains all the extracted time data from varioussentences spoken by multiple speakers.
#            presented in long format, containing timepoint metrics for both primer and target words in each sentence:
#
#      ______________________________________________________________________________________
#     fileName	    |  segment	|startPoint	|endPoint	|duration_ms	|midInterval | sentencee
#      --------------------------------------------------------------------------------------
#     sp1_apple.wav     	primer	   0.07	      0.87	      800          	0.47      sp1_apple  
#     sp1_apple.wav       target     2         2.47	      470          	 2.235      sp1_apple
#     ---------------------------------------------------------------------------------------
#
#
#     OUTCOME OF THE SCRIPT
#    1) Extracts part of sentence from data1 to create the 'speaker' column: e.g., sentence = sp1_apple -> speaker = sp1
#    2) Filter the data1 set for the target segments = data1_target where only the columns fileName, 
#          startPoint, and endPoint are retained for merging (carried out again for primer segments in file).
#    3) Perform a left join that combines data2 with the filtered data1_target using fileName as the key. 
#    4) Updated data2 now has the speaker, targetOnset and Offset columns filled in for each fileName. 
#    5) Note**: Target and Primers have been separated for clarity in this example.
#        But this can be carried out again on a filtered_primer dataset. 
#
#        RESULTING DATA 
#   
#        _____________________________________________________________________________
#       fileName       | speaker | targetOnset|	targetOffset|	PrimerOnset	|PrimerOffset
#       ------------------------------------------------------------------------------
#       sp1_file.wav      sp1        2             2.47          0.07          0.87
#      -------------------------------------------------------------------------------
#
# 
#
# (c) Lucy Jackson (2024)/https://lucy-jackson.github.io/
######################################################################################################################

library(dplyr) # Load in relevant packages

# Load in the first dataset
data1 <- read.csv("data1.csv", stringsAsFactors = TRUE)

# Extract information and create new columns
# Example: Extract a 'speaker' from the file name prefix and a 'sentence' from the suffix
data1_clean <- data1 %>%
  mutate(speaker = str_extract(fileName, "^[A-Za-z]+"),
         sentence = str_extract(fileName, "(?<=_).*")) 

head(data1_clean)  # Preview the cleaned data

# Save the cleaned data to a new file
write.csv(data1_clean, "data1_clean.csv")

# -----------------------------------------
# Filtering the data to focus on data from target words only
# For example: Keep only rows where 'segment' is 'target'
data1_target <- data1_clean %>%
  filter(segment == 'target') %>%         # Keep only 'target' rows
  select(fileName, startPoint, endPoint)  # Select relevant columns for merging

head(data1_target)  # Preview filtered data

# -----------------------------------------
# Load in the second dataset
data2 <- read.csv("data2.csv", stringsAsFactors = TRUE)
head(data2)

# Perform a LEFT JOIN to combine the datasets
# Add 'startPoint' from data1_target to data2
# Replace or modify a column in data2 based on the join result
data2_updated <- data2 %>%
  left_join(data1_target, by = "fileName") %>%
  mutate(targetOnset = ifelse(!is.na(startPoint), startPoint, targetOnset)) %>% 
  select(-startPoint)  # Remove 'startPoint' after using it

head(data2_targetupdated)  # Preview the final merged data

# Perform a LEFT JOIN to combine the datasets
# Add 'endPoint' from data1_target to data2
# Replace or modify a column in data2 based on the join result
data2_updated <- data2 %>%
  left_join(data2_targetupdated, by = "fileName") %>%
  mutate(targetOffset = ifelse(!is.na(endPoint), endPoint, targetOffset)) %>% 
  select(-endPoint)  # Remove 'endPoint' after using it

head(data2_targetprimerupdated)  # Preview the final merged data



# Save resulting merged data to .csv file
write.csv(data2_targetprimerupdated, "target-primer_timepoints.csv")
