###################################################################################
#                Text-Image Generator R Script
# This R script generates individual .jpg images of specified words. 
# Format: 2000 x 2000 pixels at 600 DPI
#  
#  1) In the example script below, 4 jpg images are generated, with the words:
#     'dog, cat, banana, apple' in a centred position. 
#  2) Text size, position and colour can be amended in the annotate() argument.
#  3) Image background colour, size and dpi can also be amended. 
#  4) Images will be saved in working directory.
#
# (c) Lucy Jackson (2024)/https://lucy-jackson.github.io/
###################################################################################

library(ggplot2)



# Full list of words
words <- c("dog", "cat", "banana", "apple")

# Exact width and height for 2000 x 2000 pixels at 600 DPI
width_exact <- 2000 / 600  # 3.333333 inches
height_exact <- 2000 / 600

# Loop to create and save images
for (word in words) {
  p <- ggplot() +
    annotate("text", x = 0.5, y = 0.5, label = word, size = 20, color = "black") +
    theme_void() +  # Remove all axes and gridlines
    theme(plot.background = element_rect(fill = "white"))  # White background
  
  # Save as .jpg file with 2000 x 2000 pixels at 600 DPI
  ggsave(filename = paste0(word, ".jpg"), 
         plot = p, 
         width = width_exact, 
         height = height_exact, 
         units = "in", 
         dpi = 600)
}
