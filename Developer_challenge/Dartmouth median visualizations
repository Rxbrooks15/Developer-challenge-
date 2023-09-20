
# Developer challenge  ----------------------------------------------------

#challenge to create visualization for Dartmouth courses dataset 

# Libraries ---------------------------------------------------------------


library(readxl)
library(tidyverse)

library(gganimate)
library(readr)
library(readxl)
library(gridExtra)
# install.packages("rvest")
library(rvest)

library(sf)
library(sp)
library(magick)
library(jpeg)
library(lubridate)
library(urbnmapr)
library(ggridges)



# Loading and exploring Data ----------------------------------------------


dart_courses <- read_csv(file.choose())
file.choose()

dart_courses <- read_csv("C:\\Users\\raine\\Desktop\\DALI\\Dartmouth - Courses.csv")
courses

glimpse(dart_courses)
head(dart_courses)
tail(dart_courses)
dim(dart_courses)

str(dart_courses)


dart_courses %>% 
  View
colnames(dart_courses)

nrow(dart_courses)
summary(dart_courses)

class(dart_courses$`Median GPA Points`)

# Personal stylized themes and palettes -----------------------------------


#Personal theme
Rain_theme <- function() {
  theme_minimal() +
    theme(
      text = element_text(size = 12, family = "Times New Roman"), # change font size and family
      plot.background = element_rect(fill = "#E7FFF2"),  # change background color
      panel.grid.major = element_blank(),                  # remove major gridlines
      panel.grid.minor = element_blank(),                  # remove minor gridlines
      panel.border = element_blank(),                      # remove plot border
      axis.line = element_line(color = "black")             # change axis line color
    )
}
#Personal sequencial pallete that accounts for color blindness 
seq_pal <- colorRampPalette(c("#F5F7D0","#44A6CB","#10007C"))
#personal categorical/ qualitative palette 
rain_color_palette <- c("#0072B2", "#009E73", "#F0E442", "#D55E00", "#CC79A7")

seq_pal <- c("red","#10007C","#44A6CB")



# Data manipulation + test visualization -------------------------------------------------------


dart_courses %>% 
  filter(!is.na(Department)) %>%
  group_by(Department) %>% 
  #Get singular GPA point for each department 
  summarise_at(vars(`Median GPA Points`), mean, na.rm = TRUE) %>%
  #Reorder Department values based on the value of the Median GPA. Highest to top and left... lowest to bottom and right 
  mutate(Department = reorder(Department, `Median GPA Points`, FUN = function(x) -mean(x))) %>%
  #mapping ranges to categorical values in new column 
  mutate(difficulty = case_when(
    `Median GPA Points` >= 3.0 & `Median GPA Points` < 3.6 ~ "High",
    `Median GPA Points` >= 3.60001 & `Median GPA Points` < 3.8 ~ "Moderate",
    `Median GPA Points` >= 3.80001 & `Median GPA Points` <= 4.0 ~ "Low"
  )) %>%
  ggplot(aes(x = Department, y = `Median GPA Points`, color = difficulty))+
  geom_point()+
  #flip axis for better color 
  coord_flip()+
  
  labs(title = "Median GPA based on department from 2021 - 2022",
       color = "Course Difficulty
  (based on GPA)",
       subtitle = "High difficulty: 3.0-3.6 
Moderate difficulty: 3.61-3.8
Low difficulty: 3.81-4.0")+
  scale_color_manual(values = seq_pal) +
  Rain_theme()
  
  # theme_minimal()
ggsave("dart_courses.png", width = 10, height = 6, dpi = 300, device = "png")


# 2nd Data visualization --------------------------------------------------


dart_courses %>% 
  select(`Average Section Size`)


rain_color_palette <- c("#0072B2", "#009E73", "#F0E442", "#D55E00", "#CC79A7")

dart_courses2 <- dart_courses %>% 

  group_by(Department,`Median GPA Points`) %>% 
  #get average section size per Department and Median 
  summarise(Average_Section_Size = mean(`Average Section Size`,
                                        na.rm = TRUE,
                                        .groups = "drop")) %>% 
  #remove nas for cleaner data manipulation
  filter(!is.na(Average_Section_Size)) %>%
  filter(!is.na(`Median GPA Points`)) %>%
  filter(!is.na(Department)) %>%
  #creating new column to mapranges to each bucket
  mutate(section_size = case_when(
    Average_Section_Size >= 5 & Average_Section_Size <= 15 ~ "Bucket 1",
    Average_Section_Size > 15 & Average_Section_Size <= 20 ~ "Bucket 2",
    Average_Section_Size > 20 & Average_Section_Size <= 25 ~ "Bucket 3",
    Average_Section_Size > 25 & Average_Section_Size <= 30 ~ "Bucket 4",
    Average_Section_Size >  30 & Average_Section_Size <= 200 ~ "Bucket 5",
    TRUE ~ NA_character_
  )) %>%
  # mutate(bucket = cut(Average_Section_Size, breaks = c(10, 20, 30, 40, 50,100), 
  #                     labels = c("Bucket 1", "Bucket 2", "Bucket 3", "Bucket 4", "Bucket 5"),
  #                     include.lowest = TRUE, right = FALSE)) %>%

  ggplot(aes(x = `Median GPA Points`,
             y = Department,
             fill = section_size))+
  #ridges > boxplot... more information shared
  geom_density_ridges(quantile_lines = TRUE,
                      alpha = 0.7,
                       quantiles = 2,)+
  geom_density_ridges(jittered_points = TRUE, alpha = 0.4)+
  # scale_fill_gradient(low = "blue", high = "red") 
  
  # scale_fill_manual(values = c("Bucket 1" = "blue", "Bucket 2" = "green", 
  #                              "Bucket 3" = "yellow", "Bucket 4" = "orange", 
  #                              "Bucket 5" = "red"))+
  #use personal palette to re edit colors 
  scale_fill_manual(values = rain_color_palette)+
  labs(title = "Do larger class sizes equate to lower grades?
Median grades of different subjects based on class size",
       fill = "Section size
(number of students)",
       subtitle = "Bucket 1: 5-15, Bucket 2: 15-20, Bucket 3: 20-35, Bucket 4: 25-30, Bucket 5: 30 and above")+
  Rain_theme()+
  xlim(2.75, 4.2)

dart_courses2
  # facet_wrap(~section_size)+
  # theme_minimal()


ggsave("dart_courses2.png", width = 10, height = 6, dpi = 300, device = "png")


#Figuring out breaks/ buckets for legend  
  dart_courses %>%
    summarise(minimum_size = min(`Average Section Size`, na.rm = TRUE))
  
  dart_courses %>%
    summarise(minimum_size = max(`Average Section Size`, na.rm = TRUE))

