
library(here)
library(dplyr)
library(janitor)
here::here()
library(tidyverse)
library("readxl")
library(naniar)

candy_2015 <- read_excel((here("raw_data/candy_ranking_data/boing-boing-candy-2015.xlsx")))

candy_2016 <- read_excel((here("raw_data/candy_ranking_data/boing-boing-candy-2016.xlsx")))

candy_2017 <- read_excel((here("raw_data/candy_ranking_data/boing-boing-candy-2017.xlsx")))

candy_2015 %>% 
  names()

candy_2015 <- candy_2015 %>% 
  clean_names()

# renaming column to match all tables 
candy_2015 <- candy_2015 %>%
  rename(age = how_old_are_you,
         going_trick_or_treating = are_you_going_actually_going_trick_or_treating_yourself)


# here tring to do pivot longer to have two column candy and ratings
candy_2015 <- candy_2015 %>% 
  pivot_longer(c("butterfinger":"york_peppermint_patties","necco_wafers","sea_salt_flavored_stuff_probably_chocolate_since_this_is_the_it_flavor_of_the_year"), 
               names_to = "candy",
               values_to = "ratings") 


# select only column we need
candy_2015 <- candy_2015 %>% 
  select(going_trick_or_treating,
         age,
         candy,
         ratings)


# adding to column gender, year and country as missing in this table to match the no of coulmn in all tables
# to do bind_rows in the end
candy_2015 <- candy_2015 %>% 
  mutate(country = "NA", .after = "age") %>% 
  mutate(gender = "NA", .before = "age", year = "2015") 


# now cleanning table 2016 
candy_2016 %>% 
  names()

candy_2016 <- candy_2016 %>% 
  clean_names()

candy_2016 <- candy_2016 %>% 
  pivot_longer(c("x100_grand_bar":"york_peppermint_patties"), 
               names_to = "candy",
               values_to = "ratings") 

candy_2016 <- candy_2016 %>%
  rename(age = how_old_are_you,
         going_trick_or_treating = are_you_going_actually_going_trick_or_treating_yourself,
        country = which_country_do_you_live_in,
        gender = your_gender)

# I add year column in each table to be easy to do analysis in rmd file
candy_2016 <- candy_2016 %>% 
  mutate(year = "2016") 

candy_2016 <- candy_2016 %>% 
  select(going_trick_or_treating,
         gender,
         year,
         age,
         country,
         candy,
         ratings)


# pattern for cleanning 2017 candy data names
pattern1 <- "Q+[0-9]:"
pattern2 <- "Q+[0-9][0-9]:"
pattern3 <- "Q+[0-9][' '][|]"

 names(candy_2017) = gsub(pattern1, replacement = "", x = names(candy_2017)) 
names(candy_2017) = gsub(pattern2, replacement = "", x = names(candy_2017)) 
names(candy_2017) = gsub(pattern3, replacement = "", x = names(candy_2017)) 


candy_2017 <- candy_2017 %>% 
  clean_names()

candy_2017 <- candy_2017 %>% 
  pivot_longer(c("x100_grand_bar":"york_peppermint_patties"), 
               names_to = "candy",
               values_to = "ratings") 

candy_2017 <- candy_2017 %>% 
  rename(going_trick_or_treating = going_out)

# I add year column in each table to be easy to do analysis in rmd file
candy_2017 <- candy_2017 %>% 
  mutate(year = "2017") 

candy_2017 <- candy_2017 %>% 
  select(going_trick_or_treating,
         gender,
         year,
         age,
         country,
         candy,
         ratings)


# binding rows for the 3 tables
clean_candy <- bind_rows(candy_2015, candy_2016,candy_2017)


# to check before and after how country column look like
clean_candy %>% 
  select(country) %>% 
   group_by(country) %>% 
  summarise(no = n())

# pattern to clean country column
uk_pattern <- c("endland|england|england|scotland|u.k.|uk|united kindom|united kingdom")

canada_pattern <- c("can|canada|canada|canada`")

usa_pattern <- c("usausausa|usas|usaa|usa? hard to tell anymore..|usa!!!!!!|usa! usa!|usa!|usa usa usa!!!!
                 |usa usa usa usa|usa usa usa|usa (i think but it's an election year so who can really tell)
                 |usa|usa|units states|united ststes|united      ststes|usa|us|usa|united states of america
                 |us|united states|united states|us|u.s.|u.s.a.|united states of america|america|america|u.s.|
                 murica|unites states|us of a|united state|usa! usa! usa!|merica|the united states|united sates|
                 united stated|ussa|'merica|ahem....amerca|n. america|the best one - usa|the united states of america
                 |the yoo ess of aaayyyyyy|u s|u s a|u.s.|u.s.a.|unied states|unhinged states|unied states|unite states
                 |united  states of america|united staes|united statea|united states|united states|united statss|united stetes
                 |	united ststes|united ststes|units states|usa|usa|usa (i think but it's an election year so who can really tell)
                 |usa usa usa|usa usa usa usa|usa usa usa!!!!|usa!|usa! usa!|usa!!!!!!|usa? hard to tell anymore..|usaa|usas|usausausa")

 clean_candy <- clean_candy %>%
  mutate(country = case_when(
    str_detect(country, "not[\\s]{1,}") ~ NA_character_,
    str_detect(country, "australia") ~ "other",
    str_detect(country, "austria") ~ "other",
    str_detect(country, "soviet canuckistan") ~ "other",
    str_detect(country, "not the usa or canada") ~ "other", 
    str_detect(country, str_c(uk_pattern, collapse = "|")) ~ "uk",
    str_detect(country, str_c(usa_pattern, collapse = "|")) ~ "usa",
    str_detect(country, str_c(canada_pattern, collapse = "|")) ~ "canada",
    is.na(country) == TRUE ~ NA_character_,
    TRUE ~ "other")
  )
 
 #writting clean data to folder
 write_csv(clean_candy,"clean_data/clean_candy_15_16_17.csv")
 




