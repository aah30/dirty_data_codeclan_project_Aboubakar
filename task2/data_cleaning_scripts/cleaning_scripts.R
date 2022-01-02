library(here)
library(dplyr)
here::here()
library(tidyverse)

# reading data
cake <- read_csv(here("raw_data/cake/cake-ingredients-1961.csv"))


# first trying to find all column that all variable not na,
#as I notice BM column all variable is na # so I tried to check all other columns
contains_any_na <- sapply(cake, function(x) any(!is.na(x)))

# here's the columns names that at least has one value
columns_names_not_all_na <- names(cake)[contains_any_na][-1]

# now will replace all na by 0
cake <- cake %>% 
  replace(is.na(.), 0)

# only columns has at least one value will be kept
cake <- cake %>% 
  select(Cake, columns_names_not_all_na)

# now reading the table has actual code name and measures 
cake_ingr_code <- read_csv(here("raw_data/cake/cake_ingredient_code.csv"))

# here will get the actual names for those coulums code not all varible is na from ingredient code table.
actual_names <- cake_ingr_code %>% 
  filter(code %in% columns_names_not_all_na) %>% 
  select(ingredient) %>% 
  pull()
  
# will select the column code from cake table as will be used next chunk in order to replace the code by actual name
code_names <- cake %>% 
  select(columns_names_not_all_na) %>% 
  names()


# replace all abbreviations by actual ingredient names
cake <- rename_(cake, .dots = setNames(code_names
                               , actual_names )) 



# now will keep only code, ingredient and measure from cake ingredient code table, 
#that match cake table which its column not all na
cake_ingr_code <- cake_ingr_code %>% 
  filter(code %in% columns_names_not_all_na)
  
  
# writing the clean data to our folder clean data

write_csv(cake,"clean_data/clean_cake-ingredients-1961.csv")
write_csv(cake_ingr_code,"clean_data/clean_cake_ingredient_code.csv")







