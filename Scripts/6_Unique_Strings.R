##Unique OCCSTR by OCC (1880 basis for 1880 and 1850 data)


### 1. Set Up {.tabset .tabset-fade}

#### README

#### 1850

library(dplyr)
library(ggplot2)
library(treemap)
library(extrafont)
library(stringr)
loadfonts()
library(knitr)
library(kableExtra)
library(tidyr)

# Loading
setwd('scripts')
mn_1850 = readr::read_csv("../Data/census_1850_occ_mn.csv") %>% mutate(city = "Manhattan")
bk_1850 = readr::read_csv("../Data/census_1850_occ_bk.csv") %>% mutate(city = "Brooklyn")
OCC_1880 = readr::read_csv("../Data/OCC1880.csv")
OCC_1950 = readr::read_csv("../Data/OCC1950.csv")
OCC_1920 = readr::read_csv("../Data/OCC1920.csv")

# Combining Data
combined_1850 = rbind(mn_1850, bk_1850) %>% 
    select(age, sex, marst, race, labforce,
           occ, city, everything()) %>%
    left_join(OCC_1880, by = c("occ" = "code")) %>%
    mutate(race = factor(race,
                         levels = c(100,120,200,210,300),
                         labels = c("White", "Black (white)",
                                    "Black / African American",
                                    "Mulatto", "American Indian")),
           labforce = factor(labforce,
                             levels = c(0,1,2),
                             labels = c("N/A",
                                        "No, not in the labor force",
                                        "Yes, in the labor force")),
           sex = factor(sex,
                        levels = c(1,2),
                        labels = c("Male", "Female")),
           marst = factor(marst,
                          levels = c(1:6),
                          labels = c("Married, spouse present",
                                     "Married, spouse absent",
                                     "Separated",
                                     "Divorced",
                                     "Widowed",
                                     "Never married/single")))

# Theme setting for visualisations
theme_set(theme_minimal() + 
              theme(panel.grid.minor = element_blank(),
                    text = element_text(family = "Arial",
                                        colour = "black")))
col_sex = c("#e21737", "#0b648f")
col_miss = c("#86b817", "#e53238")

# Saving Memory
rm(OCC_1880)
rm(bk_1850)
rm(mn_1850)
    
    #### 1880

# Loading
mn_1880 = readr::read_csv("../Data/census_1880_occ_mn.csv") %>% mutate(city = "Manhattan")
bk_1880 = readr::read_csv("../Data/census_1880_occ_bk.csv") %>% mutate(city = "Brooklyn")
OCC_1880 = readr::read_csv("../Data/OCC1880.csv")

# Combining Data
combined_1880 = rbind(mn_1880, bk_1880) %>% 
    select(age, sex, race, labforce,
           occ, city, everything()) %>%
    left_join(OCC_1880, by = c("occ" = "code")) %>%
    mutate(age = ifelse(age == "Less than 1 year old",
                        0,
                        age),
           age = as.numeric(age))

rm(OCC_1880)
rm(bk_1880)
rm(mn_1880)
```

<br>
    
    #### 1910
    
    ```{r message = FALSE, warning = FALSE}
# Loading
mn_1910 = readr::read_csv("../Data/census_1910_occ_mn.csv") %>% mutate(city = "Manhattan")
bk_1910 = readr::read_csv("../Data/census_1910_occ_bk.csv") %>% mutate(city = "Brooklyn")
OCC_1950 = readr::read_csv("../Data/OCC1950.csv")

# Combining Data
combined_1910 = rbind(mn_1910, bk_1910) %>%
    left_join(OCC_1950, by = c("occ1950" = "code")) %>%
    mutate(sex = factor(sex,
                        levels = c(1,2),
                        labels = c("Male", "Female")),
           labor_force = factor(labor_force,
                                levels = c(0,1,2),
                                labels = c("N/A", 
                                           "No, not in the labor force",
                                           "Yes, in the labor force")))

rm(OCC_1950)
rm(mn_1910)
rm(bk_1910)


str_count_1880 <- combined_1880 %>% 
    group_by(occ) %>% 
    count(occstr)

str_count_1850 <- combined_1850 %>% 
    group_by(occ) %>% 
    count(occstr)


combined_1850 %>% group_by(label) %>% 
    summarise(count = n())
    

write.csv(str_count_1850, "occstr_occ_1850.csv")
write.csv(str_count_1880, "occstr_occ_1880.csv")



############

combined_1850 %>% 
    filter(labforce == "Yes, in the labor force") %>% 
    select(occ1950, label, occstr) %>%
    left_join(OCC_1950, by = c("occ1950" = "code")) %>% 
    select(-occ1950) %>% 
    rename(occ = label.x, occ1950 = label.y)
    
##Concatenated OCCSTR by OCC and OCC1950 code combo
test <- combined_1850 %>% 
    filter(labforce == "Yes, in the labor force") %>% 
    select(occ1950, label, occstr) %>% 
    left_join(OCC_1950, by = c("occ1950" = "code")) %>% 
    select(-occ1950) %>% 
    rename(occ = label.x, occ1950 = label.y) %>% 
    group_by(occ1950, occ) %>%
    mutate(occstr_merge = paste0(unique(occstr), collapse = ", ")) %>% 
    group_by(occ1950, occ, occstr_merge) %>% 
    summarise(count = n()) %>%
    group_by(occ1950) %>%
    mutate(occ_count = n()) %>%
    filter(occ_count > 1)

test <- combined_1880 %>% 
    filter(labforce == "Yes, in the labor force") %>% 
    select(occ1950, label, occstr) %>% 
    rename(occ = label) %>% 
    group_by(occ1950, occ) %>%
    mutate(occstr_merge = paste0(unique(occstr), collapse = ", ")) %>% 
    group_by(occ1950, occ, occstr_merge) %>% 
    summarise(count = n()) %>%
    group_by(occ1950) %>%
    mutate(occ_count = n()) %>%
    filter(occ_count > 1)

test <- combined_1910 %>% 
    filter(labor_force == "Yes, in the labor force") %>% 
    group_by(occ1950) %>%
    mutate(occstr_merge = paste0(unique(occstr), collapse = ", ")) %>% 
    count(occstr_merge) %>% 
    left_join(OCC_1950, by = c("occ1950" = "code")) %>% 
    ungroup() %>% 
    select(-occ1950) %>% 
    rename(occ1950 = label)
    
 test_str <- combined_1850 %>% 
     filter(labforce == "Yes, in the labor force") %>% 
     select(occ1950, label, occstr) %>% 
     left_join(OCC_1950, by = c("occ1950" = "code")) %>% 
     select(-occ1950) %>% 
     rename(occ = label.x, occ1950 = label.y) %>% 
     group_by(occstr) %>% 
     mutate(occ_merge = paste0(unique(occ), collapse = "; ")) %>% 
     mutate(occ1950_merge = paste0(unique(occ1950), collapse = "; ")) %>%
     mutate(n_occ = n_distinct(occ)) %>% 
     mutate(n_occ1950 = n_distinct(occ1950)) %>% 
     group_by(occ_merge, occ1950_merge, occstr, n_occ1950, n_occ) %>% 
     summarise(count = n())
     
 test_str <- combined_1880 %>% 
     filter(labforce == "Yes, in the labor force") %>% 
     select(occ1950, label, occstr) %>% 
     rename(occ = label) %>% 
     group_by(occstr) %>% 
     mutate(occ_merge = paste0(unique(occ), collapse = "; ")) %>% 
     mutate(occ1950_merge = paste0(unique(occ1950), collapse = "; ")) %>%
     mutate(n_occ = n_distinct(occ)) %>% 
     mutate(n_occ1950 = n_distinct(occ1950)) %>% 
     group_by(occ_merge, occ1950_merge, occstr, n_occ1950, n_occ) %>% 
     summarise(count = n())  
         
         
test_str <- combined_1910 %>% 
    filter(labor_force == "Yes, in the labor force") %>%
    select(label, occstr) %>% 
    rename(occ1950 = label) %>% 
    group_by(occstr) %>% 
    mutate(occ1950_merge = paste0(unique(occ1950), collapse = "; ")) %>%
    mutate(n_occ1950 = n_distinct(occ1950)) %>% 
    group_by(occ1950_merge, occstr, n_occ1950) %>% 
    summarise(count = n())

#role of industry codes
##initial sense is that they impact the coding... can test out with categories like laborer... but know that they are aleady on a 1950s basis and not avail for all years
#explore blank / non occupational response in 1880, 1850 (not such a large issue in 1850)

##filter blank in 1850 and 1880 and explore occstrs over different codes in occ and occ1950
##filter not yet classified in 1910 '' ''

test_blank <- combined_1850 %>% 
    filter(labforce == "Yes, in the labor force") %>% 
    select(occ1950, label, occstr) %>% 
    left_join(OCC_1950, by = c("occ1950" = "code")) %>% 
    select(-occ1950) %>% 
    rename(occ = label.x, occ1950 = label.y) %>% 
    filter(occ == "Blank") %>% 
    group_by(occ1950, occstr) %>% 
    summarise(count = n())

test_blank <- combined_1850 %>% 
    filter(labforce == "Yes, in the labor force") %>% 
    select(occ1950, label, occstr) %>% 
    left_join(OCC_1950, by = c("occ1950" = "code")) %>% 
    select(-occ1950) %>% 
    rename(occ = label.x, occ1950 = label.y) %>% 
    filter(occ1950 == "Not yet classified") %>% 
    group_by(occ1950, occstr) %>% 
    summarise(count = n())

#1880 blank
test_blank <- combined_1880 %>% 
    filter(labforce == "Yes, in the labor force") %>% 
    select(occ1950, label, occstr) %>% 
    rename(occ = label) %>% 
    filter(occ == "Blank") %>% 
    group_by(occ1950, occstr, occ) %>%
    summarise(count = n())
#1880 not yet classified
test_blank <- combined_1880 %>% 
    filter(labforce == "Yes, in the labor force") %>% 
    select(occ1950, label, occstr) %>% 
    rename(occ = label) %>% 
    filter(occ1950 == "Not yet classified") %>% 
    group_by(occ1950, occstr, occ) %>%
    summarise(count = n())
# 1910 not yet classified
test_blank <- combined_1910 %>% 
    filter(labor_force == "Yes, in the labor force") %>% 
    select(label, occstr) %>% 
    rename(occ1950 = label) %>% 
    filter(occ1950 == "Not yet classified") %>% 
    group_by(occ1950, occstr) %>% 
    summarise(count = n())

#Preparing materials to join
join_1910 <-  combined_1910 %>% 
    select(occ1950, occstr, label) %>% 
    group_by(occ1950, occstr, label) %>% 
    summarise(count = n())
    
#Loading joins on OCCSTR for recoding OCC
#matched
#1880 first (eventually merge all of the joined tables)
#shows the occ1950 categories that the strings are coded into
#many multiple matches, pointing to the signifiance of intersecting these joins with other fields (industry)
matched_1910_occ1880 <- readr::read_csv("../Data/matching_1910/Census1910_matched-occ-codes-from-1880.csv") %>% 
    left_join(join_1910, by = c("occstr" = "occstr"))

#missing
#1880 first (eventually merge all of the joined tables)
missing_1910_occ1880 <- readr::read_csv("../Data/matching_1910/Census1910_missing-occ-codes-in-1880.csv") %>% 
    left_join(join_1910, by = c("Occupation (string)" = "occstr"))
              