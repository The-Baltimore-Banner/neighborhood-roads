---
title: The Baltimore Banner's analysis of Baltimore vehicle crashes by street
  and neighborhood
author: "Ryan Little"
date: '2022-06-30'
output: html_document
---

```{r setup, include=TRUE}
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(readr)
library(janitor)
library(lubridate)
library(tidycensus)
library(readxl)
library(corrr)
library(sqldf)
library(scales)
library(psych)
options(digits=3)
options(scipen=999)
'%notin%' <- Negate('%in%')


#does a standard group_by and count() with percentage
grouper <- function(input_df, group_by_column, new_column_name = "n()"){
  output_df <- input_df %>%
    group_by(.data[[group_by_column]]) %>%
    summarise(temp_count = n()) %>%
    mutate(percent = temp_count/sum(temp_count)*100) %>%
    arrange(desc(percent)) %>%
    rename(!!new_column_name := temp_count)
  return(output_df)
}

#group/counts every column in input dataframe
group_count <- function(input_df, group_column_name='n()', state_filter=NA, start_col = 1){
  column_names <- colnames(input_df)
  if(!is.na(state_filter)){
    input_df <- input_df %>%
      filter(state == state_filter)
  }
  for (column in column_names[start_col:length(column_names)]){
    output <- grouper(input_df, column, group_column_name)
    print(output)
  }
}

#function for calculating age, updates to the day; stolen from the internet
calc_age <- function(birthDate, refDate = Sys.Date()) {
    require(lubridate)
    period <- as.period(interval(birthDate, refDate),
                        unit = "year")
    period$year
}

#lowers case of every character column in a dataframe
lower_df <- function(input_df){
  names <- colnames(input_df)
  output_df <- input_df
  names <- colnames(output_df)
  for (name in names){
    if (is.character(output_df[[name]])){
      output_df[[name]] <- tolower(output_df[[name]])
      #print('yes')
    } else {
      output_df[[name]] <- output_df[[name]]
      #print('no')
    }
  }
  return(output_df)
}

#imports every file in a folder
import_files_in_folder <- function(input_path, output_name=''){
  files <- list.files(path=input_path)
  for (file in files){you
    file_path = paste0(input_path, file)
    file_sans_csv = str_remove(file, '.csv')
    file_sans_csv = gsub("-", "_", file_sans_csv)
    imported_file <- read_csv(file_path)
    assign(paste0(file_sans_csv, output_name), imported_file, envir = parent.frame())
  }
}

import_bind_files_in_folder <- function(input_path, output_name=''){
  files <- list.files(path=input_path)
  master <- read_csv(paste0(input_path, files[1]))
  for (file in files[2:length(files)]){
    if (str_sub(file, -3) == 'csv'){
      binder <- read_csv(paste0(input_path, file))
      master <- master %>%
        rbind(binder)      
    } else {
      #pass
    }
  }
  return(master)
}

grouper_sum <- function(input_df, group_by_column, sum_column, new_column_name = "n()"){
  output_df <- input_df %>%
    group_by(.data[[group_by_column]]) %>%
    summarise(temp_count = sum(.data[[sum_column]])) %>%
    mutate(percent = temp_count/sum(temp_count)*100) %>%
    arrange(desc(percent)) %>%
    rename(!!new_column_name := temp_count)
  return(output_df)
  
}

```

## Overview

This is the code The Baltimore Banner used in R to analyze the [Maryland Statewide Vehicle Crashes](https://opendata.maryland.gov/Public-Safety/Maryland-Statewide-Vehicle-Crashes/65du-s3qu). This analysis found that Orleans Street in the Baltimore neighborhoods of Dunbar-Broadway and CARE have some of the highest rates of vehicle crashes per resident of any of the more residential neighborhoods in the city since 2015, the first year the state makes data available for. The analysis is limited by deficiencies in the data that impact about 1 in every 5 crashes. Residents have raised concerns about crashes there for years and want the city to implement traffic calming measures. Read our story here: [‘No one feels safe’: residents push for traffic calming on Orleans street](https://www.thebaltimorebanner.com/baltimore/east-baltimore/no-one-feels-safe-residents-push-for-traffic-calming-on-orleans-street-WRFSFNSZJREZTNSERUKTV3SE3M/).

## Methodology

The Baltimore Banner determined neighborhood location using a geospatial join of the Maryland Statewide Vehicle Crashes database with [Baltimore's neighborhoods database](https://data.baltimorecity.gov/datasets/neighborhoods/explore). The crashes database was filtered to only include Baltimore City. Street names were cleaned using a Python address parser call [usadress](https://pypi.org/project/usaddress/) and [OpenRefine](https://openrefine.org/). The address parser broke street names from the state database into its parts, isolating street name and type (i.e. rd, st, ave). Misspellings and similar abbreviations were repaired using key collision algorithms in OpenRefine. Road designations are from the [Baltimore Department of Transportation's Road Classifications](https://baltimore-department-of-transportation-baltimoredot.hub.arcgis.com/maps/5ef5d5c249004f098fc39dead71b8055/explore?location=39.282291%2C-76.595400%2C11.43). Decennial U.S. Census data is from the [BigLocalNews/AP 2020 Census Co-op](https://twitter.com/BigLocalNews/status/1430575708800274435).

The Baltimore Banner examined relationships between neighborhoods and roads. The lowest population quartile of neighborhood and road combinations was excluded from analysis. For simplicity, rates were calculated for each year against the 2020 population. 

```{r pressure, echo=FALSE}

#Import 2020 Census from BigLocalNews AP partnership
census <- read_csv("data/prop_change_by_neighborhood.csv") %>%
  lower_df() %>%
  select(neighborhood, x2020_pop)

#Import Baltimore DOT road classifications
road_classifications <- read_csv("data/baltimore-road-classifications-clean.csv") %>%
  clean_names() %>%
  lower_df() %>%
  ungroup() %>%
  mutate(clean_name = str_replace(fullname, paste0("", dirpre, " "), '')) %>%
  mutate(clean_name = str_replace(clean_name, paste0(" ", dirsur, ""), '')) %>%
  mutate(clean_name = str_replace(clean_name, "i ", "i")) %>%
  mutate(clean_name = trimws(clean_name, which = c("both")))

#creating list of principal arteries for filtering
principal_arteries <- road_classifications %>%
  filter(!is.na(fullname) & sha_class == "part") %>%
  select(clean_name, sha_class) %>%
  unique()

#import accidents output from QGIS
accidents<- read_csv("data/baltimore-city-accidents-by-neighborhood.csv") %>%
  clean_names() %>%
  lower_df() %>%
  mutate(acc_date = ymd(acc_date)) %>%
  rename(neighborhood = name)

#write output to clean in OpenRefine
#temp <- accidents%>%
#  select(report_no, mainroad_name)

#write_csv(temp, "output/mainroad_names_for_parsing.csv")

#import cleaned road names
refined_addresses <- read_csv("data/street-name-parsed-refined-with-types.csv") %>%
  select(-mainroad_name)

#join refined names
accidents<- accidents%>%
  left_join(refined_addresses) %>%
  #Replaced all '0' with 'o' during address parsing to repair common error and make key collision refinement more effective, removing here
  mutate(clean_name = case_when(
    str_detect(combined, '([:digit:])(\\o)') == TRUE ~ str_replace_all(combined, 'o', '0'),
    TRUE ~ combined
  ))

#importing all Baltimore accidents even if they do not have a geospatial neighborhood join
full_accidents <- read_csv("data/baltimore-city-accidents.csv")

```

### Raw Crashes by Neighborhood

```{r}

years <- c(2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022)

for (input_year in years){
  
  print(grouper(accidents%>% filter(year == input_year), "neighborhood"))
    
}

#The downtown neighborhood always has the most accidents.

```

### Crashes Per 100k

```{r}


#Looking for more accidents in each road in each Baltimore neighborhood
grouped_accidents <- accidents%>%
  #excluding roads where a clean name does not exist
  filter(!is.na(clean_name)) %>%
  group_by(clean_name, neighborhood) %>%
  summarise(accidents = n()) %>%
  clean_names() %>%
  left_join(census) %>%
  mutate(accidents = as.double(accidents)) %>%
  mutate(accidents_per_1k = (accidents/(x2020_pop/1000))) %>%
  ungroup() %>%
  mutate(part = case_when(
    clean_name %in% principal_arteries$clean_name ~ TRUE,
    TRUE ~ FALSE
  ))


few_accidents <- grouped_accidents %>%
  filter(accidents < 5)
  

  
  #removing accidents that list a street name but have locations that are due to bad location information
grouped_accidents <- grouped_accidents %>%
  filter(accidents > 5)

quantile = quantile(grouped_accidents$x2020_pop, na.rm = TRUE)[2]
quantile

```

```{r}

mutated_grouped_accidents <- grouped_accidents %>%
  #filtering for top 75%
  #this is the quantile from line 227
  filter(x2020_pop >= quantile) %>%
  mutate(rank_raw = order(order(accidents, decreasing = TRUE))) %>%
  mutate(rank_per_1k = rank(desc(accidents_per_1k))) %>%
  arrange(rank_per_1k)

top_10_accidents_per_1k <- mutated_grouped_accidents  %>%
  slice(1:10)%>%
  #arrange(neighborhood)
  arrange(desc(accidents_per_1k))

#write_csv(top_10_accidents_per_1k, 'output/top-10-accidents-per-1k.csv')

top_10_accidents_per_1k


```




```{r}

for (input_year in years){
  
  grouped_accidents <- accidents%>%
    filter(!is.na(clean_name),
           year == input_year) %>%
    group_by(clean_name, neighborhood) %>%
    summarise(accidents = n()) %>%
    clean_names() %>%
    left_join(census) %>%
    mutate(accidents = as.double(accidents)) %>%
    mutate(accidents_per_1k = (accidents/(x2020_pop/1000))) %>%
    ungroup() %>%
    #test <- grouped_accidents %>%
    mutate(part = case_when(
      clean_name %in% principal_arteries$clean_name ~ TRUE,
      TRUE ~ FALSE
    ))
  
  mutated_grouped_accidents <- grouped_accidents %>%
    #filtering for top 75%
      #this is the quantile from line 227
    filter(x2020_pop >= quantile) %>%
    mutate(rank_raw = order(order(accidents, decreasing = TRUE))) %>%
    mutate(rank_per_1k = rank(desc(accidents_per_1k))) %>%
    arrange(rank_per_1k)
  
  output <- mutated_grouped_accidents %>%
    slice(1:10)%>%
    #arrange(neighborhood)
    arrange(desc(accidents_per_1k)) %>%
    rename(!!as.character(input_year) := clean_name)
  
  print(output)

}

#Orleans St in Dunbar-Broadway and CARE have been in the top ten most accidents per resident In some years, CARE or Dunbar-Broadway have had the most accidents per resident. Orleans St in the CARE neighborhood has had the most in any year in 2017 and 2020. Dunbar-Broadway had the most per resident of any street in any neighborhood with 40 per 1,000 residents in 2015 and 2016. It is the current leader in 2022.

```

## Errors and limitations

The Maryland Statewide Vehicle Crashes database has a number of flaws that impact this analysis. In some cases, crashes list geospatial locations that mark streets that do not match the listed street name. Some rows do not include street names and/or geospatial locations. Some crashes list street names, cities our even counties that do not match their geospatial location. These limitations excluded 26,219 crashes from the analysis in Baltimore — 19% of all crashes. 

```{r}

#Rows without clean street names
nrow(accidents %>% filter(is.na(clean_name)))

```

```{r}

#Calculating difference between raw Baltimore crashes and those that were included in the neighborhood geospatial join.
nrow(full_accidents)-nrow(accidents)

```

```{R}

#Calculating percent of crashes that did not have both clean street name and a corresponding geospatial location.
(26219/nrow(full_accidents)*100)

rm(full_accidents)
```
Some corresponding locations match with neighborhoods that have very few crashes. In some cases, these are legitmate matches on smaller roads that have only a small overlap with a street. In other cases, there may be instances where a geospatial location is located in a neighborhood that does not actually contain that road. We are excluding these instances from the data. An analysis of these low pairings suggest that they could manipulate the totals, but appear to impact the top 10 at similar rates. Attributing all of these low pairings to any given pairing only changes the order in two places. 

```{r}

#these streets have neighborhood pairings that are less than 5. 
low_neighborhood_street_pairing_counts_for_top_10 <- few_accidents %>%
  group_by(clean_name) %>%
  summarise(accidents = sum(accidents)) %>%
  arrange(desc(accidents)) %>%
  filter(clean_name %in% top_10_accidents_per_1k$clean_name)

temp <- top_10_accidents_per_1k %>% select(clean_name, accidents, accidents_per_1k, x2020_pop) %>% rename(counted_accidents = accidents)

low_neighborhood_street_pairing_counts_for_top_10 %>%
  rename(possibly_missing_accidents = accidents) %>%
  left_join(temp, by = c("clean_name" = "clean_name")) %>%
  mutate(possible_accidents_per_1k = ((counted_accidents+possibly_missing_accidents)/(x2020_pop/1000))) %>%
  mutate(difference_from_accidents_per_1k = possible_accidents_per_1k - accidents_per_1k) %>%
  select(-x2020_pop) %>%
  arrange(desc(possible_accidents_per_1k))

rm(temp)

```

```{r}

# Number of accidents that may be missing from the calculation of the top 10 neighborhood/street pairings.
sum(low_neighborhood_street_pairing_counts_for_top_10$accidents)


```



## License

Copyright 2022, The Baltimore Banner and The Ventoulis Institute for Local Journalism

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.