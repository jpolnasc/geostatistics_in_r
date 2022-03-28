---
title: "Data Cleaning - Climate Normals : 1981-2010"
knit: (function(input_file, encoding) {
  out_dir <- '../../../docs';
  rmarkdown::render(input_file,
  encoding=encoding,
  output_file=file.path(dirname(input_file), out_dir, paste0("data_cleaning",'.html') ))})
output:
  html_document:
    keep_md: yes
    toc: yes
    toc_depth: 2
    toc_float: yes
    fig_caption: yes
---



# Introduction

## Climate Normals:

" Climatological normal or climate normal (CN) is a 30-year average of a weather variable for a given time of year.[1] Most commonly, a CN refers to a particular month of year, but it may also refer to a broader scale, such as a specific meteorological season.[2] More recently, CN have been reported for narrower scales, such as day of year and even hourly scale.[3]

Climatological normals are used as an average or baseline to evaluate climate events and provide context for year-to-year variability. Normals can be calculated for a variety of weather variables including temperature and precipitation and rely on data from weather stations. " 

source: https://en.wikipedia.org/wiki/Climatological_normal

## Source of data used in this project:

https://portal.inmet.gov.br/normais

# Data Cleaning Process





Checking if all the file are in the folder



```r
path = '../../data/raw/climate_normals/'
list.files(path)
```

```
## [1] "30-Precipitação-Acumulada-NCB_1981-2010.xls"
## [2] "Estações-Normal-Climatoógica-1981-2010.xls" 
## [3] "Normal-Climatologica-UR.xlsx"
```

We are going to read the excel file and turn it into a dataframe. 
I translated the name of the columns to make it a little more intuitive for those who don't speak Portuguese =)


```r
stations = read_excel(paste0(path,"Estações-Normal-Climatoógica-1981-2010.xls"), skip = 3) %>% clean_names() %>%
  rename("height" = "atitude",
         "number" = "no",
         "code" = "codigo",
         "station_name" = "nome_da_estacao",
         "start_of_operation" = "inicio_operacao",
         "end_of_operation" = "fim_operacao",
         "deactivated" = "situacao") %>%
  mutate(deactivated = as.factor (ifelse(is.na(deactivated), 0,1))) %>%
  select(- c(start_of_operation, end_of_operation))
```


Some of the stations are deactivated, but that's not a big deal since we are only going to use this data to get the their locations and do a left join with our variables of interest.



```r
stations %>%
  ggplot() +
  geom_point(aes(x = longitude, y = latitude, colour = deactivated))
```

![](/Users/jpolnasc/projetos/geostatistics_in_r/kriging_and_cokriging_climate_normals_brazil/notebooks/data_cleaning/../../../docs/data_cleaning_files/figure-html/unnamed-chunk-4-1.png)<!-- -->


## Precipitation

### Monthly/year accumulated


Let's do some translation and force the variables to be numeric. Some of them came as text due to Excel issues.



```r
normals = read_excel(paste0(path,"30-Precipitação-Acumulada-NCB_1981-2010.xls"), skip = 3) %>% 
  clean_names() %>%
  rename("code" = "codigo",
         "name_of_the_station" = "nome_da_estacao",
         "january" = "janeiro",
         "february" = "fevereiro",
         "march" = "marco",
         "april" = "abril",
         "may" = "maio",
         "june" = "junho",
         "july" = "julho",
         "august" = "agosto",
         "september" = "setembro",
         "october" = "outubro",
         "november" = "novembro",
         "december" = "dezembro",
         "year" = "ano") %>%
  mutate(january = as.numeric(january),
         february = as.numeric(february),
         march = as.numeric(march),
         april = as.numeric(april),
         may = as.numeric(may),
         june = as.numeric(june),
         july = as.numeric(july),
         august = as.numeric(august),
         september = as.numeric(september),
         october = as.numeric(october),
         november = as.numeric(november),
         december = as.numeric(december),
         year = as.numeric(year))
head(normals,3)
```

```
## # A tibble: 3 × 16
##    code name_of_the_station uf    january february march april   may  june  july
##   <dbl> <chr>               <chr>   <dbl>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
## 1 82704 CRUZEIRO DO SUL     AC       247      258.  293.  233. 156.   89.7  59.1
## 2 82915 RIO BRANCO          AC       294.     298.  278.  207.  87.7  32.8  31.9
## 3 82807 TARAUACA            AC       282.     258.  327.  201. 136.   60.9  48.7
## # … with 6 more variables: august <dbl>, september <dbl>, october <dbl>,
## #   november <dbl>, december <dbl>, year <dbl>
```


Now we are going to merge the location of the stations with the observed amounts of precipitation.



```r
df = left_join(normals,
          stations %>% as.data.frame() %>% dplyr::select(code, longitude, latitude), 
          by = 'code')

head(df,3)
```

```
## # A tibble: 3 × 18
##    code name_of_the_station uf    january february march april   may  june  july
##   <dbl> <chr>               <chr>   <dbl>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
## 1 82704 CRUZEIRO DO SUL     AC       247      258.  293.  233. 156.   89.7  59.1
## 2 82915 RIO BRANCO          AC       294.     298.  278.  207.  87.7  32.8  31.9
## 3 82807 TARAUACA            AC       282.     258.  327.  201. 136.   60.9  48.7
## # … with 8 more variables: august <dbl>, september <dbl>, october <dbl>,
## #   november <dbl>, december <dbl>, year <dbl>, longitude <dbl>, latitude <dbl>
```


We have a low percentage of missing data, which is not going to get in our way.



```r
df %>% check_nan_perc()
```

```
##   code name_of_the_station uf january    february march april may        june
## 1    0                   0  0       0 0.003076923     0     0   0 0.003076923
##          july august   september october november    december       year
## 1 0.006153846      0 0.003076923       0        0 0.003076923 0.01538462
##     longitude    latitude
## 1 0.003076923 0.003076923
```

And this is the spatial distribution of precipitation expected for a year in Brazil.



```r
  df %>% ggplot() +
  geom_point(aes(x = longitude, y = latitude, colour = year))
```

![](/Users/jpolnasc/projetos/geostatistics_in_r/kriging_and_cokriging_climate_normals_brazil/notebooks/data_cleaning/../../../docs/data_cleaning_files/figure-html/unnamed-chunk-8-1.png)<!-- -->


Finally, let's persist the data in the "cleansed" folder



```r
path_out = '../../data/cleansed/climate_normals/precipitation.csv'

write_csv(df, path_out)
```


## Relative humidity

### Monthly/year

Now let's do the same for relative humidity.
We could just copy and paste the code we used before, but let's not repeat ourselves. We might want to do this for all climate normals available in the future, in which case a function might be useful

This is the general structure of the Excel files for climate normals. We need to figure out how many lines to skip everytime we open a new climate therefore this part won't be turned into a function. 
Let's build a "fix columns" function:

It searches for the names in Portuguese, translates translates to English and transforms the columns we need to numeric format.

First let's build a pseudo-dictionary, as base R does not support dictionaries.



```r
col_names_dict <- function(column_name) {
  
  "This function takes a string as specified bellow and returns the corresponding name in english"
  
  dic = data.frame(row.names=c("codigo",
                         "nome_da_estacao",
                         "janeiro",
                         "fevereiro",
                         "marco", 
                         "abril", 
                         "maio", 
                         "junho", 
                         "julho", 
                         "agosto",
                         "setembro", 
                         "outubro",
                         "novembro",
                         "dezembro",
                         "ano") , val=c("code",
                                         "name_of_the_station",
                                         "january",
                                         "february",
                                         "march",
                                         "april",
                                         "may",
                                         "june",
                                         "july",
                                         "august",
                                         "september",
                                         "october",
                                         "november",
                                         "december",
                                         "year"))
  
  return (dic[column_name,])
  
}

col_names_dict(c("marco", "abril"))
```

```
## [1] "march" "april"
```


Now we take the dictionary and use the vectorized ifelse function to change the names. We are actually making a copy of the dataframe and this can be problematic for large datasets.
We're doing things this way to try to keep the functions as pure as possible.


```r
translate_col_names <- function(dataframe, names_dictionary) {
  
  dataframe_out = dataframe #copy the dataframe so we can preserve the original in the main scope of the notebook
  
  original_columns_names = colnames(dataframe_out)

  translated_columns = col_names_dict(original_columns_names)

  colnames(dataframe_out) = ifelse( is.na(translated_columns) != TRUE, translated_columns, original_columns_names)
  
  return(dataframe_out)
  
}

normals = read_excel(paste0(path,"Normal-Climatologica-UR.xlsx"), skip = 2) %>% 
  clean_names()

normals_t = normals %>% translate_col_names(col_names_dict(normals))


colnames(normals)
```

```
##  [1] "codigo"          "nome_da_estacao" "uf"              "janeiro"        
##  [5] "fevereiro"       "marco"           "abril"           "maio"           
##  [9] "junho"           "julho"           "agosto"          "setembro"       
## [13] "outubro"         "novembro"        "dezembro"        "ano"
```

```r
colnames(normals_t)
```

```
##  [1] "code"                "name_of_the_station" "uf"                 
##  [4] "january"             "february"            "march"              
##  [7] "april"               "may"                 "june"               
## [10] "july"                "august"              "september"          
## [13] "october"             "november"            "december"           
## [16] "year"
```


Now we need to change some columns from character to numeric. We can use the "apply" function to do this. There will be warnings about the introduction of NAs, but this is just the result of missing data. We will silence these messages for the rest of this notebook.



```r
columns_to_numeric <- function(dataframe) {
  
  dataframe_out = dataframe 
  
  columns_to_change = c( "january",
                       "february",
                       "march",
                       "april",
                       "may",
                       "june",
                       "july",
                       "august",
                       "september",
                       "october",
                       "november",
                       "december",
                       "year" )
  
  subset_dataframe = dataframe_out[ columns_to_change ]
  
  subset_dataframe = apply(subset_dataframe, MARGIN = 2, as.numeric)
  
  dataframe_out[ colnames(subset_dataframe) ] = subset_dataframe
  
  return (dataframe_out)
  
}

normals_t %>% columns_to_numeric %>% head(2)
```

```
## Warning in apply(subset_dataframe, MARGIN = 2, as.numeric): NAs introduzidos por
## coerção

## Warning in apply(subset_dataframe, MARGIN = 2, as.numeric): NAs introduzidos por
## coerção

## Warning in apply(subset_dataframe, MARGIN = 2, as.numeric): NAs introduzidos por
## coerção

## Warning in apply(subset_dataframe, MARGIN = 2, as.numeric): NAs introduzidos por
## coerção

## Warning in apply(subset_dataframe, MARGIN = 2, as.numeric): NAs introduzidos por
## coerção

## Warning in apply(subset_dataframe, MARGIN = 2, as.numeric): NAs introduzidos por
## coerção

## Warning in apply(subset_dataframe, MARGIN = 2, as.numeric): NAs introduzidos por
## coerção

## Warning in apply(subset_dataframe, MARGIN = 2, as.numeric): NAs introduzidos por
## coerção

## Warning in apply(subset_dataframe, MARGIN = 2, as.numeric): NAs introduzidos por
## coerção

## Warning in apply(subset_dataframe, MARGIN = 2, as.numeric): NAs introduzidos por
## coerção

## Warning in apply(subset_dataframe, MARGIN = 2, as.numeric): NAs introduzidos por
## coerção

## Warning in apply(subset_dataframe, MARGIN = 2, as.numeric): NAs introduzidos por
## coerção
```

```
## # A tibble: 2 × 16
##    code name_of_the_station uf    january february march april   may  june  july
##   <dbl> <chr>               <chr>   <dbl>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
## 1 83249 ALAGOINHAS          BA       73.6     75.8  75.5  81.4  85.5  87.1  85.9
## 2 82353 ALTAMIRA            PA       83.6     85.4  85.9  85.6  84.1  80.6  NA  
## # … with 6 more variables: august <dbl>, september <dbl>, october <dbl>,
## #   november <dbl>, december <dbl>, year <dbl>
```


Now we encapsulate everything as a "fix_columns" function



```r
fix_columns <- function(dataframe) {
  
  dataframe_out = dataframe
  
  dataframe_out_translated = dataframe_out %>% translate_col_names(col_names_dict(dataframe_out))
  
  return (dataframe_out_translated %>% columns_to_numeric)
  
}


normals_fixed = normals %>% fix_columns()

normals %>% head(2)
```

```
## # A tibble: 2 × 16
##   codigo nome_da_estacao uf    janeiro  fevereiro marco abril  maio  junho julho
##    <dbl> <chr>           <chr> <chr>    <chr>     <chr> <chr>  <chr> <chr> <chr>
## 1  83249 ALAGOINHAS      BA    73.5999… 75.8      75.5  81.40… 85.5  87.1  85.9 
## 2  82353 ALTAMIRA        PA    83.6     85.4      85.9  85.6   84.1  80.5… -    
## # … with 6 more variables: agosto <chr>, setembro <chr>, outubro <dbl>,
## #   novembro <chr>, dezembro <chr>, ano <chr>
```

```r
normals_fixed %>% head(2)
```

```
## # A tibble: 2 × 16
##    code name_of_the_station uf    january february march april   may  june  july
##   <dbl> <chr>               <chr>   <dbl>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
## 1 83249 ALAGOINHAS          BA       73.6     75.8  75.5  81.4  85.5  87.1  85.9
## 2 82353 ALTAMIRA            PA       83.6     85.4  85.9  85.6  84.1  80.6  NA  
## # … with 6 more variables: august <dbl>, september <dbl>, october <dbl>,
## #   november <dbl>, december <dbl>, year <dbl>
```


Now we can do the left join with the stations dataframe



```r
df = left_join(normals_fixed,
          stations %>% as.data.frame() %>% dplyr::select(code, longitude, latitude), 
          by = 'code')

head(df,3)
```

```
## # A tibble: 3 × 18
##    code name_of_the_station uf    january february march april   may  june  july
##   <dbl> <chr>               <chr>   <dbl>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
## 1 83249 ALAGOINHAS          BA       73.6     75.8  75.5  81.4  85.5  87.1  85.9
## 2 82353 ALTAMIRA            PA       83.6     85.4  85.9  85.6  84.1  80.6  NA  
## 3 82970 ALTO PARNAIBA       MA       81       82.2  82.5  79.5  74.2  68.3  63.4
## # … with 8 more variables: august <dbl>, september <dbl>, october <dbl>,
## #   november <dbl>, december <dbl>, year <dbl>, longitude <dbl>, latitude <dbl>
```


```r
df %>% check_nan_perc()
```

```
##   code name_of_the_station uf    january    february      march       april
## 1    0                   0  0 0.06666667 0.006060606 0.02424242 0.006060606
##          may       june       july      august   september october   november
## 1 0.01818182 0.01212121 0.04242424 0.006060606 0.006060606       0 0.01212121
##     december     year   longitude    latitude
## 1 0.01212121 0.169697 0.006060606 0.006060606
```

And this is the spatial distribution of humidity expected for a year in Brazil.



```r
  df %>% ggplot() +
  geom_point(aes(x = longitude, y = latitude, colour = year))
```

![](/Users/jpolnasc/projetos/geostatistics_in_r/kriging_and_cokriging_climate_normals_brazil/notebooks/data_cleaning/../../../docs/data_cleaning_files/figure-html/unnamed-chunk-16-1.png)<!-- -->


As a last step, we can wrap everything as a "prepare_climate_normals_dataframe" function



```r
prepare_climate_normals_dataframe <- function(normal_dataframe, path_to_raw_data) {
  
  stations = read_excel(paste0(path_to_raw_data,"Estações-Normal-Climatoógica-1981-2010.xls"), skip = 3) %>% clean_names() %>%
  rename("height" = "atitude",
         "number" = "no",
         "code" = "codigo",
         "station_name" = "nome_da_estacao",
         "start_of_operation" = "inicio_operacao",
         "end_of_operation" = "fim_operacao",
         "deactivated" = "situacao") %>%
  mutate(deactivated = as.factor (ifelse(is.na(deactivated), 0,1))) %>%
  select(- c(start_of_operation, end_of_operation))
  
  dataframe_out = normal_dataframe %>% 
    fix_columns() %>%
      left_join(stations %>% as.data.frame() %>% dplyr::select(code, longitude, latitude), 
          by = 'code')
    
  return (dataframe_out)
  
}

df = read_excel(paste0(path,"Normal-Climatologica-UR.xlsx"), skip = 2) %>% 
  clean_names() %>% prepare_climate_normals_dataframe(path)

df %>% head(3)
```

```
## # A tibble: 3 × 18
##    code name_of_the_station uf    january february march april   may  june  july
##   <dbl> <chr>               <chr>   <dbl>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
## 1 83249 ALAGOINHAS          BA       73.6     75.8  75.5  81.4  85.5  87.1  85.9
## 2 82353 ALTAMIRA            PA       83.6     85.4  85.9  85.6  84.1  80.6  NA  
## 3 82970 ALTO PARNAIBA       MA       81       82.2  82.5  79.5  74.2  68.3  63.4
## # … with 8 more variables: august <dbl>, september <dbl>, october <dbl>,
## #   november <dbl>, december <dbl>, year <dbl>, longitude <dbl>, latitude <dbl>
```


... and persist this as a csv file in the cleansed folder



```r
path_out = '../../data/cleansed/climate_normals/umidity.csv'

write_csv(df, path_out)
```

