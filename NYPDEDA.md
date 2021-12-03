NYPD SHOOTING
================
03/12/2021

### DATA PREPERATION

The data used was taken from the following link below in the code,
provided by the professor.

``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.5     v purrr   0.3.4
    ## v tibble  3.1.5     v dplyr   1.0.7
    ## v tidyr   1.1.4     v stringr 1.4.0
    ## v readr   2.0.2     v forcats 0.5.1

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
data_link<-c("https://data.cityofnewyork.us/api/views/833y-fsy8/rows.csv?accessType=DOWNLOAD")
df<-read_csv(data_link)
```

    ## Rows: 23585 Columns: 19

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr  (10): OCCUR_DATE, BORO, LOCATION_DESC, PERP_AGE_GROUP, PERP_SEX, PERP_R...
    ## dbl   (7): INCIDENT_KEY, PRECINCT, JURISDICTION_CODE, X_COORD_CD, Y_COORD_CD...
    ## lgl   (1): STATISTICAL_MURDER_FLAG
    ## time  (1): OCCUR_TIME

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
dim(df)
```

    ## [1] 23585    19

``` r
summary(df)
```

    ##   INCIDENT_KEY        OCCUR_DATE         OCCUR_TIME           BORO          
    ##  Min.   :  9953245   Length:23585       Length:23585      Length:23585      
    ##  1st Qu.: 55322804   Class :character   Class1:hms        Class :character  
    ##  Median : 83435362   Mode  :character   Class2:difftime   Mode  :character  
    ##  Mean   :102280741                      Mode  :numeric                      
    ##  3rd Qu.:150911774                                                          
    ##  Max.   :230611229                                                          
    ##                                                                             
    ##     PRECINCT      JURISDICTION_CODE LOCATION_DESC      STATISTICAL_MURDER_FLAG
    ##  Min.   :  1.00   Min.   :0.000     Length:23585       Mode :logical          
    ##  1st Qu.: 44.00   1st Qu.:0.000     Class :character   FALSE:19085            
    ##  Median : 69.00   Median :0.000     Mode  :character   TRUE :4500             
    ##  Mean   : 66.21   Mean   :0.333                                               
    ##  3rd Qu.: 81.00   3rd Qu.:0.000                                               
    ##  Max.   :123.00   Max.   :2.000                                               
    ##                   NA's   :2                                                   
    ##  PERP_AGE_GROUP       PERP_SEX          PERP_RACE         VIC_AGE_GROUP     
    ##  Length:23585       Length:23585       Length:23585       Length:23585      
    ##  Class :character   Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##    VIC_SEX            VIC_RACE           X_COORD_CD        Y_COORD_CD    
    ##  Length:23585       Length:23585       Min.   : 914928   Min.   :125757  
    ##  Class :character   Class :character   1st Qu.: 999925   1st Qu.:182539  
    ##  Mode  :character   Mode  :character   Median :1007654   Median :193470  
    ##                                        Mean   :1009379   Mean   :207300  
    ##                                        3rd Qu.:1016782   3rd Qu.:239163  
    ##                                        Max.   :1066815   Max.   :271128  
    ##                                                                          
    ##     Latitude       Longitude        Lon_Lat         
    ##  Min.   :40.51   Min.   :-74.25   Length:23585      
    ##  1st Qu.:40.67   1st Qu.:-73.94   Class :character  
    ##  Median :40.70   Median :-73.92   Mode  :character  
    ##  Mean   :40.74   Mean   :-73.91                     
    ##  3rd Qu.:40.82   3rd Qu.:-73.88                     
    ##  Max.   :40.91   Max.   :-73.70                     
    ## 

``` r
sapply(df, function(x) sum(is.na(x)))
```

    ##            INCIDENT_KEY              OCCUR_DATE              OCCUR_TIME 
    ##                       0                       0                       0 
    ##                    BORO                PRECINCT       JURISDICTION_CODE 
    ##                       0                       0                       2 
    ##           LOCATION_DESC STATISTICAL_MURDER_FLAG          PERP_AGE_GROUP 
    ##                   13581                       0                    8295 
    ##                PERP_SEX               PERP_RACE           VIC_AGE_GROUP 
    ##                    8261                    8261                       0 
    ##                 VIC_SEX                VIC_RACE              X_COORD_CD 
    ##                       0                       0                       0 
    ##              Y_COORD_CD                Latitude               Longitude 
    ##                       0                       0                       0 
    ##                 Lon_Lat 
    ##                       0

``` r
df=select(df, -c(INCIDENT_KEY,Lon_Lat,Longitude,Latitude,Y_COORD_CD,X_COORD_CD,PERP_AGE_GROUP,PERP_RACE,PERP_SEX,JURISDICTION_CODE,PRECINCT,LOCATION_DESC))
df=na.omit(df)
dim(df)
```

    ## [1] 23585     7

``` r
df <- df %>% 
  mutate(OCCUR_DATE = mdy(OCCUR_DATE))
head(df)
```

    ## # A tibble: 6 x 7
    ##   OCCUR_DATE OCCUR_TIME BORO     STATISTICAL_MUR~ VIC_AGE_GROUP VIC_SEX VIC_RACE
    ##   <date>     <time>     <chr>    <lgl>            <chr>         <chr>   <chr>   
    ## 1 2006-08-27 05:35      BRONX    TRUE             25-44         F       BLACK H~
    ## 2 2011-03-11 12:03      QUEENS   FALSE            65+           M       WHITE   
    ## 3 2019-10-06 01:09      BROOKLYN FALSE            18-24         F       BLACK   
    ## 4 2011-09-04 03:35      BRONX    FALSE            <18           M       BLACK   
    ## 5 2013-05-27 21:16      QUEENS   FALSE            18-24         M       BLACK   
    ## 6 2013-09-01 04:17      BROOKLYN FALSE            <18           M       BLACK

### Graph Preparation

``` r
df_boro <- table(df$BORO)
df_boro <- as.data.frame(df_boro)
p1 <- ggplot(df_boro, aes(x=Var1, y=Freq, fill=Var1)) + geom_bar(stat="identity")
p1 <- p1 + ggtitle("Number of Shootings in the Borough") +xlab("Borough")+ ylab("Numbers")
p1 <- p1 + theme(axis.text.x = element_text(angle = -90))
victim_race <- table(df$VIC_RACE)
victim_race <- as.data.frame(victim_race)
p2 <-ggplot(victim_race, aes(x=Var1, y=Freq, fill=Var1)) + geom_bar(stat="identity")+ggtitle("Victim Race") +xlab("Race")+ ylab("Numbers")
p2 <- p2 + theme(axis.text.x = element_text(angle = 90))
victim_age <- table(df$VIC_AGE_GROUP)
victim_age  <- as.data.frame(victim_age)
p3 <- ggplot(victim_age, aes(x=Var1, y=Freq, fill=Var1)) + geom_bar(stat="identity")+ggtitle("Victim Ages") +xlab("Age Group")+ ylab("Numbers")
victim_sex <- table(df$VIC_SEX)
victim_sex  <- as.data.frame(victim_sex)
p4 <- ggplot(victim_sex, aes(x=Var1, y=Freq, fill=Var1)) + geom_bar(stat="identity")+ggtitle("Victim Gender") +xlab("Gender")+ ylab("Numbers")
Br <-subset(df, BORO=='BROOKLYN', select=c(BORO, OCCUR_DATE))
n <- 4
Br$YEAR <- substr(Br$OCCUR_DATE, nchar(Br$OCCUR_DATE) - n + 1, nchar(Br$OCCUR_DATE))
Br <- subset(Br, select = -c(OCCUR_DATE))
BROOKLYN  <- table(Br$YEAR)
BROOKLYN  <- as.data.frame(BROOKLYN )
p5 <- ggplot(data=BROOKLYN, aes(x=Var1, y=Freq, group=1)) +
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
p5 <- p5 + ggtitle("Number of Shooting in BROOKLYN") +xlab("YEAR")+ ylab("Numbers")
p6 <- p5 + stat_smooth(method = "lm")
```

### GRAPHS

``` r
plot(p1)
```

![](NYPDEDA_files/figure-gfm/unnamed-chunk-3-1.png)<!-- --> Looking at
the graph you can see that most of the shooting incident happened in
Brooklyn and Bronx had the second highest number of shootings.

``` r
plot(p2)
```

![](NYPDEDA_files/figure-gfm/unnamed-chunk-4-1.png)<!-- --> Blacks were
killed the most, the difference between the number of Blacks killed
compared to every other race is significantly big. A lot of this could
be due to the struggles Blacks go through in America.

``` r
plot(p3)
```

![](NYPDEDA_files/figure-gfm/unnamed-chunk-5-1.png)<!-- --> Most of that
were killed belonged in the age group of 25-44, and in close second the
age group of 18-24.

``` r
plot(p4)
```

![](NYPDEDA_files/figure-gfm/unnamed-chunk-6-1.png)<!-- --> The number
of males killed in New York is significantly higher compared to the
number of women killed.

``` r
plot(p5)
```

![](NYPDEDA_files/figure-gfm/unnamed-chunk-7-1.png)<!-- --> The model
shows a steady decrease in the number of shootings over the years from
2006 to 2020. But in 2020 there was a significant up in killings in the
borough of Brooklyn. The number of killings reached to how much there
were in the mid to late 2000s. We have to wait until 2021 to see if this
was an outline or Brooklyn is going back to how it was in the past with
a lot of shooting incidients.

### MODELS

``` r
plot(p6)
```

    ## `geom_smooth()` using formula 'y ~ x'

![](NYPDEDA_files/figure-gfm/unnamed-chunk-8-1.png)<!-- --> Using this
linear regression line you can use it to predict the number of shootings
over the years. The linear regression line shows its going in a downard
slope, which indicates that the number of killings over the years in
Brooklyn have been decreasing.

``` r
sessionInfo()
```

    ## R version 4.1.2 (2021-11-01)
    ## Platform: x86_64-w64-mingw32/x64 (64-bit)
    ## Running under: Windows 10 x64 (build 19043)
    ## 
    ## Matrix products: default
    ## 
    ## locale:
    ## [1] LC_COLLATE=English_Canada.1252  LC_CTYPE=English_Canada.1252   
    ## [3] LC_MONETARY=English_Canada.1252 LC_NUMERIC=C                   
    ## [5] LC_TIME=English_Canada.1252    
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ##  [1] lubridate_1.8.0 forcats_0.5.1   stringr_1.4.0   dplyr_1.0.7    
    ##  [5] purrr_0.3.4     readr_2.0.2     tidyr_1.1.4     tibble_3.1.5   
    ##  [9] ggplot2_3.3.5   tidyverse_1.3.1
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] Rcpp_1.0.7       lattice_0.20-45  assertthat_0.2.1 digest_0.6.28   
    ##  [5] utf8_1.2.2       R6_2.5.1         cellranger_1.1.0 backports_1.3.0 
    ##  [9] reprex_2.0.1     evaluate_0.14    highr_0.9        httr_1.4.2      
    ## [13] pillar_1.6.4     rlang_0.4.12     curl_4.3.2       readxl_1.3.1    
    ## [17] rstudioapi_0.13  Matrix_1.3-4     rmarkdown_2.11   splines_4.1.2   
    ## [21] labeling_0.4.2   bit_4.0.4        munsell_0.5.0    broom_0.7.10    
    ## [25] compiler_4.1.2   modelr_0.1.8     xfun_0.27        pkgconfig_2.0.3 
    ## [29] mgcv_1.8-38      htmltools_0.5.2  tidyselect_1.1.1 fansi_0.5.0     
    ## [33] crayon_1.4.2     tzdb_0.2.0       dbplyr_2.1.1     withr_2.4.2     
    ## [37] grid_4.1.2       nlme_3.1-153     jsonlite_1.7.2   gtable_0.3.0    
    ## [41] lifecycle_1.0.1  DBI_1.1.1        magrittr_2.0.1   scales_1.1.1    
    ## [45] cli_3.1.0        stringi_1.7.5    vroom_1.5.5      farver_2.1.0    
    ## [49] fs_1.5.0         xml2_1.3.2       ellipsis_0.3.2   generics_0.1.1  
    ## [53] vctrs_0.3.8      tools_4.1.2      bit64_4.0.5      glue_1.4.2      
    ## [57] hms_1.1.1        parallel_4.1.2   fastmap_1.1.0    yaml_2.2.1      
    ## [61] colorspace_2.0-2 rvest_1.0.2      knitr_1.36       haven_2.4.3
