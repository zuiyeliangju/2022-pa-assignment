Exercise 3
================

## Code from Exercise 2 to be reused

## Setting up data and environment

## We first need to do a few things before we can manipulate the data.

``` r
# set path for R to find our data
data_path <- "/Volumes/Study/MBA/PMBA/ORGB-690-051-People Analytics/Group project/Used data/"
```

## 1. Load data

We’ll load application data only here (you are welcome to load the other
three files as well). Because we are loading from a .parquet format
file, we’ll use library `arrow` and the functions `read_parquet()`. For
the rest of the files, we can use function `read_csv()` which comes with
a package `readr` (which is included in `tidyverse` set of packages, so
if we are loading `tidyverse` there is no need to also load `readr`).
Note that the path to the data file on my computer is defined above, in
the `data_path` variable.

``` r
library(arrow) # to be able to load data in the .parquet format
```

    ## 
    ## Attaching package: 'arrow'

    ## The following object is masked from 'package:utils':
    ## 
    ##     timestamp

``` r
# read application data
app_data_sample <- read_parquet(paste0(data_path,"app_data_sample.parquet"))
```

To inspect the top slice of the data, we can simply call it:

``` r
app_data_sample
```

    ## # A tibble: 2,018,477 × 16
    ##    application_number filing_date examiner_name_last examiner_name_first
    ##    <chr>              <date>      <chr>              <chr>              
    ##  1 08284457           2000-01-26  HOWARD             JACQUELINE         
    ##  2 08413193           2000-10-11  YILDIRIM           BEKIR              
    ##  3 08531853           2000-05-17  HAMILTON           CYNTHIA            
    ##  4 08637752           2001-07-20  MOSHER             MARY               
    ##  5 08682726           2000-04-10  BARR               MICHAEL            
    ##  6 08687412           2000-04-28  GRAY               LINDA              
    ##  7 08716371           2004-01-26  MCMILLIAN          KARA               
    ##  8 08765941           2000-06-23  FORD               VANESSA            
    ##  9 08776818           2000-02-04  STRZELECKA         TERESA             
    ## 10 08809677           2002-02-20  KIM                SUN                
    ## # … with 2,018,467 more rows, and 12 more variables:
    ## #   examiner_name_middle <chr>, examiner_id <dbl>, examiner_art_unit <dbl>,
    ## #   uspc_class <chr>, uspc_subclass <chr>, patent_number <chr>,
    ## #   patent_issue_date <date>, abandon_date <date>, disposal_type <chr>,
    ## #   appl_status_code <dbl>, appl_status_date <chr>, tc <dbl>

### Get gender for examiners

We’ll get gender based on the first name of the examiner, which is
recorded in the field `examiner_name_first`. We’ll use library `gender`
for that, relying on a modified version of their own
[example](https://cran.r-project.org/web/packages/gender/vignettes/predicting-gender.html).

Note that there are over 2 million records in the applications table –
that’s because there are many records for each examiner, as many as the
number of applications that examiner worked on during this time frame.
Our first step therefore is to get all *unique* names in a separate list
`examiner_names`. We will then guess gender for each one and will join
this table back to the original dataset. So, let’s get names without
repetition:

``` r
library(gender)
#install_genderdata_package() # only run this line the first time you use the package, to get data for it
# get a list of first names without repetitions
examiner_names <- app_data_sample %>% 
  distinct(examiner_name_first)
examiner_names
```

    ## # A tibble: 2,595 × 1
    ##    examiner_name_first
    ##    <chr>              
    ##  1 JACQUELINE         
    ##  2 BEKIR              
    ##  3 CYNTHIA            
    ##  4 MARY               
    ##  5 MICHAEL            
    ##  6 LINDA              
    ##  7 KARA               
    ##  8 VANESSA            
    ##  9 TERESA             
    ## 10 SUN                
    ## # … with 2,585 more rows

Now let’s use function `gender()` as shown in the example for the
package to attach a gender and probability to each name and put the
results into the table `examiner_names_gender`

``` r
# get a table of names and gender
examiner_names_gender <- examiner_names %>% 
  do(results = gender(.$examiner_name_first, method = "ssa")) %>% 
  unnest(cols = c(results), keep_empty = TRUE) %>% 
  select(
    examiner_name_first = name,
    gender,
    proportion_female
  )
examiner_names_gender
```

    ## # A tibble: 1,822 × 3
    ##    examiner_name_first gender proportion_female
    ##    <chr>               <chr>              <dbl>
    ##  1 AARON               male              0.0082
    ##  2 ABDEL               male              0     
    ##  3 ABDOU               male              0     
    ##  4 ABDUL               male              0     
    ##  5 ABDULHAKIM          male              0     
    ##  6 ABDULLAH            male              0     
    ##  7 ABDULLAHI           male              0     
    ##  8 ABIGAIL             female            0.998 
    ##  9 ABIMBOLA            female            0.944 
    ## 10 ABRAHAM             male              0.0031
    ## # … with 1,812 more rows

Finally, let’s join that table back to our original applications data
and discard the temporary tables we have just created to reduce clutter
in our environment.

``` r
# remove extra colums from the gender table
examiner_names_gender <- examiner_names_gender %>% 
  select(examiner_name_first, gender)
# joining gender back to the dataset
app_data_sample <- app_data_sample %>% 
  left_join(examiner_names_gender, by = "examiner_name_first")
# cleaning up
rm(examiner_names)
rm(examiner_names_gender)
gc()
```

    ##            used  (Mb) gc trigger  (Mb) limit (Mb) max used  (Mb)
    ## Ncells  4558767 243.5    7558292 403.7         NA  4957304 264.8
    ## Vcells 49591198 378.4   92536563 706.0      16384 79906957 609.7

### Guess the examiner’s race

We’ll now use package `wru` to estimate likely race of an examiner. Just
like with gender, we’ll get a list of unique names first, only now we
are using surnames.

``` r
library(wru)
examiner_surnames <- app_data_sample %>% 
  select(surname = examiner_name_last) %>% 
  distinct()
examiner_surnames
```

    ## # A tibble: 3,806 × 1
    ##    surname   
    ##    <chr>     
    ##  1 HOWARD    
    ##  2 YILDIRIM  
    ##  3 HAMILTON  
    ##  4 MOSHER    
    ##  5 BARR      
    ##  6 GRAY      
    ##  7 MCMILLIAN 
    ##  8 FORD      
    ##  9 STRZELECKA
    ## 10 KIM       
    ## # … with 3,796 more rows

We’ll follow the instructions for the package outlined here
<https://github.com/kosukeimai/wru>.

``` r
examiner_race <- predict_race(voter.file = examiner_surnames, surname.only = T) %>% 
  as_tibble()
```

    ## [1] "Proceeding with surname-only predictions..."

    ## Warning in merge_surnames(voter.file): Probabilities were imputed for 698
    ## surnames that could not be matched to Census list.

``` r
examiner_race
```

    ## # A tibble: 3,806 × 6
    ##    surname    pred.whi pred.bla pred.his pred.asi pred.oth
    ##    <chr>         <dbl>    <dbl>    <dbl>    <dbl>    <dbl>
    ##  1 HOWARD       0.643   0.295    0.0237   0.005     0.0333
    ##  2 YILDIRIM     0.861   0.0271   0.0609   0.0135    0.0372
    ##  3 HAMILTON     0.702   0.237    0.0245   0.0054    0.0309
    ##  4 MOSHER       0.947   0.00410  0.0241   0.00640   0.0185
    ##  5 BARR         0.827   0.117    0.0226   0.00590   0.0271
    ##  6 GRAY         0.687   0.251    0.0241   0.0054    0.0324
    ##  7 MCMILLIAN    0.359   0.574    0.0189   0.00260   0.0463
    ##  8 FORD         0.620   0.32     0.0237   0.0045    0.0313
    ##  9 STRZELECKA   0.666   0.0853   0.137    0.0797    0.0318
    ## 10 KIM          0.0252  0.00390  0.00650  0.945     0.0198
    ## # … with 3,796 more rows

As you can see, we get probabilities across five broad US Census
categories: white, black, Hispanic, Asian and other. (Some of you may
correctly point out that Hispanic is not a race category in the US
Census, but these are the limitations of this package.)

Our final step here is to pick the race category that has the highest
probability for each last name and then join the table back to the main
applications table. See this example for comparing values across
columns: <https://www.tidyverse.org/blog/2020/04/dplyr-1-0-0-rowwise/>.
And this one for `case_when()` function:
<https://dplyr.tidyverse.org/reference/case_when.html>.

``` r
examiner_race <- examiner_race %>% 
  mutate(max_race_p = pmax(pred.asi, pred.bla, pred.his, pred.oth, pred.whi)) %>% 
  mutate(race = case_when(
    max_race_p == pred.asi ~ "Asian",
    max_race_p == pred.bla ~ "black",
    max_race_p == pred.his ~ "Hispanic",
    max_race_p == pred.oth ~ "other",
    max_race_p == pred.whi ~ "white",
    TRUE ~ NA_character_
  ))
examiner_race
```

    ## # A tibble: 3,806 × 8
    ##    surname    pred.whi pred.bla pred.his pred.asi pred.oth max_race_p race 
    ##    <chr>         <dbl>    <dbl>    <dbl>    <dbl>    <dbl>      <dbl> <chr>
    ##  1 HOWARD       0.643   0.295    0.0237   0.005     0.0333      0.643 white
    ##  2 YILDIRIM     0.861   0.0271   0.0609   0.0135    0.0372      0.861 white
    ##  3 HAMILTON     0.702   0.237    0.0245   0.0054    0.0309      0.702 white
    ##  4 MOSHER       0.947   0.00410  0.0241   0.00640   0.0185      0.947 white
    ##  5 BARR         0.827   0.117    0.0226   0.00590   0.0271      0.827 white
    ##  6 GRAY         0.687   0.251    0.0241   0.0054    0.0324      0.687 white
    ##  7 MCMILLIAN    0.359   0.574    0.0189   0.00260   0.0463      0.574 black
    ##  8 FORD         0.620   0.32     0.0237   0.0045    0.0313      0.620 white
    ##  9 STRZELECKA   0.666   0.0853   0.137    0.0797    0.0318      0.666 white
    ## 10 KIM          0.0252  0.00390  0.00650  0.945     0.0198      0.945 Asian
    ## # … with 3,796 more rows

Let’s join the data back to the applications table.

``` r
# removing extra columns
examiner_race <- examiner_race %>% 
  select(surname,race)
app_data_sample <- app_data_sample %>% 
  left_join(examiner_race, by = c("examiner_name_last" = "surname"))
rm(examiner_race)
rm(examiner_surnames)
gc()
```

    ##            used  (Mb) gc trigger  (Mb) limit (Mb) max used  (Mb)
    ## Ncells  4973583 265.7    7558292 403.7         NA  7558292 403.7
    ## Vcells 53387821 407.4   92536563 706.0      16384 90570932 691.1

### Examiner’s tenure

To figure out the timespan for which we observe each examiner in the
applications data, let’s find the first and the last observed date for
each examiner. We’ll first get examiner IDs and application dates in a
separate table, for ease of manipulation. We’ll keep examiner ID (the
field `examiner_id`), and earliest and latest dates for each application
(`filing_date` and `appl_status_date` respectively). We’ll use functions
in package `lubridate` to work with date and time values.

``` r
library(lubridate) # to work with dates
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:arrow':
    ## 
    ##     duration

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
examiner_dates <- app_data_sample %>% 
  select(examiner_id, filing_date, appl_status_date) 
examiner_dates
```

    ## # A tibble: 2,018,477 × 3
    ##    examiner_id filing_date appl_status_date  
    ##          <dbl> <date>      <chr>             
    ##  1       96082 2000-01-26  30jan2003 00:00:00
    ##  2       87678 2000-10-11  27sep2010 00:00:00
    ##  3       63213 2000-05-17  30mar2009 00:00:00
    ##  4       73788 2001-07-20  07sep2009 00:00:00
    ##  5       77294 2000-04-10  19apr2001 00:00:00
    ##  6       68606 2000-04-28  16jul2001 00:00:00
    ##  7       89557 2004-01-26  15may2017 00:00:00
    ##  8       97543 2000-06-23  03apr2002 00:00:00
    ##  9       98714 2000-02-04  27nov2002 00:00:00
    ## 10       65530 2002-02-20  23mar2009 00:00:00
    ## # … with 2,018,467 more rows

The dates look inconsistent in terms of formatting. Let’s make them
consistent. We’ll create new variables `start_date` and `end_date`.

``` r
examiner_dates <- examiner_dates %>% 
  mutate(start_date = ymd(filing_date), end_date = as_date(dmy_hms(appl_status_date))) %>% 
  filter(year(end_date)<2018)
```

Let’s now identify the earliest and the latest date for each examiner
and calculate the difference in days, which is their tenure in the
organization.

``` r
examiner_dates <- examiner_dates %>% 
  group_by(examiner_id) %>% 
  summarise(
    earliest_date = min(start_date, na.rm = TRUE), 
    latest_date = max(end_date, na.rm = TRUE),
    tenure_days = interval(earliest_date, latest_date) %/% days(1)
    )
examiner_dates
```

    ## # A tibble: 5,649 × 4
    ##    examiner_id earliest_date latest_date tenure_days
    ##          <dbl> <date>        <date>            <dbl>
    ##  1       59012 2004-07-28    2015-07-24         4013
    ##  2       59025 2009-10-26    2017-05-18         2761
    ##  3       59030 2005-12-12    2017-05-22         4179
    ##  4       59040 2007-09-11    2017-05-23         3542
    ##  5       59052 2001-08-21    2007-02-28         2017
    ##  6       59054 2000-11-10    2016-12-23         5887
    ##  7       59055 2004-11-02    2007-12-26         1149
    ##  8       59056 2000-03-24    2017-05-22         6268
    ##  9       59074 2000-01-31    2017-03-17         6255
    ## 10       59081 2011-04-21    2017-05-19         2220
    ## # … with 5,639 more rows

Joining back to the applications data.

``` r
app_data_sample <- app_data_sample %>% 
  left_join(examiner_dates, by = "examiner_id")
rm(examiner_dates)
gc()
```

    ##            used  (Mb) gc trigger (Mb) limit (Mb)  max used   (Mb)
    ## Ncells  4987877 266.4   13537783  723         NA  13537783  723.0
    ## Vcells 65766922 501.8  133428650 1018      16384 133354983 1017.5

## Exercise 3

## Linear Regression Models

#### Libraries for the regression

``` r
library(modelsummary)
library(kableExtra)
```

    ## 
    ## Attaching package: 'kableExtra'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     group_rows

### First we calculate art_unit_size and art_unit_gender_ratio variables

``` r
## calculate art unit size
art_unit_size <- app_data_sample %>% group_by(examiner_art_unit) %>% 
  count()  %>% 
  select(
    art_unit_size = n,
    examiner_art_unit
  )
art_unit_size
```

    ## # A tibble: 291 × 2
    ## # Groups:   examiner_art_unit [291]
    ##    art_unit_size examiner_art_unit
    ##            <int>             <dbl>
    ##  1           101              1600
    ##  2            54              1609
    ##  3          6961              1611
    ##  4          9164              1612
    ##  5          5842              1613
    ##  6          9100              1614
    ##  7         14133              1615
    ##  8         15721              1616
    ##  9         11921              1617
    ## 10         10661              1618
    ## # … with 281 more rows

``` r
## joining art_unit_size back to dataset
Table_All_1 <- app_data_sample %>% 
  left_join(art_unit_size, by = "examiner_art_unit")

## calculate art_unit_gender_ratio (female to male ratio)
## count "female" number within each art_unit
art_unit_gender_size = Table_All_1 %>% group_by(examiner_art_unit,gender) %>%
  count() %>%  filter(!is.na(gender)) %>% filter(gender == 'female')

Table_gender_size <- art_unit_gender_size %>% 
  left_join(art_unit_size, by = "examiner_art_unit")

Table_gender_size$female_to_male_ratio = Table_gender_size$n/(Table_gender_size$art_unit_size - Table_gender_size$n)

## joining gender_ratio back to dataset
 
Table_All <- Table_All_1 %>% 
  left_join(Table_gender_size %>% dplyr::select(female_to_male_ratio), by = "examiner_art_unit")
```

    ## Adding missing grouping variables: `examiner_art_unit`, `gender`

``` r
Table_All
```

    ## # A tibble: 2,018,477 × 24
    ##    application_number filing_date examiner_name_last examiner_name_first
    ##    <chr>              <date>      <chr>              <chr>              
    ##  1 08284457           2000-01-26  HOWARD             JACQUELINE         
    ##  2 08413193           2000-10-11  YILDIRIM           BEKIR              
    ##  3 08531853           2000-05-17  HAMILTON           CYNTHIA            
    ##  4 08637752           2001-07-20  MOSHER             MARY               
    ##  5 08682726           2000-04-10  BARR               MICHAEL            
    ##  6 08687412           2000-04-28  GRAY               LINDA              
    ##  7 08716371           2004-01-26  MCMILLIAN          KARA               
    ##  8 08765941           2000-06-23  FORD               VANESSA            
    ##  9 08776818           2000-02-04  STRZELECKA         TERESA             
    ## 10 08809677           2002-02-20  KIM                SUN                
    ## # … with 2,018,467 more rows, and 20 more variables:
    ## #   examiner_name_middle <chr>, examiner_id <dbl>, examiner_art_unit <dbl>,
    ## #   uspc_class <chr>, uspc_subclass <chr>, patent_number <chr>,
    ## #   patent_issue_date <date>, abandon_date <date>, disposal_type <chr>,
    ## #   appl_status_code <dbl>, appl_status_date <chr>, tc <dbl>, gender.x <chr>,
    ## #   race <chr>, earliest_date <date>, latest_date <date>, tenure_days <dbl>,
    ## #   art_unit_size <int>, gender.y <chr>, female_to_male_ratio <dbl>

``` r
# remove unused columns
Table_gender_size = subset(Table_gender_size, select = -c(gender))
```

### OLS 1 - Using gender and race to explain Tenure_days

``` r
app_data_sample$likely_race = as.factor(app_data_sample$race)
levels(app_data_sample$likely_race)
```

    ## [1] "Asian"    "black"    "Hispanic" "other"    "white"

``` r
OLS_1 = lm(tenure_days ~ as.factor(gender) + likely_race, data=app_data_sample)
modelsummary(OLS_1)
```

|                       |    Model 1    |
|:----------------------|:-------------:|
| (Intercept)           |   5571.755    |
|                       |    (2.059)    |
| as.factor(gender)male |   -126.557    |
|                       |    (1.768)    |
| likely_raceblack      |    154.695    |
|                       |    (4.812)    |
| likely_raceHispanic   |   -319.414    |
|                       |    (5.009)    |
| likely_raceother      |   -649.198    |
|                       |   (51.299)    |
| likely_racewhite      |    75.326     |
|                       |    (1.967)    |
| Num.Obs.              |    1714618    |
| R2                    |     0.008     |
| R2 Adj.               |     0.008     |
| AIC                   |  28849607.0   |
| BIC                   |  28849693.5   |
| Log.Lik.              | -14424796.496 |
| F                     |   2741.858    |
| RMSE                  |    1089.96    |

### OLS 2 - Using gender and art_unit_size to explain Tenure_days

``` r
OLS_2 = lm(tenure_days ~art_unit_size + as.factor(gender.x), data=Table_All)
modelsummary(OLS_2)
```

|                         |    Model 1    |
|:------------------------|:-------------:|
| (Intercept)             |   5175.248    |
|                         |    (2.161)    |
| art_unit_size           |     0.042     |
|                         |    (0.000)    |
| as.factor(gender.x)male |    -82.789    |
|                         |    (1.741)    |
| Num.Obs.                |    1714618    |
| R2                      |     0.045     |
| R2 Adj.                 |     0.045     |
| AIC                     |  28785125.2   |
| BIC                     |  28785174.6   |
| Log.Lik.                | -14392558.603 |
| F                       |   39968.961   |
| RMSE                    |    1069.66    |

### OLS 3 - Using art_unit_size and race variabes to explain Tenure_days

``` r
OLS_3 <- lm(tenure_days ~ art_unit_size + as.factor(race)  , data=Table_All)
modelsummary(OLS_3)
```

|                         |    Model 1    |
|:------------------------|:-------------:|
| (Intercept)             |   5089.649    |
|                         |    (1.881)    |
| art_unit_size           |     0.043     |
|                         |    (0.000)    |
| as.factor(race)black    |    93.889     |
|                         |    (3.842)    |
| as.factor(race)Hispanic |   -303.481    |
|                         |    (4.610)    |
| as.factor(race)other    |   -578.071    |
|                         |   (50.491)    |
| as.factor(race)white    |    33.126     |
|                         |    (1.699)    |
| Num.Obs.                |    2018477    |
| R2                      |     0.050     |
| R2 Adj.                 |     0.050     |
| AIC                     |  33898992.4   |
| BIC                     |  33899080.0   |
| Log.Lik.                | -16949489.209 |
| F                       |   21260.670   |
| RMSE                    |    1073.02    |

### OLS 4 - Using gender,race and art_unit_size variabes to explain Tenure_days

``` r
OLS_4 = lm(tenure_days ~ Table_All$art_unit_size + as.factor(gender.x) + as.factor(race)  , data=Table_All)
modelsummary(OLS_4)
```

|                         |    Model 1    |
|:------------------------|:-------------:|
| (Intercept)             |   5170.987    |
|                         |    (2.512)    |
| Table_All$art_unit_size |     0.042     |
|                         |    (0.000)    |
| as.factor(gender.x)male |    -79.919    |
|                         |    (1.741)    |
| as.factor(race)black    |    145.367    |
|                         |    (4.714)    |
| as.factor(race)Hispanic |   -307.032    |
|                         |    (4.907)    |
| as.factor(race)other    |   -567.322    |
|                         |   (50.259)    |
| as.factor(race)white    |    17.361     |
|                         |    (1.940)    |
| Num.Obs.                |    1714618    |
| R2                      |     0.048     |
| R2 Adj.                 |     0.048     |
| AIC                     |  28779286.6   |
| BIC                     |  28779385.4   |
| Log.Lik.                | -14389635.294 |
| F                       |   14344.558   |
| RMSE                    |    1067.84    |

### OLS 5 - Using gender,race, art_unit_size and female_to_male_ratio variabes to explain Tenure_days

``` r
OLS_5 = lm(tenure_days ~ art_unit_size + as.factor(gender.x) + as.factor(race) + female_to_male_ratio , data=Table_All)
modelsummary(OLS_5)
```

|                         |    Model 1    |
|:------------------------|:-------------:|
| (Intercept)             |   5066.715    |
|                         |    (2.739)    |
| art_unit_size           |     0.040     |
|                         |    (0.000)    |
| as.factor(gender.x)male |    -26.295    |
|                         |    (1.821)    |
| as.factor(race)black    |    152.052    |
|                         |    (4.716)    |
| as.factor(race)Hispanic |   -306.718    |
|                         |    (4.894)    |
| as.factor(race)other    |   -576.222    |
|                         |   (50.113)    |
| as.factor(race)white    |    -4.036     |
|                         |    (1.948)    |
| female_to_male_ratio    |    210.896    |
|                         |    (2.194)    |
| Num.Obs.                |    1711242    |
| R2                      |     0.053     |
| R2 Adj.                 |     0.053     |
| AIC                     |  28712662.9   |
| BIC                     |  28712774.1   |
| Log.Lik.                | -14356322.456 |
| F                       |   13608.718   |
| RMSE                    |    1064.73    |

## Exercise 3

## Anser to questions

#### Q1: Discuss which model is a better fit with the data and explain why you think so?

By trying the 5 regression models above, regression model OLS_5 is the
best fit one, since it has the largest r-square value which means this
model explain the most numbers of observations. It also has a small
number of residual error.

### Q2: Interpret your estimated effects of the variables you have included in the models

Observation from models include following: 1. The intercept of each
model is about 5100 days (max with 5500), that is a base line of tenure
days for each agent. 2. Based on that, the larger art unit size (or
saying the larger group), the longer agent would stay. 3. Male agents
stay shorter than female, generally. Hispanic race agent stay shorter
than the other known races. 4. Blake race agent tend to stay longer. 5.
The higher female to male ration within one art unit, the longer the
agent will stay.

### Q3: What recommendations can you offer based on your analysis?

My recommendation so far is to extend the general tenure days for
agents, the manager would consider group small group into larger group,
or saying combine similar units into bigger ones. And pay attention to
the gender ratio within same group, keeping it in a fairly high value
would help keep the agents.
