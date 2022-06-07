Exercise 5
================

## 0. Loading data and preparing variables

Here, I’m using the steps from exercise 2 example to create the
necessary variables.

``` r
# set path for R to find our data
data_path <- "/Volumes/Study/MBA/PMBA/ORGB-690-051-People Analytics/Group project/Used data/"
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

library(gender)
#install_genderdata_package() # only run this line the first time you use the package, to get data for it

# get a list of first names without repetitions
examiner_names <- app_data_sample %>% 
  distinct(examiner_name_first)

examiner_names_gender <- examiner_names %>% 
  do(results = gender(.$examiner_name_first, method = "ssa")) %>% 
  unnest(cols = c(results), keep_empty = TRUE) %>% 
  select(
    examiner_name_first = name,
    gender,
    proportion_female
  )

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
    ## Ncells  4528427 241.9    8002852 427.4         NA  4548616 243.0
    ## Vcells 49525029 377.9   95441066 728.2      16384 79840822 609.2

``` r
# Examiners' race
library(wru)

examiner_surnames <- app_data_sample %>% 
  select(surname = examiner_name_last) %>% 
  distinct()

examiner_race <- predict_race(voter.file = examiner_surnames, surname.only = T) %>% 
  as_tibble()
```

    ## [1] "Proceeding with surname-only predictions..."

    ## Warning in merge_surnames(voter.file): Probabilities were imputed for 698
    ## surnames that could not be matched to Census list.

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

examiner_race <- examiner_race %>% 
  select(surname,race)

app_data_sample <- app_data_sample %>% 
  left_join(examiner_race, by = c("examiner_name_last" = "surname"))

rm(examiner_race)
rm(examiner_surnames)
gc()
```

    ##            used  (Mb) gc trigger  (Mb) limit (Mb) max used  (Mb)
    ## Ncells  4943050 264.0    8002852 427.4         NA  8002852 427.4
    ## Vcells 53320741 406.9   95441066 728.2      16384 95216651 726.5

``` r
# Examiner's tenure

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

examiner_dates <- examiner_dates %>% 
  mutate(start_date = ymd(filing_date), end_date = as_date(dmy_hms(appl_status_date))) %>% 
  filter(year(end_date)<2018) %>% 
  group_by(examiner_id) %>% 
  summarise(
    earliest_date = min(start_date, na.rm = TRUE), 
    latest_date = max(end_date, na.rm = TRUE),
    tenure_days = interval(earliest_date, latest_date) %/% days(1)
    )

app_data_sample <- app_data_sample %>% 
  left_join(examiner_dates, by = "examiner_id")

rm(examiner_dates)
gc()
```

    ##            used  (Mb) gc trigger   (Mb) limit (Mb)  max used   (Mb)
    ## Ncells  4957950 264.8   14376536  767.8         NA  14376536  767.8
    ## Vcells 65701071 501.3  165213360 1260.5      16384 137317605 1047.7

## Adding paygrade data

First, we load the paygrade file.

``` r
examiner_gs <- read_csv(paste0(data_path,"examiner_gs.csv"))
```

    ## Rows: 52109 Columns: 6
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (3): examiner_name, start_date, end_date
    ## dbl (3): examiner_grade, old_pid, new_pid
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
examiner_ids <- read_csv(paste0(data_path,"examiner_ids.csv"))
```

    ## Rows: 19454 Columns: 4
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): examiner_name
    ## dbl (3): old_pid, new_pid, patex_id
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

### We need to replace various IDs with examiner ID

The ID fields in the `examiner_gs.csv` file don’t match those in the
application data file. Because we’ll need to join these files later, we
need to bring in the proper ID field, by using the cross-walk file
`examiner_ids`.

``` r
examiner_gs <- examiner_gs %>% 
  left_join(examiner_ids) %>% 
  select(
    grade = examiner_grade,
    start_date,
    end_date,
    examiner_id = patex_id
  )
```

    ## Joining, by = c("examiner_name", "old_pid", "new_pid")

## Estimate time in grade

Now we need to estimate the average time each examiner spends in a given
GS paygrade. Note that the less-biased way to do that is to exclude the
latest or highest grade for each examiner. This is because after
examiners reach the highest grade (which is usually grade 14 in this
context), they cannot advance to the next grade. Imagine someone who
advances through grades 11, 12 and 13 in just 18 months (so, the average
for that examiner is 6 months per grade), but then stays in grade 14 and
works at the agency for another ten years. If you were to average all
grades, it would look like it took the examiner several years on average
to get promoted through each grade. This is because the last grade
biases the average positively.

Similarly, for examiners who get promoted right before the end of our
sample’s time window, the very short time they stay in the highest
observed grade will negatively bias the average. It will look like the
examiner has progressed through the grades way faster than she did.

``` r
time_in_grade <- examiner_gs %>% 
  mutate(
    start_date = mdy(start_date), # converting into proper date type
    end_date = mdy(end_date), # converting into proper date type
    days_in_grade = interval(start_date, end_date) %/% days(1)
  ) %>% 
  group_by(examiner_id) %>% 
  filter(grade!=max(grade, na.rm = TRUE)) %>% # dropping the highest grade record
  summarise(mean_days_in_grade = mean(days_in_grade, na.rm = TRUE))

time_in_grade
```

    ## # A tibble: 10,860 × 2
    ##    examiner_id mean_days_in_grade
    ##          <dbl>              <dbl>
    ##  1       59012               356.
    ##  2       59015               783 
    ##  3       59016               341.
    ##  4       59018               368.
    ##  5       59019               293 
    ##  6       59025               485 
    ##  7       59027               364.
    ##  8       59030               493.
    ##  9       59033               258.
    ## 10       59035               308.
    ## # … with 10,850 more rows

## Prepare application data

Let’s get the measure of application processing time for each examiner.
We’ll do this by “collapsing” the dataframe from application level to
examiner level (i.e., one record for each examiner, not multiple
records).

``` r
examiner_data <- app_data_sample %>% 
  filter(disposal_type!="PEND") %>% # here, we exclude in-process applications
  mutate(
    app_start_date = ymd(filing_date),
    app_end_date = case_when(
      disposal_type == "ISS" ~ ymd(patent_issue_date), # for issued patents
      disposal_type == "ABN" ~ ymd(abandon_date), # for abandoned applications
      TRUE ~ NA_Date_
    ),
    app_proc_days = interval(app_start_date, app_end_date) %/% days(1)) %>% 
  filter(app_proc_days>0 & app_proc_days < 3650) %>% # limit to 0-10 years
  group_by(examiner_id) %>% 
  summarise(
    app_count = n(),
    tc = min(tc, na.rm = TRUE),
    gender = first(gender),
    race = first(race),
    tenure_days = max(tenure_days, na.rm = TRUE),
    mean_app_proc_days = mean(app_proc_days, na.rm = TRUE)
  )

examiner_data
```

    ## # A tibble: 5,549 × 7
    ##    examiner_id app_count    tc gender race  tenure_days mean_app_proc_days
    ##          <dbl>     <int> <dbl> <chr>  <chr>       <dbl>              <dbl>
    ##  1       59012        84  1700 male   white        4013              1295.
    ##  2       59025        96  2400 male   Asian        2761              1152.
    ##  3       59030       358  2400 <NA>   black        4179              1008.
    ##  4       59040       233  1700 female Asian        3542              1305.
    ##  5       59052         8  2100 male   Asian        2017               535.
    ##  6       59054        10  2100 <NA>   Asian        5887              1297 
    ##  7       59055         2  2100 male   Asian        1149               932.
    ##  8       59056      1019  2100 male   Asian        6268              1077.
    ##  9       59074       166  2100 <NA>   white        6255              1579.
    ## 10       59081        48  2400 male   Asian        2220              1317.
    ## # … with 5,539 more rows

Now, let’s join in the time in grade data.

``` r
examiner_data <- examiner_data %>% 
  left_join(time_in_grade)
```

    ## Joining, by = "examiner_id"

``` r
examiner_data
```

    ## # A tibble: 5,549 × 8
    ##    examiner_id app_count    tc gender race  tenure_days mean_app_proc_days
    ##          <dbl>     <int> <dbl> <chr>  <chr>       <dbl>              <dbl>
    ##  1       59012        84  1700 male   white        4013              1295.
    ##  2       59025        96  2400 male   Asian        2761              1152.
    ##  3       59030       358  2400 <NA>   black        4179              1008.
    ##  4       59040       233  1700 female Asian        3542              1305.
    ##  5       59052         8  2100 male   Asian        2017               535.
    ##  6       59054        10  2100 <NA>   Asian        5887              1297 
    ##  7       59055         2  2100 male   Asian        1149               932.
    ##  8       59056      1019  2100 male   Asian        6268              1077.
    ##  9       59074       166  2100 <NA>   white        6255              1579.
    ## 10       59081        48  2400 male   Asian        2220              1317.
    ## # … with 5,539 more rows, and 1 more variable: mean_days_in_grade <dbl>

## Q1: How long does it take, on average, men and women to advance to the next pay grade? (Hint: you may want to exclude time in the highest paygrade for this analysis)

It took female agent 542 days to advance to the next pay grade, and took
male agent 546 days to the next pat grade, which is very close to
female. So we cannot observe obvious difference on days required for
promotion based on gender.

``` r
female_table <- examiner_data %>% 
  filter(gender == "female")
mean(female_table$mean_days_in_grade,na.rm=TRUE)
```

    ## [1] 542.1556

``` r
male_table <- examiner_data %>% 
  filter(gender == "male")
mean(male_table$mean_days_in_grade,na.rm=TRUE)
```

    ## [1] 546.1771

## Q2: Are these differences themselves different by examiners’ race?

It took white agent average 551 days to advance to the next pay grade,
took Asian agent 534 days to the next pat grade, took black agent 589
days to the next pat grade, and took Hispanic agent 504 days to the next
pat grade. The difference by race cannot be ignored, for example, the
race with longest average days in one grade is about 17% more than the
race with the shortest days in one grade.

``` r
white_table <- examiner_data %>% 
  filter(race == "white")
mean(white_table$mean_days_in_grade,na.rm=TRUE)
```

    ## [1] 551.0042

``` r
asian_table <- examiner_data %>% 
  filter(race == "Asian")
mean(asian_table$mean_days_in_grade,na.rm=TRUE)
```

    ## [1] 534.5086

``` r
black_table <- examiner_data %>% 
  filter(race == "black")
mean(black_table$mean_days_in_grade,na.rm=TRUE)
```

    ## [1] 589.2735

``` r
hispanic_table <- examiner_data %>% 
  filter(race == "Hispanic")
mean(hispanic_table$mean_days_in_grade,na.rm=TRUE)
```

    ## [1] 504.2059

## Q3: Is there are relationship (an association) between average time in grade and average application prosecution time?

First, I calculated the correlation coefficient equals to 0.006 or 0.6%,
which is very small meaning there is barely no relationship between
average days in one grade and the average application prosecution time.

Scatterplot also shows that there is no linear regression relationship
between these two factors.

By running a few regression models considering average application
prosecution time, gender and race, I couldn’t find obvious relationship
between average time in one grade and the average application
prosecution time.

``` r
library(ggpubr)
```

``` r
cor(examiner_data$mean_days_in_grade,examiner_data$mean_app_proc_days, method = c("pearson", "kendall", "spearman"))
```

    ## [1] NA

``` r
cor.test(examiner_data$mean_days_in_grade,examiner_data$mean_app_proc_days, method=c("pearson", "kendall", "spearman"))
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  examiner_data$mean_days_in_grade and examiner_data$mean_app_proc_days
    ## t = 0.39953, df = 4501, p-value = 0.6895
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.02325809  0.03515805
    ## sample estimates:
    ##         cor 
    ## 0.005955058

``` r
ggscatter(examiner_data, x = "mean_days_in_grade", y = "mean_app_proc_days", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          )
```

    ## `geom_smooth()` using formula 'y ~ x'

    ## Warning: Removed 1046 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 1046 rows containing non-finite values (stat_cor).

    ## Warning: Removed 1046 rows containing missing values (geom_point).

![](Assignment-5_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
names(examiner_data)
```

    ## [1] "examiner_id"        "app_count"          "tc"                
    ## [4] "gender"             "race"               "tenure_days"       
    ## [7] "mean_app_proc_days" "mean_days_in_grade"

``` r
library(modelsummary)
models <- list()
models[['m1']] <- lm(mean_days_in_grade ~ 1 + mean_app_proc_days, data = examiner_data) 
models[['m2']] <- lm(mean_days_in_grade ~ 1 + mean_app_proc_days + as_factor(gender),data = examiner_data) 
models[['m3']] <- lm(mean_days_in_grade ~ 1 + mean_app_proc_days + as_factor(race),data = examiner_data) 
models[['m4']] <- lm(mean_days_in_grade ~ 1 + mean_app_proc_days + as_factor(gender)+ as_factor(race), data = examiner_data) 
models[['m5']] <- lm(mean_days_in_grade ~ 1 + as_factor(gender) + as_factor(race), data = examiner_data)
modelsummary(models)
```

|                         |     m1     |     m2     |     m3     |     m4     |     m5     |
|:------------------------|:----------:|:----------:|:----------:|:----------:|:----------:|
| (Intercept)             |  528.481   |  550.975   |  531.761   |  552.690   |  549.214   |
|                         |  (43.856)  |  (49.860)  |  (44.213)  |  (50.105)  |  (15.346)  |
| mean_app_proc_days      |   0.014    |   -0.004   |   0.016    |   -0.003   |            |
|                         |  (0.035)   |  (0.039)   |  (0.035)   |  (0.040)   |            |
| as_factor(gender)female |            |   -4.166   |            |   -4.611   |   -4.505   |
|                         |            |  (23.854)  |            |  (23.871)  |  (23.824)  |
| as_factor(race)Asian    |            |            |  -17.130   |   -9.874   |  -10.004   |
|                         |            |            |  (21.627)  |  (25.693)  |  (25.628)  |
| as_factor(race)black    |            |            |   38.196   |   51.199   |   51.149   |
|                         |            |            |  (49.231)  |  (60.209)  |  (60.197)  |
| as_factor(race)Hispanic |            |            |  -46.940   |  -46.899   |  -46.944   |
|                         |            |            |  (49.354)  |  (53.547)  |  (53.536)  |
| as_factor(race)other    |            |            |  -86.266   |  -81.593   |  -82.014   |
|                         |            |            | (654.746)  | (681.698)  | (681.586)  |
| Num.Obs.                |    4503    |    3838    |    4503    |    3838    |    3838    |
| R2                      |   0.000    |   0.000    |   0.001    |   0.000    |   0.000    |
| R2 Adj.                 |   0.000    |   -0.001   |   -0.001   |   -0.001   |   -0.001   |
| AIC                     |  71176.4   |  60975.0   |  71182.1   |  60981.3   |  60979.3   |
| BIC                     |  71195.6   |  61000.0   |  71227.0   |  61031.3   |  61023.1   |
| Log.Lik.                | -35585.191 | -30483.507 | -35584.071 | -30482.639 | -30482.642 |
| F                       |   0.160    |   0.019    |   0.480    |   0.295    |   0.353    |
| RMSE                    |   654.48   |   681.30   |   654.60   |   681.50   |   681.41   |

## Q4: Write a substantive conclusion, discussing what these results could mean. Make sure to discuss implications but also the limitations of your approach and possible threats to inference (this point must be done individually and not in a group)

My first conclusion is that we cannot use application processing time to
predict the agents’ promotion speed. The correlation between agents’
promotion and race and gender is negligible too. It seems to me that,
the application processing time cannot reflect agents’ performance, and
agents’ promotion is not based how quickly they finish the applications.

The second conclusion is that there is barely no competition exists in
this organization, everyone can get a promotion after working for a
similar period of time, which is slightly less than 2 years.

The limitations of my analysis include: 1. Maybe there are important
factors that been used to evaluate agents’ performance which do not been
included in current data set. 2. Maybe there are non-linear relationship
exists between the average days in one grade and the factors been
considered, which are not been checked here.
