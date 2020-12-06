Advent of Code Solutions
================
Kevin Kent
12/2/2020

# Day 1

# Part 1

``` r
library(here)
```

    ## here() starts at /Users/kevinkent/Documents/AdventOfCode

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.2     ✓ purrr   0.3.4
    ## ✓ tibble  3.0.4     ✓ dplyr   1.0.2
    ## ✓ tidyr   1.1.2     ✓ stringr 1.4.0
    ## ✓ readr   1.4.0     ✓ forcats 0.5.0

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
data <- read.table(here::here("2020", "Inputs", "day_1.txt"))

diffs <- 2020 - data$V1 

data %>%
  filter(V1 %in%  diffs) %>%
  prod(.)
```

    ## [1] 692916

# Part 2

``` r
expand.grid(data$V1, data$V1) %>%
  filter((Var1 + Var2) %in% diffs) %>%
  select(Var1) %>%
  distinct() %>%
  prod(.)
```

    ## [1] 289270976

# Day 2

## Part 1

``` r
library(here)
library(tidyverse)

data <- read.table(here::here("2020", "Inputs", "day_2.txt"), sep = " ", col.names = c("lower_upper", "letter", "string")) %>%
  mutate(letter = str_remove_all(letter, ":")) %>%
  separate(lower_upper, into = c("lower", "upper"), sep = "-") 

data %>%
  mutate(string = str_extract_all(string, letter)) %>%
  mutate(length = map(string, ~ length(.))) %>%
  mutate(valid = case_when(
    as.numeric(length) >= as.numeric(lower) & as.numeric(length) <= as.numeric(upper) ~ TRUE,
    TRUE ~ FALSE
  )) %>%
  summarise(sum(valid))
```

    ##   sum(valid)
    ## 1        447

## Part 2

``` r
data %>%
  mutate(lower = as.numeric(lower),
         upper = as.numeric(upper)) %>%
  mutate(string_first = substr(string, lower, lower), 
         string_second = substr(string, upper, upper)) %>%
  mutate(valid = case_when(
    string_first == letter & string_second ==  letter ~ FALSE,
    string_first == letter | string_second == letter ~ TRUE,
    TRUE ~ FALSE
  )) %>%
  summarise(sum(valid))
```

    ##   sum(valid)
    ## 1        249

# Day 3

## Part 1 & 2

``` r
data <- read_lines(here::here("2020", "Inputs", "day_3.txt")) %>%
  as.data.frame(.) %>%
  rename(pattern = 1) 

get_counts <- function(slope, rows, data) {
  
data %>%
  slice(rows) %>%
  mutate(row = row_number()) %>%
  mutate(position = slope*(row - 1) + 1) %>%
  mutate(adj_position = position %% 31) %>%
  mutate(adj_position = case_when(
    adj_position == 0 ~ 31, 
    TRUE ~ adj_position
  ))  %>%
    mutate(match = str_sub(pattern, adj_position, adj_position)) %>%
  filter(match == "#") %>%
  nrow(.)
 
}

slopes <- c(1, 3, 5, 7, 1)
row_ic <-list(1:nrow(data), 1:nrow(data), 1:nrow(data), 1:nrow(data), seq(1, nrow(data), by  = 2))

prod(map2_int(slopes, row_ic, get_counts, data = data))
```

    ## [1] 4385176320

# Day 4

``` r
data <- read_file(here::here("2020", "Inputs", "day_4.txt")) %>%
  as.data.frame(.) %>%
  rename(pattern = 1) %>%
  separate_rows(pattern, sep = "\n\n") %>%
  mutate(passport_id = row_number()) %>%
  separate_rows(pattern, sep = " |\n") %>%
  separate(pattern, into = c("key", "value"), sep = ":") %>%
  filter(!key %in% c('', 'cid')) %>% 
  mutate(is_present = !is.na(value)) %>%
  group_by(passport_id) %>%
  mutate(num_valid = sum(is_present)) %>%
  ungroup() %>%
  filter(!is.na(value)) %>%
  pivot_wider(names_from = "key", values_from = "value") %>%
  mutate(hgt_units = str_extract(hgt, "[a-z]{2}"), 
         hgt = str_extract(hgt, "[0-9]+")) %>%
  mutate(hgt = as.numeric(hgt)) %>%
    filter(num_valid == 7) 
```

    ## Warning: Expected 2 pieces. Missing pieces filled with `NA` in 1 rows [2098].

``` r
# part 1

data %>%
  count()
```

    ## # A tibble: 1 x 1
    ##       n
    ##   <int>
    ## 1   256

``` r
# part 2

data %>%
  filter(1920 <= byr & byr <= 2002 &
        2010  <=  iyr & iyr <= 2020 &
        2020 <=  eyr & eyr <=  2030 &
          !is.na(hgt_units) &
            ((hgt_units == "in" & hgt >= 59 & hgt <= 76) | 
      (hgt_units == "cm" & hgt >= 150 & hgt <= 193)) & str_detect(hcl, "#(([a-f])|([0-9])){6}") & 
        ecl %in% c("amb", "blu", "brn", "gry", "grn", "hzl", "oth")  & 
        str_detect(pid, "^[0-9]{9}$")) %>%
  count()
```

    ## # A tibble: 1 x 1
    ##       n
    ##   <int>
    ## 1   198

# Day 5

``` r
data <- read_lines(here::here("2020", "Inputs", "day_5.txt"))

num_cols <- 8
num_rows <- 128
row_sequence <- 0:(num_rows-1)
col_sequence <- 0:(num_cols-1)

halve_seq <- function(seat_sequence, string) {
  
  curr_indication <- substr(string, 1, 1)
  string <- substr(string, 2, nchar(string))
  
  if ((curr_indication == 'F') | (curr_indication == 'L')) {
    
    seat_sequence <- seat_sequence[1:(length(seat_sequence)/2)]
    
  } 
  
  else {
    
    seat_sequence <- seat_sequence[((length(seat_sequence)/2) + 1):length(seat_sequence)]
    
  }
  
  if (length(seat_sequence) == 1) {
    
    seat_sequence
    
  }
  
  else {
    
    halve_seq(seat_sequence, string)
    
  }
  
}

get_id <- function(row, col) {
  
  (row * 8) + col
  
}

 rows <- map_int(data, ~ halve_seq(row_sequence, .))
 cols <- map_int(data, ~ halve_seq(col_sequence, substr(., 8, 10)))
 ids  <- map2_dbl(rows, cols, ~ get_id(.x, .y))
 
 max(ids)
```

    ## [1] 871

``` r
 which(!seq(0:858)  %in% ids)
```

    ##  [1]   1   2   3   4   5   6   7   8   9  10  11 640

# Day 6

``` r
data <- read_file(here::here("2020", "Inputs", "day_6.txt")) %>%
  as.tibble(.) %>%
  separate_rows(value, sep = "\n\n") %>%
  mutate(group = row_number()) %>%
  separate_rows(value, sep = "\n") %>%
  group_by(group) %>%
  mutate(person = row_number()) %>%
  mutate(value = str_trim(value)) %>%
  ungroup()
```

    ## Warning: `as.tibble()` is deprecated as of tibble 2.0.0.
    ## Please use `as_tibble()` instead.
    ## The signature and semantics have changed, see `?as_tibble`.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_warnings()` to see where this warning was generated.

``` r
# Part 1

data %>%
  separate_rows(value, sep = "")   %>%
  filter(value!='') %>%
  distinct(value, group) %>%
  count(group) %>%
  summarise(sum(n))
```

    ## # A tibble: 1 x 1
    ##   `sum(n)`
    ##      <int>
    ## 1     6911

``` r
# Part 2

data %>%
  group_by(group) %>%
  mutate(num_people = n()) %>%
  separate_rows(value, sep = "")   %>%
  filter(value!='') %>%
  group_by(group, num_people, value) %>%
  summarise(letter_counts = n()) %>%
  ungroup() %>%
  filter(letter_counts == num_people) %>%
  distinct(group, value) %>%
  count(group) %>%
  summarise(sum(n))
```

    ## `summarise()` regrouping output by 'group', 'num_people' (override with `.groups` argument)

    ## # A tibble: 1 x 1
    ##   `sum(n)`
    ##      <int>
    ## 1     3473
