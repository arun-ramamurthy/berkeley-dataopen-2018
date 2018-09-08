# Libraries (used across most functions, loaded here for convenience)
library(magrittr)
library(purrr)
library(dplyr)
library(pryr)
library(ggplot2)
library(viridis)
library(scales)
library(forcats)
library(stringr)
library(lubridate)
library(tidyr)
library(readr)
library(jsonlite)
library(rvest)
library(tictoc)
library(assertthat)
library(feather)
library(broom)
library(glue)
library(here)
library(lazyeval)

extract <- magrittr::extract
here <- here::here
flatten <- purrr::flatten
pluck <- purrr::pluck

# Macros
`%notin%` = function(x,y) !(x %in% y)
intersection = function(a, b) unique(a[a %in% b], b[b %in% a])
`%between%` = function(x, pair) ((x >= pair[[1]]) & (x <= pair[[2]])) | ((x >= pair[[1]]) & (x <= pair[[2]]))
unilen = function(x) {x %>% unique() %>% length()}
doesnt.equal = function(x, y) !equals(x,y)
isnt.na = function(x){! is.na(x)}
isnt_null = function(x) {! is_null(x)}
doesnt_exist = function(x){! exists(x)}
is_named = function(x){names(x) %>% is_null() %>% not()}
create_if_needed = function(x){if (! dir.exists(x)) dir.create(x)}
dir.rm = function(x){x %>% unlink(recursive = T)}
dir.recreate = function(x){dir.rm(x); dir.create(x)}
zero_length = . %>% length() %>% not()
nonzero_length = . %>% zero_length() %>% not()
nodata = function(x){!length(x) || is.null(x) || ifelse(is.data.frame(x), nrow(x) == 0, F)}
hasdata = function(x){! nodata(x)}
first = function(x){x %>% head(1)}
last = function(x){x %>% tail(1)}
firstrow = function(df){df %>% slice(1)}
lastrow = function(df){df %>% slice(nrow(.))}
which.min.tied <- function(v){m <- min(v); (1:length(v))[v == m]}
pick <- function(v, n = 1){v %>% magrittr::extract(sample.int(length(.), size = n, replace = F))}
selfname = function(lst){lst %>% set_names()}
safe_names = function(v) {names(v) %||% v}
date <- function() {str_sub(now(), end = 10)}
logger = function(msg, out_file){cat(now() + " | " + msg, file = out_file, fill = T, append = T)}
generate_logger = function(out_file){unlink(out_file); function(err) {logger(err, out_file)}}
pad0 <- function(n) {ifelse(n < 10, "0" + n, "" + n)}
rmse = function(y, y_hat = 0) {sqrt(sum((y - y_hat)^2)/length(y))}
mae = function(y, y_hat = 0) {sum(abs(y - y_hat)) / length(y)}
ggfilter <- function(condition){function(d) {d %>% filter_(condition)}}
capture_numeric <- function(str) {str %>% str_extract(pattern = "\\d+") %>% as.numeric}
andp <- function(...) {list(...) %>% reduce(and)}
orp <- function(...) {list(...) %>% reduce(or)}
lcirc <- function(v) {v %>% lead(default = first(v))}
rcirc <- function(v) {v %>% lag(default = last(v))}
as_percentage <- function(prop, digits = 1){round(1e2 * prop, digits = digits) + "%"}
check_setdiffs <- function(a, b){diffab <- setdiff(a, b); diffba <- setdiff(b, a); list(diffab, diffba)}
argmax <- function(v, tie = NA_character_) {if (nonzero_length(v)) v %>% safe_names %>% extract(v == max(v)) %>% {ifelse(length(.) > 1, tie, .)} else NA_character_}
argmin <- function(v, tie = NA_character_) {if (nonzero_length(v)) v %>% safe_names %>% extract(v == min(v)) %>% {ifelse(length(.) > 1, tie, .)} else NA_character_}
shuffle_df <- function(df){df %>% slice(sample(1:n()))}

# Custom Functions
`+` = function(x, y) {
  if (is.character(x) || is.character(y)) {
    return(paste(x , y, sep = ""))
  } else {
    .Primitive("+")(x, y)
  }
}

monetary_to_double <- function(str) {
  case_when(is.na(str) ~ NA_real_,
            str_detect(str, "B") ~ multiply_by(capture_numeric(str), 1e9),
            str_detect(str, "M") ~ multiply_by(capture_numeric(str), 1e6),
            TRUE ~ capture_numeric(str))
}
