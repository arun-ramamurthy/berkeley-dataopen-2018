### This script loads several R functions to help with text munging.

# Load convenience functions,
library(here)
here = here::here
library(glue)
library(pryr)
## For common R-specific tasks
source(glue("{here()}/CODE/convenience.R"))

try(NUMBER_DETECTOR %<c-% "[0-9.]+", silent = T)
try(MONEY_DETECTOR %<c-% "\\$[0-9,.]+", silent = T)
try(EMAIL_DETECTOR %<c-% "\\b[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\\.[a-zA-Z0-9-.]+\\b", silent = T)
try(PHONE_DETECTOR %<c-% "\\b(\\+[:digit:])?[ .-]?(\\([0-9]{0,3}\\))?[0-9. -]{7,15}[0-9]\\b", silent = T)

try(PUNCTUATION %<c-% c("", " ", ".", "!", ","), silent = T)
try(COMMON_STOPWORDS %<c-% unique(stop_words$word), silent = T) ## From tidytext


#' Convenience wrapper for rvest scraping.
#'
#' \code{scrape} uses \code{html_nodes} to scrape from the given link.
#'
#' @param link URL, as a character string
#' @param css CSS path, as a character string
#' @param xpath XPath, as a character string. Takes priority over css
#' @return scraped data, as a character vector
scrape <- function(link, css, xpath = NULL) {
  if (is.null(xpath))
    read_html(link) %>%
    html_nodes(css) %>%
    html_text()
  else
    read_html(link) %>%
    html_nodes(xpath = xpath) %>%
    html_text()
}

#' A convenient wrapper for \code{textclean::collapse} with sep = "\n".
#'
#' Uses \code{textclean::collapse} to collapse the passed vector into a single string, separated by newlines.
#'
#' @param v character vector
#' @param last terminate output with this string (optional)
collapsen <- function(v, last = "") {
  collapse(v, sep = "\n", last = last)
}

#' A convenient wrapper for \code{str_replace} that replaces with empty strings.
#'
#' Uses \code{str_replace} to remove all instances of pattern in the passed string.
#'
#' @param string character string, or character vector
#' @param pattern Pattern to look for, as a character string
str_replace0 <- function(string, pattern) {
  str_replace(string, pattern, replacement = "") %>% str_trim()
}

#' A convenient wrapper for \code{str_replace_all} that replaces with empty strings.
#'
#' Uses \code{str_replace_all} to remove all instances of pattern in the passed string.
#'
#' @param string character string, or character vector
#' @param pattern Pattern to look for, as a character string
str_replace_all0 <- function(string, pattern) {
  str_replace_all(string, pattern, replacement = "") %>% str_trim()
}

#' Approximate match for canonical element in non-canonical set
#'
#' \code{approximate_match} finds the first element in fanon that (approximately) matches canonical element
#'
#' @param canon_elem a canonical search term, as a character string
#' @param fanon a list of strings to search within, as a character vector
#' @param strictness maximal Levenshtein distance to still be considered a "match"
#' @param LOG A LOG function for logging a miss
#' @return a character string
approximate_match <- function(canon_elem, fanon, strictness = 5, LOG = NULL) {
  dists <- adist(canon_elem, fanon) %>% `[`(1, ) %>% as.vector()
  fanon %<>% subset(dists <= strictness)
  dists %<>% subset(dists <= strictness)
  match_locs <- which.min.tied(dists)
  matches <-
    if (canon_elem %in% fanon) canon_elem else
      if (length(match_locs)) fanon[match_locs] else
        NA_character_
  if (is.na(matches) && isnt_null(LOG))
    glue("{canon_elem} NOT FOUND in non-canonical set!") %>% LOG()
  return(matches)
}

#' Diagnosis of fuzzy matching between two character vectors.
#'
#' \code{fuzzy_joiner} diagnoses approximate matches in the form of a dataframe.
#' Each row delineates a unique element from the canonical set, and its closest match in the non-canonical set.
#'
#' @param canon canonical strings, as a character vector
#' @param fanon non-canonical strings, as a character vector
#' @param strictness maximal Levenshtein distance to still be considered a "match"
#' @param LOG A LOG function for logging misses
#' @return dataframe marking misses and (even approximate) matches
fuzzy_joiner <- function(canon, fanon, strictness = 5, LOG = NULL) {
  fuzzy_joiner_singleton <- function(canon_elem, fanon, strictness, LOG) {
    LOG(glue("Matching {canon_elem}"))
    data_frame(
      canon = canon_elem,
      fanon = approximate_match(tolower(canon_elem), tolower(fanon), strictness, LOG),
      `miss?` = is.na(fanon),
      `hit?` = ! `miss?`,
      `perfect?` = if_else(`miss?`, F, canon == fanon)
    )
  }
  a <- canon %>% unique() %>% subset(. != "")
  b <- fanon %>% unique() %>% subset(. != "")
  joiner <- a %>% map_dfr(~ fuzzy_joiner_singleton(., b, strictness, LOG))
  return(joiner)
}

#' Non-canonical strings that match with canonical strings.
#'
#' \code{pull_canonical_matches} uses \code{fuzzy_joiner} to extract the "fuzzy set" that encapsulates the canonical set.
#' Each outputed string is a non-canonical string that discovered a match in the canonical set.
#'
#' @param canon canonical strings, as a character vector
#' @param fanon non-canonical strings, as a character vector
#' @param strictness maximal Levenshtein distance to still be considered a "match"
#' @param LOG A LOG function for logging misses
#' @return character vector of matched non-canonical forms
pull_canonical_matches <- function(canon, fanon, strictness = 5, LOG = NULL) {
  fuzzy_joiner(canon, fanon, strictness, LOG) %>% filter(`hit?`) %>% pull(fanon) %>% unique()
}

#' Is the search word near the target word in the text?
#'
#' Computes the distance between all instances of the target word and all instances of the search word,
#' then returns TRUE iff any distances <= N. If either word not found, returns FALSE. This is a symmetric operation.
#' Can test equality in a variety of ways:
#' * "ignore_case" - string equality, ignoring case (default)
#' * "exact" - string equality, case-specific
#' * "regex" - regex-style matching (can be slow)
#' * "switch" - allows for target_word and search_word to be vectors of strings, and tests all pairs between them
#' * "switch_ignore_case" - like "switch", but ignores case
#'
#'  @param text string to search within
#'  @param target_word known word in the text
#'  @param search_word word to search for, in comparison to target
#'  @param N maximum distances between target and search to still be considered "near"
#'  @param type one of “ignore_case” (default), “exact”, “regex”, “regex_ignore_case”, “switch”, “switch_ignore_case”
#'  @return logical vector
near <- function(text, target_word, search_word, N = 4, type = c("ignore_case", "exact", "regex", "regex_ignore_case", "switch", "switch_ignore_case")) {
  type = match.arg(type)
  equals <-
    if (type == "exact") magrittr::equals else
      if (type == "regex") stringr::str_detect else
        if (type == "regex_ignore_case") function(a, b) {str_detect(tolower(a), tolower(b))} else
          if (type == "ignore_case") function(a, b) {toupper(a) == toupper(b)} else
            if (type == "switch") function(a, b) {a %>% map_lgl(~ . %in% b)} else
              if (type == "switch_ignore_case") function(a, b) {tolower(a) %>% map_lgl(~ . %in% tolower(b))} else
                stop("Wrong type given! (" + type + ")")
  near_singleton <- function(sentence, target_word, search_word) {
    words <- split_words(sentence)
    target_locs <-  which(equals(words, target_word))
    search_locs <- which(equals(words, search_word))
    distances <-
      cross2(target_locs, search_locs, .filter = function(x, y) {abs(x - y) %>% {. > N || . == 0}})
    return(! is_empty(distances))
  }
  return(text %>% map_lgl(~ near_singleton(., target_word, search_word)))
}

#' How far is the target word from the search word in the text?
#'
#' Computes the distance between all instances of the target word and all instances of the search word.
#' This is a symmetric operation.
#' Can test equality in a variety of ways:
#' * "ignore_case" - string equality, ignoring case (default)
#' * "exact" - string equality, case-specific
#' * "regex" - regex-style matching (can be slow)
#' * "switch" - allows for target_word and search_word to be vectors of strings, and tests all pairs between them
#' * "switch_ignore_case" - like "switch", but ignores case
#'
#'  @param text string to search within
#'  @param target_word known word in the text
#'  @param search_word word to search for, in comparison to target
#'  @param type one of “ignore_case” (default), “exact”, “regex”, “regex_ignore_case”, “switch”, “switch_ignore_case”
#'  @return list of double vectors
word_distance <- function(text, target_word, search_word, type = c("ignore_case", "exact", "regex", "regex_ignore_case", "switch", "switch_ignore_case")) {
  type = match.arg(type)
  equals <-
    if (type == "exact") magrittr::equals else
      if (type == "regex") stringr::str_detect else
        if (type == "regex_ignore_case") function(a, b) {str_detect(tolower(a), tolower(b))} else
          if (type == "ignore_case") function(a, b) {toupper(a) == toupper(b)} else
            if (type == "switch") function(a, b) {a %>% map_lgl(~ . %in% b)} else
              if (type == "switch_ignore_case") function(a, b) {toupper(a) %>% map_lgl(~ . %in% toupper(b))} else
                stop("Wrong type given! (" + type + ")")
  word_distance_singleton <- function(sentence, target_word, search_word) {
    words <- split_words(sentence)
    target_locs <-  which(equals(words, target_word))
    search_locs <- which(equals(words, search_word))
    distances <-
      cross2(target_locs, search_locs) %>%
      map_dbl(~ abs(.[[1]] - .[[2]])) %>%
      discard(. == 0)
    return(distances)
  }
  return(text %>% map(~ word_distance_singleton(., target_word, search_word)))
}

#' Splits the text into words.
#'
#' Splits with \s, and discards punctuation and spaces.
#'
#' @param text character string
#' @param flatten cast output as character vector?
#' @return character vector
split_words <- function(text, flatten = T) {
  words <-
    str_split(text, pattern = "\\s") %>%
    map(~ str_trim(.)) %>%
    map(~ discard(., . %in% PUNCTUATION))
  if (flatten) words %<>% flatten_chr()
  return(words)
}

#' Keeps only exact instances of stopwords in words (exact matches only).
#'
#' \code{keep_exact_instances} uses \code{grepl} to retain words found exactly in stopwords.
#' Ignores case.
#'
#' @param words character vector
#' @param stopwords character vector
#' @return subvector of words
keep_exact_instances <- function(words, stopwords = "") {
  words %>%
    discard(~ is.na(.)) %>%
    keep(~ toupper(.) %in% toupper(stopwords))
}

#' Discards exact instances of stopwords in words (exact matches only).
#'
#' \code{discard_exact_instances} uses \code{grepl} to discard words found exactly in stopwords.
#' Ignores case.
#'
#' @param words character vector
#' @param stopwords character vector
#' @return subvector of words
discard_exact_instances <- function(words, stopwords = "") {
  words %>%
    discard(~ is.na(.)) %>%
    keep(~ toupper(.) %notin% toupper(stopwords))
}

#' Keeps only instances of stopwords in words (whole matches only).
#'
#' \code{keep_wholeword_instances} uses \code{grepl} to retain words which contain a word in stopwords.
#' Ignores case.
#'
#' @param words character vector
#' @param stopwords character vector
#' @return subvector of words
keep_wholeword_instances <- function(words, stopwords = "") {
  words %>%
    discard(~ is.na(.)) %>%
    keep(~ str_detect(toupper(.), pattern = "\\b" + toupper(stopwords) + "\\b") %>% reduce(`||`))
}

#' Discards instances of stopwords in words (whole matches only).
#'
#' \code{discard_wholeword_instances} uses \code{grepl} to retain words which contain a word in stopwords.
#' Ignores case.
#'
#' @param words character vector
#' @param stopwords character vector
#' @return subvector of words
discard_wholeword_instances <- function(words, stopwords = "") {
  words %>%
    discard(~ is.na(.)) %>%
    discard(~ str_detect(toupper(.), pattern = "\\b" + toupper(stopwords) + "\\b") %>% reduce(`||`))
}

#' Keeps only instances of stopwords in words (substrings match).
#'
#' \code{keep_substring_instances} uses \code{grepl} to retain words containing stopwords.
#' Ignores case, and matches with substrings.
#'
#' @param words character vector
#' @param stopwords character vector
#' @return subvector of words
keep_substring_instances <- function(words, stopwords = "") {
  words %>%
    discard(~ is.na(.)) %>%
    keep(~ right_substring_found(., stopwords))
}

#' Strips away instances of stopwords in words (substrings match).
#'
#' \code{discard_substring_instances} uses \code{grepl} to discard words containing stopwords.
#' Ignores case, and matches with substrings.
#'
#' @param words character vector
#' @param stopwords character vector
#' @return subvector of words
discard_substring_instances <- function(words, stopwords = "") {
  words %>%
    discard(~ is.na(.)) %>%
    discard(~ right_substring_found(., stopwords))
}

#' Returns TRUE iff word is a substring for ANY of the passed strings.
#'
#' \code{left_substring_found} uses \code{grepl} to detect the specified word.
#'
#' @param word character string
#' @param set character vector
#' @return TRUE iff any substring matches found in set
left_substring_found <- function(word, set) {
  set %>% unique %>% grepl(pattern = word, ignore.case = T) %>% reduce(`||`)
}

#' Returns TRUE iff any strings are a substring of the passed word.
#'
#' \code{right_substring_found} uses \code{grepl} to detect the specified word.
#'
#' @param word character string
#' @param set character vector
#' @return TRUE iff any substring matches found in word
right_substring_found <- function(word, set) {
  set %>% unique %>% map(~ grepl(word, pattern = ., ignore.case = T)) %>% reduce(`||`)
}

#' Returns the pattern that was detected for each word.
#'
#' Looks for the first match in \code{patterns} that matches each word.
#' Optionally, supply \code{outputs} to yield the corresponding output to the match.
#'
#' @param words a character vector
#' @param patterns a character vector
#' @param outputs a character vector
#' @return the outputs at the index of each first match
get_matches <- function(words, patterns, outputs = patterns) {
    raw_matches <- words %>% map(~ outputs[str_detect(., patterns)])
    matches <- raw_matches %>% map_chr(~ purrr::pluck(., 1L, .default = NA_character_))
    return(matches)
}
