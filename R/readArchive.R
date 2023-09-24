read_archive_json <- function(path) {
  lines <- brio::read_lines(path)
  lines[1] <- sub("^[^{[]+([{[])", "\\1", lines[1])
  
  jsonlite::fromJSON(
    txt = lines,
    simplifyVector = TRUE,
    simplifyDataFrame = FALSE,
    simplifyMatrix = FALSE
  )
}

names(
read_archive_json("data/manifest.js")
)

tweets <- read_twitter_data(manifest, "tweets")
manifest <- read_archive_json("data/manifest.js")
simplify_twitter_data(tweets)
#
#*****************************************************************************************
#
# Srction Description
#
#*****************************************************************************************
#
#' Read the Twitter Archive JSON
#'
#' @param path Path to a Twitter archve `.js` file
read_archive_json <- function(path) {
  lines <- brio::read_lines(path)
  lines[1] <- sub("^[^{[]+([{[])", "\\1", lines[1])
  
  jsonlite::fromJSON(
    txt = lines,
    simplifyVector = TRUE,
    simplifyDataFrame = FALSE,
    simplifyMatrix = FALSE
  )
}

#' Read an twitter archive data item
#'
#' @param manifest The list from `manifest.js`
#' @param item The name of an item in the manifest
read_twitter_data <- function(manifest, item) {
  manifest$dataTypes[[item]]$files |>
    purrr::transpose() |>
    purrr::pmap(\(fileName, ...) read_archive_json(fileName))
}

#' Simplify the data, if possible and easy
#'
#' @param x A list of lists as returned from `read_twitter_data()`
#' @param simplifier A function that's applied to each item in the
#'   list of lists and that can be used to simplify the output data.
simplify_twitter_data <- function(x, simplifier = identity) {
  x <- purrr::flatten(x)
  item_names <- x |> purrr::map(names) |> purrr::reduce(union)
  if (length(item_names) > 1) return(x)
  
  x |>
    purrr::map(item_names) |>
    purrr::map_dfr(simplifier)
}

