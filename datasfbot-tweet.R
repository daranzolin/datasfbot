# Create Twitter token
datasfbot_token <- rtweet::rtweet_bot(
  api_key       = Sys.getenv("TWITTER_CONSUMER_API_KEY"),
  api_secret    = Sys.getenv("TWITTER_CONSUMER_API_SECRET"),
  access_token  = Sys.getenv("TWITTER_BEARER_TOKEN")
  # access_secret = Sys.getenv("TWITTER_ACCESS_TOKEN_SECRET")
)

# Get geospatial resources
dsf_resources <- RSocrata::ls.socrata("https://data.sfgov.org/api/geospatial")
dsf_dist <- dsf_resources[["distribution"]] 
dsf_geospatial_inds <- vapply(dsf_dist, \(d) any(grepl("geospatial", d[["downloadURL"]])), TRUE)
dsf_geospatial_resources <- dsf_resources[dsf_geospatial_inds, c("landingPage", "modified", "title", "theme", "distribution")]
dsf_geospatial_resources$modified <- as.character(dsf_geospatial_resources$modified)

# Read data
get_data <- function(x) {
  cond <- TRUE
  urls <- x[["downloadURL"]]
  while (cond) {
    for (i in seq_along(urls)) {
      out <- tryCatch(
        {
          st_read(x[["downloadURL"]][i])
        },
        error = function(e) return(NULL)
      )
      Sys.sleep(3)
      if (!is.null(out)) {
        cond <- FALSE
        break
      }
      if (i == length(urls)) {
        cond <- FALSE
        message("No data available")
        break
      }
    }
  }
  return(out)
}

safe_get_data <- purrr::possibly(get_data, otherwise = NULL)

available <- FALSE
while (!available) {
  sample_ind <- sample(1:nrow(dsf_geospatial_resources), 1)
  out <- safe_get_data(dsf_geospatial_resources$distribution[[sample_ind]])
  if (!is.null(out)) {
    available <- TRUE
    break
  }
}

m <- ggplot2::ggplot() +
  ggspatial::annotation_map_tile(type = "cartolight") +
  ggplot2::geom_sf(data = out, alpha = 0.3) +
  ggplot2::labs(
    caption = "Map by @SFDataBot"
  ) +
  ggplot2::theme_void()

# Download the image to a temporary location
temp_file <- tempfile(fileext = ".jpeg")
ggplot2::ggsave(temp_file, m)

dsf_geospatial_resources[sample_ind,]

# Build the status message (text and URL)
status_msg <- unlist(dsf_geospatial_resources[sample_ind,1:4])[c(2:4, 1)]
status_msg <- paste(paste(tools::toTitleCase(names(status_msg)), ":", status_msg), collapse = "\n")

alt_text <- paste(
  "A random dataset from DataSF visualized on a map."
)

# Post the image to Twitter
rtweet::post_tweet(
  status         = status_msg,
  media          = temp_file,
  media_alt_text = alt_text,
  token          = datasfbot_token
)
