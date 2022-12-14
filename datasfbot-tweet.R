# Create Twitter token
datasfbot_token <- rtweet::rtweet_bot(
  api_key = Sys.getenv("TWITTER_CONSUMER_API_KEY"),
  api_secret = Sys.getenv("TWITTER_CONSUMER_API_SECRET"),
  access_token  = Sys.getenv("TWITTER_ACCESS_TOKEN"),
  access_secret = Sys.getenv("TWITTER_ACCESS_TOKEN_SECRET")
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
          sf::st_read(x[["downloadURL"]][i])
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
        return(NULL)
      }
    }
  }
  return(out)
}

available <- FALSE
while (!available) {
  sample_ind <- sample(1:nrow(dsf_geospatial_resources), 1)
  out <- get_data(dsf_geospatial_resources$distribution[[sample_ind]])
  if (!is.null(out)) {
    available <- TRUE
    break
  }
}

# Create map

m <- ggplot2::ggplot() +
  mapboxapi::layer_static_mapbox(
    location = sf::st_bbox(out),
    style_url = Sys.getenv("MAPBOX_DARK_STYLE"),
    access_token = Sys.getenv("MAPBOX_ACCESS_TOKEN")
  ) +
  ggplot2::theme_void() +
  ggplot2::theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

geom_sf2 <- purrr::partial(
  ggplot2::geom_sf,
  data = out,
  alpha = 0.6,
)

my_color <- sample(c("#FDBD01", "#FF00FF", "#50C878", "#b22222"), 1)

gt <- sf::st_geometry_type(out, FALSE)

if (gt %in% c("POINT", "MULTIPOINT", "MULTILINESTRING", "LINESTRING")) m <- m + geom_sf2(color = my_color)

if (gt %in% c("POLYGON", "MULTIPOLYGON")) {
  if (nrow(out) > 1000) {
    m <- m + geom_sf2(fill = my_color, color = NA) 
  } else {
    m <- m + geom_sf2(fill = my_color, color = "#808080") 
  }
}

temp_file <- tempfile(fileext = ".jpeg")
ggplot2::ggsave(temp_file, m)

# Create status message
status_msg_bits <- unlist(dsf_geospatial_resources[sample_ind, c("title", "theme", "modified", "landingPage")])
names(status_msg_bits)[4] <- "page"
names(status_msg_bits) <- tools::toTitleCase(names(status_msg_bits))
status_msg <- sapply(1:4, \(i) paste(names(status_msg_bits)[i], ":", status_msg_bits[i]))
status_msg <- paste(status_msg, collapse = "\n")

alt_text <- "A random dataset from DataSF visualized on a map of San Francisco."

# Tweet
rtweet::post_tweet(
  status         = status_msg,
  media          = temp_file,
  media_alt_text = alt_text,
  token          = datasfbot_token
)
