library(readr)
library(dplyr)
library(ggplot2)
library(xkcd)
library(scales)
library(ggrepel)
library(Cairo)

#' @title Return the current year as a numeric variable
today_year <- function() {as.numeric(substr(Sys.Date(), 1, 4))}

#' @title A function decorator
#' @description A decorating function that returns NA for all values outside a given range
#' @param f a function that accepts vector a returns a vector of the same length
#' @param xmin the minimum value to use from f()
#' @param xmax the maximum value to use from f()
#' @param ... other arguments passed to f()
#' @return the result of f(), with NA substituted for all values of f() outside xmin and xmax
#' @references \url{http://stackoverflow.com/a/22695091/393354}
rwrap <- function(f, xmin, xmax, ...) {
  ff <- function(x, ...) {
    y = f(x, ...)
    y[x > xmax] <- NA
    y[x < xmin] <- NA
    return(y)
  }
}

#' @title pub_year_fun
#' @description Transforms a vector of publication years to a series of plot x-axis positions with
#'   increasing resolution near the present
#' @param year_vec A vector of publication years, either numeric or Date
#' @return A numeric vector of x-axis positions
#' @details Creates a sequence of 
pub_year_fun <- function(year_vec) {
  stories_range <- range(year_vec)
  x_pos <- as.numeric(substr(Sys.Date(), 1, 4))
  if(stories_range[2] - stories_range[1] > 1000)
    x_pos <- c(x_pos, seq(4 * stories_range[1] - stories_range[2], stories_range[2], by = 100))
  if(stories_range[2] - stories_range[1] > 100)
    x_pos <- c(x_pos, seq(max(500, stories_range[1]), stories_range[2], by = 10))
  if(stories_range[2] - stories_range[1] > 10)
    x_pos <- c(x_pos, seq(max(1950, stories_range[1]), stories_range[2], by = 1))
  
  x_pos <- unique(sort(x_pos))
  return(x_pos)
}

#' @title log transform of \code{abs(x)}
#' @param x A numeric vector of values to transform. Negative values are transformed as \code{abs(x)}.
#' @return A numeric vector of transformed values.
log_y_fun <- function(x) {
  log(abs(x) + 1)
}

#' @title Create a log transform of values, transforming zeroes (0) to zeroes
#' @description Accepts a vector of numeric values and returns a log transform of those values.
#'   Zeroes (0) will be transformed to 0 rather than produce errors or non-numeric results.
#'   Negative values will be transformed as their absolute value using \link{\code{log_y_fun()}} 
#'   and given a negative sign.
#' @param x A numeric vector of values to transform.
#' @return A numeric vector of transformed values.
#' @example y_trans(c(-10, 0, 1, 10))
y_trans <- function(x) {
  y <- x
  for (i in 1:length(x)) {
    if(is.na(x[i])) {y[i] <- NA}
    #else if(x[i] > as.numeric(substr(Sys.Date(), 1, 4))) {y[i] <- x[i]}
    else if(is.numeric(x[i])){ 
      if(isTRUE(all.equal(x[i], c(0.), check.names = FALSE))) {y[i] <- x[i]}
      else {y[i] <- sign(x[i]) * log_y_fun(x[i])}
    }
    else {stop(paste0("x[", i, "] failed is.numeric()."))}
  }
  return(y)
}

#' @title Provides the inverse log transform of \code{y_trans()}
#' @param x A numeric vector of values to transform
#' @return A numeric vector of transformed values
y_inv <- function(x) {
  #print(paste("10^ :", x))
  y <- x
  for (i in 1:length(x)) {
    if(is.na(x[i])) {y[i] <- NA}
    else if(is.numeric(x[i])) {
      if(isTRUE(all.equal(x[i], c(0.), check.names = FALSE))) {y[i] <- x[i]}
      else {y[i] <- sign(x[i]) * (exp(abs(x[i])) - 1)}
    }
    else {stop(paste0("x[", i, "] failed is.numeric()."))}
  }
  return(y)
}

#' @title The y scale transformation function used by \code{ggplot2::scale_y_continuous()}
#' @return A y-scale transformation function used by ggplot2
y_scale_trans <- function() trans_new("y_scale", y_trans, y_inv)

#' @title Given a numeric vector of years preceding today's date, return a log transform based on years from present
#' @param x A numeric vector of years.
#' @return A numeric vector.
log_x_fun <- function(x) {
  y <- -log(today_year() - x[x <= today_year()] + 1)
  return(y)
}

#' @title Creates a linear transform of a vector of years based on \code{y = slope * x + intercept}
#' @param x The vector of years to transform
#' @param slope The slope used for the transform
#' @param intercept The intercept used for the transform
lin_x_fun <- function(x, slope, intercept) {
  y <- slope * x + intercept
  return(y)
}

#' @title Create an inverse log transformation for the x axis data
#' @param x A numerical vector of x-axis plot coordinates
#' @return A numerical vector of linearly-scaled years
log_x_inv_fun <- function(x) {
  y <- as.integer(round(today_year() + 1 - exp(-x), 0))
  return(y)
}

#' @title Convert linear transformed x back to x in years
#' @param x The transformed x data to convert back to years
#' @param slope The slope of the original line in transformed/years
#' @param intercept The y intercept of the transformed line
lin_x_inv_fun <- function(x, slope, intercept) {
  # y : years
  # x : transformed
  # y = mx + b
  # m: years / transformed
  # b: years
  # m = (today - lin_min) / (lin_x_fun(today, slope, intercept) - line_x_fun(lin_min, slope, intercept))
  # b = y - mx, x = 0
  # b = 0 - m * 2017
  inv_slope <- 1/slope
  inv_intercept <- today_year() #- inv_slope * 0
  y <- inv_slope * x + inv_intercept
  return(y)
}

#' @title Sets up breaks for the publication year transformations
params_x_trans <- function() {
  x1 = 1960
  slope <- (0 - log_x_fun(x1)) / (today_year() - x1)
  return(list(
    x_slope = slope,
    x_intercept = 0 - slope * today_year(),
    lin_max = Inf, #today_year(),
    lin_min = 1960,
    log_max = 1960 - 1,
    log_min = -Inf
  ))
}

#' @title Transform for the x scale, with recent years transformed linearly and older years transformed logarithmically
#' @param x A numeric vector of years to transform
#' @return A numeric vector of transformed years
x_trans <- function(x) {
  # line1: linear for 1960 to present-day, effectively stretching the right side of the x-axis
  line1 <- rwrap(lin_x_fun, params_x_trans()$lin_min, params_x_trans()$lin_max)(x, params_x_trans()$x_slope, params_x_trans()$x_intercept)
  # line2: -log for -Inf to 1960, foreshortening time on the left side of the axis
  line2 <- rwrap(log_x_fun, params_x_trans()$log_min, params_x_trans()$log_max)(x)
  # The rwrap function leaves NA in line1 for dates <1960, so we can substitute in the log transform
  line1[is.na(line1)] <- line2[is.na(line1)]
  
  return(line1)
}

#' @title Inverse transformation for the x scale
#' @param x A numeric vector of values to transform to years
#' @return A numeric vector of years
x_inv <- function(x) {
  # linear portion
  inv_min <- lin_x_fun(params_x_trans()$lin_min, params_x_trans()$x_slope, params_x_trans()$x_intercept)
  inv_max <- lin_x_fun(params_x_trans()$lin_max, params_x_trans()$x_slope, params_x_trans()$x_intercept)
  line1 <- rwrap(lin_x_inv_fun, inv_min, inv_max)(x, params_x_trans()$x_slope, params_x_trans()$x_intercept)
  
  # logarithmic portion
  inv_min <- log_x_fun(params_x_trans()$log_min)
  inv_max <- log_x_fun(params_x_trans()$log_max)
  line2 <- rwrap(log_x_inv_fun, inv_min, inv_max)(x)
  
  # Combine the two portions of the scale and return a complete scale transformation
  line1[is.na(line1)] <- line2[is.na(line1)]
  return(line1)
}

#' @title The x scale transformation function used by \code{ggplot2::scale_x_continuous()}
#' @return A x-scale transformation function used by ggplot2
x_scale_trans <- function() trans_new("x_scale", x_trans, x_inv)

#' @title The labeller function for marking tick marks, used by \code{scale_x_continuous()}
#' @param x A numeric vector of non-transformed label values
#' @return A numeric vector of values used for labeling the x axis
x_labeller <- function(x) {
  as.integer(round(x, 0))
}

# Set up the "this graphic" label
origin_df <- data_frame(pub_year = as.numeric(substr(Sys.Date(), 1, 4)),
                        mid_year = 0,
                        Movie = c("this graphic"))

# Load data from csv, 
# create range in years from today's date for when the story takes place,
# and store the mean year for when the story takes place.
stories_df <- read_csv("data-raw/stories.csv") %>% 
  mutate(lo = lower - Year,
         up = upper - Year) %>%
  rowwise() %>% 
  mutate(mid_year = mean(c(lo, up))) %>% 
  rename(pub_year = Year)

# Create a dataframe for annotation lines on the plot
# 
lines_df <- data_frame(pub_year = pub_year_fun(stories_df$pub_year),
                       upper = as.numeric(substr(Sys.Date(), 1, 4)) - pub_year,
                       lower = -0.5 * (as.numeric(substr(Sys.Date(), 1, 4)) - pub_year),
                       origin = 0)

# Generate the plot
ggplot(stories_df) +
  aes(x = pub_year) +
  geom_line(data = lines_df, aes(y = upper), color = muted("blue"), alpha = 0.5) +
  geom_line(data = lines_df, aes(y = lower), color = muted("red"), alpha = 0.5) +
  geom_line(data = lines_df, aes(y = origin), color = "grey") +
  #geom_errorbar(aes(ymin = lo, ymax = up), color = "gray80") +
  #geom_point(aes(y = mid_year), alpha = 0.3, shape = 1) +
  #geom_text(aes(y = mid_year, label = Movie), alpha = 0.75, check_overlap = FALSE, size = 2, family = "Gill Sans") +
  geom_point(data = origin_df, aes(y = mid_year), shape = 4) +
  geom_text(data = origin_df, aes(y = mid_year, label = Movie), 
            alpha = 0.75, size = 2.25, family = "Gill Sans", 
            position = position_nudge(x = 0, y = 0.5), hjust = 0, vjust = 0.5) +
  geom_text_repel(aes(y = mid_year, label = Movie), 
                  alpha = 0.75, size = 2.25, family = "Gill Sans", 
                  box.padding = unit(0.15, "lines"), segment.color = "gray30") +
  annotate(geom = "text", y = max(lines_df$upper), x = min(stories_df$pub_year) -(max(stories_df$pub_year) - min(stories_df$pub_year))/4, 
           label = "Speculative futures", 
           angle = 90, hjust = 1, size = 3.2, fontface = "italic", family = "Gill Sans") +
  annotate(geom = "text", y = max(lines_df$upper), x = min(stories_df$pub_year), 
           label = "Still possible", 
           hjust = 0, vjust = 0, size = 3, color = muted("green"), fontface = "italic", family = "Gill Sans") +
  annotate(geom = "text", y = (mean(lines_df$upper)/2), x = min(stories_df$pub_year), 
           label = "Obsolete", 
           hjust = 0, vjust = 0.5, size = 3, color = muted("red"), fontface = "italic", family = "Gill Sans") +
  annotate(geom = "text", y = -5, x = min(stories_df$pub_year), 
           label = "May read as\ncontemporary fiction", 
           hjust = 0, size = 3, color = muted("blue"), fontface = "italic", family = "Gill Sans") +
  annotate(geom = "text", y = min(lines_df$lower), x = min(stories_df$pub_year), 
           label = "Recognizable as\nhistorical fiction", 
           hjust = 0, size = 3, color = muted("purple"), fontface = "italic", family = "Gill Sans") +
  scale_y_continuous(trans = "y_scale",
                     breaks = c(-10000, -1000, -100, -10, 0, 10, 100, 1000, 10000, 100000, 1000000, 10000000),
                     name = "Years Set in the Past / Future",
                     labels = comma) +
  scale_x_continuous(trans = "x_scale", 
                     breaks = c(-2000, 0, 1000, 1500, 1800, 1900, 1960, 1970, 1980, 1990, 2000, 2010, today_year()),
                     labels = x_labeller,
                     name = "Date of Publication") +
  coord_cartesian(xlim = c(2 * min(stories_df$pub_year) - max(stories_df$pub_year), max(stories_df$pub_year))) +
  labs(title = "Stories of the past and future",
       subtitle = "A visualization based on XKCD's concept",
       caption = "Based on xkcd 1491, produced in R (R Core Team 2016) CC BY-SA 2018 Thomas Hopper") +
  theme_minimal() +
  theme(text = element_text(family = "Gill Sans", size = 14),
        plot.caption = element_text(size = 6),
        panel.grid = element_line(color = "grey 90"),
        panel.grid.major = element_line(size = 0.5),
        panel.grid.minor = element_line(size = 0.1))
# Save the plot as png
ggsave("figs/stories_past_future.png", type = "cairo-png", dpi = 150, width = 10, height = 10, units = "in")
ggsave("figs/stories_past_future.pdf", device = cairo_pdf, width = 10, height = 10, units = "in")
