library(readr)
library(dplyr)
library(ggplot2)
library(xkcd)
library(scales)
library(ggrepel)

stories_df <- read_csv("data-raw/stories.csv") %>% 
  mutate(lo = lower - Year,
         up = upper - Year) %>%
  rowwise() %>% 
  mutate(mid_year = mean(c(lo, up))) %>% 
  rename(pub_year = Year)

today_year <- function() {as.numeric(substr(Sys.Date(), 1, 4))}

#' @references \url{http://stackoverflow.com/a/22695091/393354}
rwrap <- function(f, xmin, xmax, ...) {
  ff <- function(x, ...) {
    y = f(x, ...)
    y[x > xmax] <- NA
    y[x < xmin] <- NA
    return(y)
  }
}

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

lines_df <- data_frame(pub_year = pub_year_fun(stories_df$pub_year),
                       upper = as.numeric(substr(Sys.Date(), 1, 4)) - pub_year,
                       lower = -1.5 * (as.numeric(substr(Sys.Date(), 1, 4)) - pub_year),
                       origin = 0)

log_y_fun <- function(x) {
  log(abs(x) + 1)
}

y_trans <- function(x) {
  #print(paste("log10 :", x))
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
y_scale_trans <- function() trans_new("y_scale", y_trans, y_inv)

log_x_fun <- function(x) {
  y <- -log(today_year() - x[x <= today_year()] + 1)
  return(y)
}

lin_x_fun <- function(x, slope, intercept) {
  y <- slope * x + intercept
  return(y)
}

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

trans_x_fun <- function(x) {
  # line1: linear for 1960 to present-day, effectively stretching the right side of the x-axis
  line1 <- rwrap(lin_x_fun, params_x_trans()$lin_min, params_x_trans()$lin_max)(x, params_x_trans()$x_slope, params_x_trans()$x_intercept)
  # line2: -log for -Inf to 1960, foreshortening time on the left side of the axis
  line2 <- rwrap(log_x_fun, params_x_trans()$log_min, params_x_trans()$log_max)(x)
  # The rwrap function leaves NA in line1 for dates <1960, so we can substitute in the log transform
  line1[is.na(line1)] <- line2[is.na(line1)]
  
  return(line1)
}

inv_x_fun <- function(x) {
  # Not working
  inv_min <- lin_x_fun(params_x_trans()$lin_min, params_x_trans()$x_slope, params_x_trans()$x_intercept)
  inv_max <- lin_x_fun(params_x_trans()$lin_max, params_x_trans()$x_slope, params_x_trans()$x_intercept)
  line1 <- rwrap(lin_x_inv_fun, inv_min, inv_max)(x, params_x_trans()$x_slope, params_x_trans()$x_intercept)
  # Working
  inv_min <- log_x_fun(params_x_trans()$log_min)
  inv_max <- log_x_fun(params_x_trans()$log_max)
  line2 <- rwrap(log_x_inv_fun, inv_min, inv_max)(x)
  line1[is.na(line1)] <- line2[is.na(line1)]
  return(line1)
}

x_scale_trans <- function() trans_new("x_scale", trans_x_fun, inv_x_fun)

x_labeller <- function(x) {
  as.integer(round(x, 0))
}

#x <- c(-2000, -1000, -500, 0, 1000, 1500, 1800, 1900, 1960, 1980, 1990, 2000, 2010, 2015, 2016, 2017, 2020)
#x_inv <- trans_x_fun(x)
ggplot(stories_df) +
  aes(x = pub_year) +
  geom_line(data = lines_df, aes(y = upper), color = muted("blue"), alpha = 0.5) +
  geom_line(data = lines_df, aes(y = lower), color = muted("red"), alpha = 0.5) +
  geom_line(data = lines_df, aes(y = origin), color = "grey") +
  #geom_point(aes(y = mid_year), alpha = 0.3, shape = 1) +
  #geom_text(aes(y = mid_year, label = Movie), alpha = 0.75, check_overlap = FALSE, size = 2, family = "Gill Sans") +
  geom_text_repel(aes(y = mid_year, label = Movie), alpha = 0.75, size = 2, family = "Gill Sans") +
  annotate("text", y = max(lines_df$upper), x = min(stories_df$pub_year), label = "Still possible", hjust = 0, vjust = 0, size = 3, color = muted("green"), fontface = "italic", family = "Gill Sans") +
  annotate("text", y = (mean(lines_df$upper)/2), x = min(stories_df$pub_year), label = "Obsolete", hjust = 0, vjust = 0.5, size = 3, color = muted("red"), fontface = "italic", family = "Gill Sans") +
  annotate("text", y = -5, x = min(stories_df$pub_year), label = "Contemporary fiction", hjust = 0.5, size = 3, color = muted("blue"), fontface = "italic", family = "Gill Sans") +
  annotate("text", y = min(lines_df$lower), x = min(stories_df$pub_year), label = "Historical fiction", hjust = 0, size = 3, color = muted("purple"), fontface = "italic", family = "Gill Sans") +
  scale_y_continuous(trans = "y_scale",
                     breaks = c(-10000, -1000, -100, -10, 0, 10, 100, 1000, 10000, 100000, 1000000, 10000000),
                     name = "Years in the Past / Future",
                     labels = comma) +
  scale_x_continuous(trans = "x_scale", 
                     breaks = c(-2000, 0, 1000, 1800, 1900, 1980, 2000, 2010, today_year()),
                     labels = x_labeller,
                     name = "Date of Publication") +
  coord_cartesian(xlim = c(2 * min(stories_df$pub_year) - max(stories_df$pub_year), max(stories_df$pub_year))) +
  labs(title = "Stories of the past and future",
       subtitle = "A visualization based on XKCD's concept",
       caption = "Based on xkcd 1491, produced in R (R Core Team 2016) CC BY-SA 2017 Thomas Hopper") +
  theme_minimal() +
  theme(text = element_text(family = "Gill Sans"),
        plot.caption = element_text(size = 6))
ggsave("stories_past_future.png", type = "cairo-png", dpi = 150, width = 9, height = 9, units = "in")

