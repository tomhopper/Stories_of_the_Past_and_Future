library(readr)
library(dplyr)
library(ggplot2)
library(xkcd)
library(scales)

stories_df <- read_csv("data-raw/stories.csv") %>% 
  mutate(lo = lower - Year,
         up = upper - Year) %>%
  rowwise() %>% 
  mutate(mid_year = mean(c(lo, up))) %>% 
  rename(pub_year = Year)

stories_range <- range(stories_df$pub_year)

x_pos <- as.numeric(substr(Sys.Date(), 1, 4))
if(stories_range[2] - stories_range[1] > 1000)
  x_pos <- c(x_pos, seq(4 * stories_range[1] - stories_range[2], stories_range[2], by = 100))
if(stories_range[2] - stories_range[1] > 100)
  x_pos <- c(x_pos, seq(max(500, stories_range[1]), stories_range[2], by = 10))
if(stories_range[2] - stories_range[1] > 10)
  x_pos <- c(x_pos, seq(max(1950, stories_range[1]), stories_range[2], by = 1))

x_pos <- unique(sort(x_pos))

lines_df <- data_frame(pub_year = x_pos,
                       upper = as.numeric(substr(Sys.Date(), 1, 4)) - pub_year,
                       lower = -1.5 * (as.numeric(substr(Sys.Date(), 1, 4)) - pub_year),
                       origin = 0)

log_y_fun <- function(x) {
  log(abs(x) + 1)
}

y_scale_trans <- function() trans_new("y_scale",
                                      function(x) {
                                        #print(paste("log10 :", x))
                                        y <- x
                                        for (i in 1:length(x)) {
                                          if(is.na(x[i])) {y[i] <- NA}
                                          #else if(x[i] > as.numeric(substr(Sys.Date(), 1, 4))) {y[i] <- x[i]}
                                          else if(isTRUE(all.equal(x[i], c(0.), check.names = FALSE))) {y[i] <- x[i]}
                                          else if(x[i] > 0) {y[i] <- log_y_fun(x[i])}
                                          else if(x[i] < 0) {y[i] <- -log_y_fun(x[i])}
                                          else {stop(paste0("x[", i, "] failed comparisons == 0, >0, <0."))}
                                        }
                                        return(y)
                                      },
                                      function(x) {
                                        #print(paste("10^ :", x))
                                        y <- x
                                        for (i in 1:length(x)) {
                                          if(is.na(x[i])) {y[i] <- NA}
                                          else if(isTRUE(all.equal(x[i], c(0.), check.names = FALSE))) {y[i] <- x[i]}
                                          else if(x[i] > 0) {y[i] <- exp(x[i]) - 1}
                                          else if(x[i] < 0) {y[i] <- -(exp(abs(x[i])) - 1)}
                                          else {stop(paste0("x[", i, "] failed comparisons == 0, >0, <0."))}
                                        }
                                        return(y)
                                      }
)

log_x_fun <- function(x) {
  today_year <- as.numeric(substr(Sys.Date(), 1, 4))
  #y <- (today_year - x) / (200 + today_year - x)
  y <- -log(today_year - x + 1)
  return(y)
}
inv_x_fun <- function(x) {
  today_year <- as.numeric(substr(Sys.Date(), 1, 4))
  #y <- (today_year - 200 * x - today_year * x) / (1 - x)
  y <- as.integer(round(today_year + 1 - exp(-x), 0))
}
x_scale_trans <- function() trans_new("x_scale",
                                      log_x_fun,
                                      function(x) {
                                        today_year <- as.numeric(substr(Sys.Date(), 1, 4))
                                        #y <- (today_year - 200 * x - today_year * x) / (1 - x)
                                        y <- as.integer(round(today_year + 1 - exp(-x), 0))
                                      }
)

x_labeller <- function(x) {
  as.integer(round(x, 0))
}


ggplot(stories_df) +
  aes(x = pub_year) +
  geom_line(data = lines_df, aes(y = upper), color = muted("blue"), alpha = 0.5) +
  geom_line(data = lines_df, aes(y = lower), color = muted("red"), alpha = 0.5) +
  geom_line(data = lines_df, aes(y = origin), color = "grey") +
  #geom_point(aes(y = mid_year), alpha = 0.3, shape = 1) +
  geom_text(aes(y = mid_year, label = Movie), alpha = 0.75, check_overlap = TRUE, size = 2, family = "Gill Sans") +
  annotate("text", y = max(lines_df$upper), x = stories_range[1], label = "Still possible", hjust = 0, vjust = 0, size = 3, color = "grey20", fontface = "italic", family = "Gill Sans") +
  annotate("text", y = (mean(lines_df$upper)/2), x = stories_range[1], label = "Obsolete", hjust = 0, vjust = 0.5, size = 3, color = "grey20", fontface = "italic", family = "Gill Sans") +
  annotate("text", y = -5, x = stories_range[1], label = "Contemporary fiction", hjust = 0.5, size = 3, color = "grey20", fontface = "italic", family = "Gill Sans") +
  annotate("text", y = min(lines_df$lower), x = stories_range[1], label = "Historical fiction", hjust = 0, size = 3, color = "grey20", fontface = "italic", family = "Gill Sans") +
  scale_y_continuous(trans = "y_scale",
                     breaks = c(-10000, -1000, -100, -10, 0, 10, 100, 1000, 10000, 100000, 1000000, 10000000),
                     name = "Years in the Past / Future",
                     labels = comma) +
  scale_x_continuous(trans = "x_scale", 
                     breaks = c(-2000, 0, 1000, 1800, 1900, 1980, 2000, 2010, 2015),
                     labels = x_labeller,
                     name = "Date of Publication") +
  coord_cartesian(xlim = c(2 * stories_range[1] - stories_range[2], stories_range[2])) +
  theme_minimal() +
  theme(text = element_text(family = "Gill Sans"))
  
