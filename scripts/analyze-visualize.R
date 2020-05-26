
# load package
library(data.table)
library(dplyr)
library(ggplot2)

# go to data folder
setwd("../data")

# load data files
artists <- data.table(read.csv('artists.csv'))
venues <- data.table(read.csv('venues.csv'))
concerts <- data.table(read.csv('concerts.csv'))
concerts.bt <- data.table(read.csv('bt-concerts.csv'))

# format data
concerts$date <- as.Date(concerts$date, format = "%m/%d/%y")
concerts.bt$date <- as.Date(concerts.bt$date, format = "%m/%d/%y")

# merge concerts and concerts.bt to flag bt concerts
concerts.merge <- merge(concerts, concerts.bt, by = c("id"), all = T)
concerts.flagged <- merge(concerts.bt, 
                          unique(concerts.merge[!is.na(id) & !is.na(venue)][, c("id", "name", "popularity", "venue.id", "venue.name")]),
                          by = c("id"), all.x = T)
concerts.flagged$bt.concert <- 1

# stack flagged concerts with unflagged concerts for full set
concerts.unflagged <- concerts.merge[is.na(venue)][, c("id", "name", "date.x", "artist.x", "billing.x", "popularity", "venue.id", "venue.name")]
names(concerts.unflagged) <- c("id", "name", "date", "artist", "billing", "popularity", "venue.id", "venue.name")
names(concerts.flagged) <- c("id", "artist", "event", "date", "venue", "venue.city", "venue.state", "billing", 
                             "name", "popularity", "venue.id", "venue.name", "bt.concert")

concerts.all <- rbind(concerts.flagged[, -c("event", "venue", "venue.city", "venue.state")], 
                      concerts.unflagged, fill = T)

# merge on capacity information
concerts.w_capacity <- merge(concerts.all, 
                             venues[, c("id", "capacity")], 
                             by.x = "venue.id", by.y = "id", all.x = T)[order(artist, date)]

# flag outliers with:
# 1. tukey
# 2. mean + 2 * SD
# 3. dbscan
concerts.w_capacity[, outlier_thresh1 := quantile(capacity, .75, na.rm = T) +
                        (quantile(capacity, .75, na.rm = T) - 
                         quantile(capacity, .25, na.rm = T)) * 6, by = artist]
concerts.w_capacity[, outlier1 := (!is.na(capacity) & capacity > outlier_thresh1)]

concerts.w_capacity[, outlier_thresh2 := mean(capacity, na.rm = T) * sd(capacity, na.rm = T) * 2, by = artist]
concerts.w_capacity[, outlier2 := (!is.na(capacity) & capacity > outlier_thresh2)]

concerts.w_capacity <- rbindlist(lapply(unique(concerts.w_capacity$artist), function(x) {
    print(as.character(x))
    df <- concerts.w_capacity[artist == x]
    df[, date_num := as.numeric(date)]
    res <- fpc::dbscan(df[!is.na(capacity), c("date_num", "capacity")], 
                       eps = 10000, MinPts = 5)
    df$outlier_cluster <- -1
    df[!is.na(capacity)]$outlier_cluster <- res$cluster
    return(df[, -c("date_num")])
}))

# add mean capacity by month/quarter for plotting
concerts.w_capacity[, capacity_mean_mth := mean(capacity, na.rm = T), 
                    by = list(artist, format(date, format = "%Y_%m"))]
concerts.w_capacity[, capacity_mean_qtr := mean(capacity, na.rm = T), 
                    by = list(artist, paste0(year(date), "Q", quarter(date)))]

# chart random artist
artist.samp <- sample(unique(concerts.w_capacity$artist), 1)
ggplot(concerts.w_capacity[artist == artist.samp], aes(x = date)) +
    geom_point(aes(y = capacity, color = factor(outlier_cluster))) +
    geom_line(aes(y = capacity_mean_qtr)) +
    scale_color_discrete() +
    labs(title = artist.samp)

