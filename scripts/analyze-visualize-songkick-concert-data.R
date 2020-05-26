
# load package and set folder paths
library(data.table)
library(dplyr)
library(ggplot2)

base_path <- "/Users/ben-tanen/Google Drive/Projects/songkick-popularity/"

############################
###   IMPORT DATA FILES  ###
### AND FORMAT FOR MERGE ###
############################

artists <- data.table(read.csv(paste0(base_path, "data/artists.csv")))
venues <- data.table(read.csv(paste0(base_path, "data/venues.csv")))
concerts <- data.table(read.csv(paste0(base_path, "data/concerts.csv")))
concerts.bt <- data.table(read.csv(paste0(base_path, "data/bt-concerts.csv")))

# format date fields
concerts$date <- as.Date(concerts$date, format = "%m/%d/%y")
concerts.bt$date <- as.Date(concerts.bt$date, format = "%m/%d/%y")

##############################
### MERGE CONCERT DATASETS ###
##############################

# merge concerts together based on event id (manually added to data)
# in order to identify which events are BT or not
concerts.merge <- merge(concerts, concerts.bt, by = c("id"), all = T)

# merge on songkick data to BT concerts and flag
concerts.flagged <- merge(concerts.bt, 
                          unique(concerts.merge[!is.na(id) & !is.na(venue)][, c("id", "name", "popularity", "venue.id", "venue.name")]),
                          by = c("id"), all.x = T)
concerts.flagged$bt.concert <- 1

# filter out unflagged concerts
concerts.unflagged <- concerts.merge[is.na(venue)][, c("id", "name", "date.x", "artist.x", "billing.x", "popularity", "venue.id", "venue.name")]

# reformat/match column names
names(concerts.unflagged) <- c("id", "name", "date", "artist", "billing", "popularity", "venue.id", "venue.name")
names(concerts.flagged) <- c("id", "artist", "event", "date", "venue", "venue.city", "venue.state", "billing", 
                             "name", "popularity", "venue.id", "venue.name", "bt.concert")

# stack all concerts together
concerts.all <- rbind(concerts.flagged[, -c("event", "venue", "venue.city", "venue.state")], 
                      concerts.unflagged, fill = T)

# merge on capacity information
concerts.w_capacity <- merge(concerts.all, 
                             venues[, c("id", "capacity")], 
                             by.x = "venue.id", by.y = "id", all.x = T)[order(artist, date)]

#####################
### FLAG OUTLIERS ###
#####################

# flag outliers using tukey (6x parameter for now)
concerts.w_capacity[, outlier_thresh1 := quantile(capacity, .75, na.rm = T) +
                        (quantile(capacity, .75, na.rm = T) - 
                         quantile(capacity, .25, na.rm = T)) * 6, by = artist]
concerts.w_capacity[, outlier1 := (!is.na(capacity) & capacity > outlier_thresh1)]

# flag outliers using mean + 2*SD (doesn't seem great but leaving in place for now)
concerts.w_capacity[, outlier_thresh2 := mean(capacity, na.rm = T) * sd(capacity, na.rm = T) * 2, by = artist]
concerts.w_capacity[, outlier2 := (!is.na(capacity) & capacity > outlier_thresh2)]

# flag outliers using dbscan algo
concerts.w_capacity <- rbindlist(lapply(unique(concerts.w_capacity$artist), function(x) {
    print(as.character(x))
    df <- concerts.w_capacity[artist == x]
    df[, date_num := as.numeric(date)]
    res <- fpc::dbscan(df[!is.na(capacity), c("date_num", "capacity")], 
                       eps = 7500, MinPts = 5)
    df$dbscan_cluster <- -1
    df[!is.na(capacity)]$dbscan_cluster <- res$cluster
    df$outlier3 <- ifelse(df$dbscan_cluster == 0, 1, 0)
    return(df[, -c("date_num")])
}))

##############################
### AGGREGATE CAPACITY FOR ###
###    SMOOTHER PLOTTING   ###
##############################

concerts.w_capacity[outlier3 == 0, capacity_mean_mth := mean(capacity, na.rm = T), 
                    by = list(artist, format(date, format = "%Y_%m"))]
concerts.w_capacity[outlier3 == 0, capacity_mean_qtr := mean(capacity, na.rm = T), 
                    by = list(artist, paste0(year(date), "Q", quarter(date)))]

############
### PLOT ###
############

# pick a random artist to plot
artist.samp <- sample(unique(concerts.w_capacity$artist), 1)
ggplot(concerts.w_capacity[artist == artist.samp], aes(x = date)) +
    geom_point(aes(y = capacity, color = factor(outlier3))) +
    geom_line(aes(y = capacity_mean_qtr)) +
    scale_color_discrete() +
    labs(title = artist.samp)

concerts.w_capacity[artist == artist.samp & outlier3 == 1]

