
# load package
library(data.table)
library(ggplot2)

# go to data folder
setwd("../data")

# load data files
artists <- data.table(read.csv('artists.csv'))
venues <- data.table(read.csv('venues.csv'))
concerts <- data.table(read.csv('concerts.csv'))
concerts.bt <- data.table(read.csv('bt-concerts.csv'))

# format data
concerts$date <- as.Date(concerts$date, format = "%Y-%m-%d")
concerts.bt$date <- as.Date(concerts.bt$date, format = "%d-%b-%y")

# merge concerts and concerts.bt to flag bt concerts
concerts.flagged <- merge(concerts, concerts.bt, by = c("date", "artist"), all.x = T)
concerts.flagged$bt_concert <- ifelse(is.na(concerts.flagged$venue), 0, 1)
concerts.flagged <- concerts.flagged[, -c("event", "venue", "venue.city", "venue.state")]

# manually flag/deflag certain events
concerts.flagged[id == 16626779]$bt_concert <- 0
concerts.flagged[id == 27959704]$bt_concert <- 0
concerts.flagged[id == 29435009]$bt_concert <- 0
concerts.flagged[id == 33309979]$bt_concert <- 0
concerts.flagged[id == 34511124]$bt_concert <- 0

# concerts to figure out
# the districts, 2/25/2014, fix: before they appear in the data
# connor youngblood, 6/24/2014, fix: rerun data pull
# you won't, 1/21/2015, fix: concert not in data
# you won't, 12/5/2015, fix: concert not in data
# creed bratton, 9/14/2018, fix: concert not in data
# lief ..., 11/16/2018, rerun data pull

# merge on capacity information
concerts.w_capacity <- merge(concerts.flagged, venues[, c("id", "capacity")], by.x = "venue.id", by.y = "id", all.x = T)

# calculate popularity_plus
concerts.w_capacity[, popularity_plus := capacity * popularity]

# plot popularity_plus
ggplot(concerts.w_capacity[artist == "Lucy Dacus" & !is.na(popularity_plus)], aes(x = date, y = capacity)) + geom_line()

# aggregate to month/week
concerts.w_capacity[, week := as.Date(paste(year(date), 1, 1, sep = "-")) + 7 * (week(date) - 1)]
concerts.w_capacity[, month := as.Date(paste(year(date), month(date), 1, sep = "-"))]
concerts.w_capacity[, quarter := as.Date(paste(year(date), 1, 1, sep = "-")) + 90 * (quarter(date) - 1)]
concerts.agg <- concerts.w_capacity[, .(capacity = mean(capacity, na.rm = T), popularity_plus = mean(popularity_plus, na.rm = T)),
                                    by = list(quarter, artist)]

ggplot(concerts.agg[artist == "Maggie Rogers" & !is.na(capacity)], aes(x = quarter, y = capacity)) + geom_line()


