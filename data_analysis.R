# load the ggplot2 library for plotting
library(ggplot2)

# turn off factors
options(stringsAsFactors = FALSE)

# import data
data <- read.csv(file = "data.csv")
head(data)

# For testing purposes, work with the first 1000 data
#data <- head(data, 1000)

# view the format of the object in R
str(data)

# Get only timestamp, ioreads, iowrites, rbytes, tbytes, and uuid
data <- data[ , -which(names(data) %in% c("computenode","interface","pid","rpackets","state","tpackets", "drpackets", "dtpackets"))]
head(data)
gc()

# Rename column where names is "Sepal.Length"
names(data)[names(data) == "X.timestamp"] <- "timestamp"

# format timestamp col to get date/time, 2021-04-02T00:00:01.000Z
# convert single instance of date/time in format year-month-day hour:min:sec.miliseconds
data$timestamp <- as.POSIXct(data$timestamp, format="%Y-%m-%dT%H:%M:%OSZ")
head(data)

# scale ioreads and iowrites from bytes to MB, operaciones de lectura y escritura en disco, leer o escribir un bloque (4K)
# we have the number of accesses to blocks (read/write), therefore the number of bytes will be this
# number * 4k (block size), in MB = x * 4000 / 1000000 = x * 0,004
data$dioreads <- data$dioreads * 0.000004 / 60
data$diowrites <- data$diowrites * 0.000000004 /60 
data$ioreads <- data$ioreads * 0.000000004
data$iowrites <- data$iowrites * 0.000000004

# scale rbytes and tbytes from bytes to MB-GB
data$rbytes <- data$rbytes / 1000000000
data$tbytes <- data$tbytes / 1000000

# CPU, number of cores 400 means 4 cores
data$cpu <- data$cpu / 100

# RAM is expressed in % over 192Gb, therefore we multiply by it to get the memory usage
data$mem <- data$mem * 1.92

head(data)

# Get unique values of uuids
uuids <- unique(data[c("uuid")])

# Plot the IO Writes
ggplot(data = data, aes(x = timestamp, y = diowrites)) + 
  geom_line(aes(color = uuid), size = 1) +
  scale_color_manual(values = c("#7FB7BE", "#D3F3EE", "#DACC3E", "#BC2C1A", "#7D1538", "45503B", "#B098A4", "#70171F")) +
  theme(
    plot.title = element_text(color = "#FFFFFF", size = 20, face = "bold", hjust = 0.5, vjust = 1),
    plot.background = element_rect(fill = "#000000"),
    panel.background = element_rect(fill = "#000000"),
    axis.text.x = element_text(colour = "#888888"),
    axis.text.y = element_text(colour = "#888888"),
    axis.line = element_line(colour = "#888888"),
    panel.grid.minor = element_line(colour="#404040", size=0.5),
    panel.grid.major = element_line(colour="#404040", size=0.5),
    plot.margin = unit(c(2,1,1,1),"cm"),
    legend.position = "none"
  ) + ggtitle("Disk writes byte rate (TBs)")

# Plot the IO Reads
ggplot(data = data, aes(x = timestamp, y = dioreads)) + 
  geom_line(aes(color = uuid), size = 1) +
  scale_color_manual(values = c("#7FB7BE", "#D3F3EE", "#DACC3E", "#BC2C1A", "#7D1538", "45503B", "#B098A4", "#70171F")) +
  theme(
    plot.title = element_text(color = "#FFFFFF", size = 20, face = "bold", hjust = 0.5, vjust = 1),
    plot.background = element_rect(fill = "#000000"),
    panel.background = element_rect(fill = "#000000"),
    axis.text.x = element_text(colour = "#888888"),
    axis.text.y = element_text(colour = "#888888"),
    axis.line = element_line(colour = "#888888"),
    panel.grid.minor = element_line(colour="#404040", size=0.5),
    panel.grid.major = element_line(colour="#404040", size=0.5),
    plot.margin = unit(c(2,1,1,1),"cm"),
    legend.position = "none"
  ) + ggtitle("Disk reads byte rate (GBs)")

# Plot the acummulative IO Writes
ggplot(data = data, aes(x = timestamp, y = iowrites)) + 
  geom_line(aes(color = uuid), size = 1) +
  scale_color_manual(values = c("#7FB7BE", "#D3F3EE", "#DACC3E", "#BC2C1A", "#7D1538", "45503B", "#B098A4", "#70171F")) +
  theme(
    plot.title = element_text(color = "#FFFFFF", size = 20, face = "bold", hjust = 0.5, vjust = 1),
    plot.background = element_rect(fill = "#000000"),
    panel.background = element_rect(fill = "#000000"),
    axis.text.x = element_text(colour = "#888888"),
    axis.text.y = element_text(colour = "#888888"),
    axis.line = element_line(colour = "#888888"),
    panel.grid.minor = element_line(colour="#404040", size=0.5),
    panel.grid.major = element_line(colour="#404040", size=0.5),
    plot.margin = unit(c(2,1,1,1),"cm"),
    legend.position = "none"
  ) + ggtitle("Disk writes bytes (TB)")

# Plot the acummulative IO Reads
ggplot(data = data, aes(x = timestamp, y = ioreads)) + 
  geom_line(aes(color = uuid), size = 1) +
  scale_color_manual(values = c("#7FB7BE", "#D3F3EE", "#DACC3E", "#BC2C1A", "#7D1538", "45503B", "#B098A4", "#70171F")) +
  theme(
    plot.title = element_text(color = "#FFFFFF", size = 20, face = "bold", hjust = 0.5, vjust = 1),
    plot.background = element_rect(fill = "#000000"),
    panel.background = element_rect(fill = "#000000"),
    axis.text.x = element_text(colour = "#888888"),
    axis.text.y = element_text(colour = "#888888"),
    axis.line = element_line(colour = "#888888"),
    panel.grid.minor = element_line(colour="#404040", size=0.5),
    panel.grid.major = element_line(colour="#404040", size=0.5),
    plot.margin = unit(c(2,1,1,1),"cm"),
    legend.position = "none"
  ) + ggtitle("Disk reads bytes (TB)")

# Plot the CPU Utilization
ggplot(data = data, aes(x = timestamp, y = cpu)) + 
  geom_line(aes(color = uuid), size = 1) +
  scale_color_manual(values = c("#7FB7BE", "#D3F3EE", "#DACC3E", "#BC2C1A", "#7D1538", "45503B", "#B098A4", "#70171F")) +
  theme(
    plot.title = element_text(color = "#FFFFFF", size = 20, face = "bold", hjust = 0.5, vjust = 1),
    plot.background = element_rect(fill = "#000000"),
    panel.background = element_rect(fill = "#000000"),
    axis.text.x = element_text(colour = "#888888"),
    axis.text.y = element_text(colour = "#888888"),
    axis.line = element_line(colour = "#888888"),
    panel.grid.minor = element_line(colour="#404040", size=0.5),
    panel.grid.major = element_line(colour="#404040", size=0.5),
    plot.margin = unit(c(2,1,1,1),"cm"),
    legend.position = "none"
  ) + ggtitle("CPU Utilization (cores)")

# Plot the RAM usage
ggplot(data = data, aes(x = timestamp, y = mem)) + 
  geom_line(aes(color = uuid), size = 1) +
  scale_color_manual(values = c("#7FB7BE", "#D3F3EE", "#DACC3E", "#BC2C1A", "#7D1538", "45503B", "#B098A4", "#70171F")) +
  theme(
    plot.title = element_text(color = "#FFFFFF", size = 20, face = "bold", hjust = 0.5, vjust = 1),
    plot.background = element_rect(fill = "#000000"),
    panel.background = element_rect(fill = "#000000"),
    axis.text.x = element_text(colour = "#888888"),
    axis.text.y = element_text(colour = "#888888"),
    axis.line = element_line(colour = "#888888"),
    panel.grid.minor = element_line(colour="#404040", size=0.5),
    panel.grid.major = element_line(colour="#404040", size=0.5),
    plot.margin = unit(c(2,1,1,1),"cm"),
    legend.position = "none"
  ) + ggtitle("RAM usage (Gb)")

# Plot the Network incoming bytes
ggplot(data = data, aes(x = timestamp, y = rbytes)) + 
  geom_line(aes(color = uuid), size = 1) +
  scale_color_manual(values = c("#7FB7BE", "#D3F3EE", "#DACC3E", "#BC2C1A", "#7D1538", "45503B", "#B098A4", "#70171F")) +
  theme(
    plot.title = element_text(color = "#FFFFFF", size = 20, face = "bold", hjust = 0.5, vjust = 1),
    plot.background = element_rect(fill = "#000000"),
    panel.background = element_rect(fill = "#000000"),
    axis.text.x = element_text(colour = "#888888"),
    axis.text.y = element_text(colour = "#888888"),
    axis.line = element_line(colour = "#888888"),
    panel.grid.minor = element_line(colour="#404040", size=0.5),
    panel.grid.major = element_line(colour="#404040", size=0.5),
    plot.margin = unit(c(2,1,1,1),"cm"),
    legend.position = "none"
  ) + ggtitle("Network incoming bytes (GB)")

# Plot the Network outgoing bytes
ggplot(data = data, aes(x = timestamp, y = tbytes)) + 
  geom_line(aes(color = uuid), size = 1) +
  scale_color_manual(values = c("#7FB7BE", "#D3F3EE", "#DACC3E", "#BC2C1A", "#7D1538", "45503B", "#B098A4", "#70171F")) +
  theme(
    plot.title = element_text(color = "#FFFFFF", size = 20, face = "bold", hjust = 0.5, vjust = 1),
    plot.background = element_rect(fill = "#000000"),
    panel.background = element_rect(fill = "#000000"),
    axis.text.x = element_text(colour = "#888888"),
    axis.text.y = element_text(colour = "#888888"),
    axis.line = element_line(colour = "#888888"),
    panel.grid.minor = element_line(colour="#404040", size=0.5),
    panel.grid.major = element_line(colour="#404040", size=0.5),
    plot.margin = unit(c(2,1,1,1),"cm"),
    legend.position = "none"
  ) + ggtitle("Network outgoing bytes (MB)")

