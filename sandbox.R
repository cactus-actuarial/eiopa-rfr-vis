library("tibble")
library("readxl")
library("tidyr")
library("ggplot2")

data <- read_excel("data//EIOPA_RFR_20181231_Term_Structures.xlsx", sheet = "RFR_spot_no_VA", col_types = "numeric")
data1 <- data[-c(1:8),]
colnames(data1)[colnames(data1) == "Main menu"] <- "time"
data2 <- gather(data1, country, value, colnames(data1)[2]:colnames(data1)[ncol(data1)])


ggplot(subset(data2, country == "Poland"), aes(x=time, y=value, colour = country)) +
    geom_line() + 
    scale_x_continuous(breaks = c(2019, seq(from = 2020, to = 2070, by = 5))) +
    scale_y_continuous(breaks = seq(from = 0, to = 0.08, by = 0.005), labels = scales::percent_format())