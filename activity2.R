library(dplyr)
library(ggplot2)
library(cowplot)
library(mice)



read_csv = read.csv('C:/Users/14168/Desktop/activity.csv')
hist(read_csv[,1])
read_csv[is.na(read_csv)] <- 0
mean = mean(read_csv$steps)
median = median(read_csv$steps)
steps_per_interval <- read_csv %>% group_by(date) %>% summarise(steps = mean(steps, na.rm=TRUE))
p <- ggplot(steps_per_interval, aes(x=date, y=steps, group=1)) +
  geom_line( color="#69b3a2") + 
  xlab("") +
  theme(axis.text.x=element_text(angle=60, hjust=1)) 

p
read_csv$interval[which.max(read_csv$steps)]
ggplot(read_csv, aes(steps)) +
  geom_histogram(color = "#000000", fill = "#0099F8") +
  ggtitle("Variable distribution") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))
value_imputed <- data.frame(
  original = read_csv$steps,
  imputed_zero = replace(read_csv$steps, is.na(read_csv$steps), 0),
  imputed_mean = replace(read_csv$steps, is.na(read_csv$steps), mean(read_csv$steps, na.rm = TRUE)),
  imputed_median = replace(read_csv$steps, is.na(read_csv$steps), median(read_csv$steps, na.rm = TRUE))
)
value_imputed
h1 <- ggplot(value_imputed, aes(x = original)) +
  geom_histogram(fill = "#ad1538", color = "#000000", position = "identity") +
  ggtitle("Original distribution") +
  theme_classic()
h2 <- ggplot(value_imputed, aes(x = imputed_zero)) +
  geom_histogram(fill = "#15ad4f", color = "#000000", position = "identity") +
  ggtitle("Zero-imputed distribution") +
  theme_classic()
h3 <- ggplot(value_imputed, aes(x = imputed_mean)) +
  geom_histogram(fill = "#1543ad", color = "#000000", position = "identity") +
  ggtitle("Mean-imputed distribution") +
  theme_classic()
h4 <- ggplot(value_imputed, aes(x = imputed_median)) +
  geom_histogram(fill = "#ad8415", color = "#000000", position = "identity") +
  ggtitle("Median-imputed distribution") +
  theme_classic()

plot_grid(h1, h2, h3, h4, nrow = 2, ncol = 2)


read_csv_numeric <- read_csv %>%
  select(steps, interval)

md.pattern(read_csv_numeric)

mice_imputed <- data.frame(
  original = read_csv$steps,
  imputed_pmm = complete(mice(read_csv_numeric, method = "pmm"))$steps,
  imputed_cart = complete(mice(read_csv_numeric, method = "cart"))$steps
)
mice_imputed

h1 <- ggplot(value_imputed, aes(x = original)) +
  geom_histogram(fill = "#ad1538", color = "#000000", position = "identity") +
  ggtitle("Original distribution") +
  theme_classic()
h2 <- ggplot(value_imputed, aes(x = imputed_pmm)) +
  geom_histogram(fill = "#15ad4f", color = "#000000", position = "identity") +
  ggtitle("imputed_pmm distribution") +
  theme_classic()
h3 <- ggplot(value_imputed, aes(x = imputed_cart)) +
  geom_histogram(fill = "#1543ad", color = "#000000", position = "identity") +
  ggtitle("imputed_cart distribution") +
  theme_classic()


plot_grid(h1, h2,h3, nrow = 2, ncol = 2)
imputed_median = replace(read_csv, is.na(read_csv), median(read_csv$steps, na.rm = TRUE))
steps_totalnumber_day <- imputed_median %>% group_by(date) %>% summarise(steps = sum(steps, na.rm=TRUE))
ggplot(steps_totalnumber_day, aes(steps)) +
  geom_histogram(color = "#000000", fill = "#0099F8") +
  ggtitle("Variable distribution") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))

