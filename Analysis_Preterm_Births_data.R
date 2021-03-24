
################################################################################

#                  Load the libraries for this analysis                       #

################################################################################



library(tidyverse)
library(ggmap)
library(randomForest)
library(urbnmapr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(usmap)
library(reshape2)
library(hrbrthemes)
library(viridis)


################################################################################

#                        Read in the raw data set                              #

################################################################################



preterm_raw <- 
  read.csv(file = 'preterm-and-very-preterm-births-by-county-2010-2018-3.csv')



################################################################################

#    Perform transformations of the raw data and do basic data exploration     #

################################################################################



# Look at the structure and content of the data set.
glimpse(preterm_raw)
View(preterm_raw)

# Set variable names to lower case.
names(preterm_raw) <- tolower(names(preterm_raw))

# Select variables
preterm <- preterm_raw %>% 
  select(year, county, birth.type, 
         total.births, events)

glimpse(preterm)

#Change none for 0
preterm[is.na(preterm)] <- 0
str(preterm)

#Get rid off total results for California state as a whole
preterm <- preterm[preterm$county!="California",] 

preterm$county <- as.factor(preterm$county)
levels(preterm$county)          
length(levels(preterm$county)) #58 counties it is
# [1] "Alameda"         "Alpine"          "Amador"          "Butte"           "Calaveras"       "Colusa"          "Contra Costa"   
# [8] "Del Norte"       "El Dorado"       "Fresno"          "Glenn"           "Humboldt"        "Imperial"        "Inyo"           
# [15] "Kern"            "Kings"           "Lake"            "Lassen"          "Los Angeles"     "Madera"          "Marin"          
# [22] "Mariposa"        "Mendocino"       "Merced"          "Modoc"           "Mono"            "Monterey"        "Napa"           
# [29] "Nevada"          "Orange"          "Placer"          "Plumas"          "Riverside"       "Sacramento"      "San Benito"     
# [36] "San Bernardino"  "San Diego"       "San Francisco"   "San Joaquin"     "San Luis Obispo" "San Mateo"       "Santa Barbara"  
# [43] "Santa Clara"     "Santa Cruz"      "Shasta"          "Sierra"          "Siskiyou"        "Solano"          "Sonoma"         
# [50] "Stanislaus"      "Sutter"          "Tehama"          "Trinity"         "Tulare"          "Tuolumne"        "Ventura"        
# [57] "Yolo"            "Yuba"   



preterm$birth.type <- as.factor(preterm$birth.type)
levels(preterm$birth.type) # two factors
#[1] "Preterm Births"      "Very Preterm Births"

#Change to wide format
preterm_wide <- dcast(preterm, year + county + total.births ~ birth.type, value.var="events")

names(preterm_wide)[4] <- "preterm.births"
names(preterm_wide)[5] <- "very.preterm.births"

#Add calculated normal births
preterm_wide$normal.births <- preterm_wide$total.births - preterm_wide$preterm.births
str(preterm_wide)

#Reorder
data_preterm <- preterm_wide[, c(1, 2, 3, 6, 4, 5)]
head(data_preterm)

#Add rates
data_preterm$rate_preterm <- data_preterm$preterm.births/data_preterm$total.births
data_preterm$rate_very_preterm <- data_preterm$very.preterm.births/data_preterm$total.births
head(data_preterm)
str(data_preterm)

# year    county total.births normal.births preterm.births very.preterm.births rate_preterm rate_very_preterm
# 1 2010   Alameda        19280         17658           1622                 259   0.08412863        0.01343361
# 2 2010    Alpine            4             4              0                   0   0.00000000        0.00000000
# 3 2010    Amador          272           247             25                   0   0.09191176        0.00000000
# 4 2010     Butte         2447          2264            183                  31   0.07478545        0.01266857
# 5 2010 Calaveras          346           317             29                   0   0.08381503        0.00000000
# 6 2010    Colusa          338           314             24                   0   0.07100592        0.00000000


################################################################################

#                          Data visualization                                  #

################################################################################

# An ordered numeric variable for the X axis (year)
# Another numeric variable for the Y axis (preterm_rate)
# A categorical variable that specify the group of the observation (county)


total_by_year <- aggregate(data_preterm['total.births'], by=data_preterm['year'], sum)
total_by_year
qqplot(total_by_year$year, total_by_year$total.births)
#Total.births is growing year to year

preterm_by_year <- aggregate(data_preterm['total.births'], by=data_preterm['year'], sum)
#how norm is norm  - total
hist(data_preterm$total.births)
hist(data_preterm$total.births, probability = TRUE, breaks = 100) 
# Los Angeles County has the highest quantity of total births per year


#p110 - hist of frequency
hist(data_preterm$rate_preterm) 
hist(data_preterm$rate_very_preterm)


#p110 - hist of density
hist(data_preterm$rate_preterm, probability = TRUE, breaks = 10)
lines(density(data_preterm$rate_preterm))

hist(data_preterm$rate_very_preterm, probability = TRUE, breaks = 10)
lines(density(data_preterm$rate_very_preterm))





#scatterplot for total_births
plot(data_preterm$year, data_preterm$total.births, pch=19, col="black")  
# LA is an outlier in total births


#scatterplot for preterm_rate
plot(data_preterm$year, 
     data_preterm$rate_preterm, 
     pch=16, 
     col="blue",
     xlab="year", 
     ylab="preterm rate", 
     main="Preterm Births / Total Births in 58 counties of California")
#Some counties on zero (with the smallest total.births)


#scatterplot for very_preterm_rate

plot(data_preterm$year, 
     data_preterm$rate_very_preterm, 
     pch=16, 
     col="blue",
     xlab="year", 
     ylab="preterm rate", 
     main="Very Preterm Births / Total Births in 58 counties of California")
#Some counties on zero (with the smallest total.births)


#smooth scatterplot for total_births
smoothScatter(data_preterm$year, data_preterm$total.births)


#smooth scatterplot for preterm_rate
smoothScatter(data_preterm$year, data_preterm$rate_preterm) # some spots


#smooth scatterplot for very_preterm_rate
smoothScatter(data_preterm$year, data_preterm$rate_very_preterm) # 3 spots


#QQplot for preterm_rate
qqplot(data_preterm$year, data_preterm$rate_preterm)

# QQ very_preterm_rate
qqplot(data_preterm$year, data_preterm$rate_very_preterm)

#heatmap

heat <- data_preterm[order(data_preterm$year, data_preterm$county),]
heat2 <- data_preterm[order(data_preterm$county, data_preterm$year),]
head(heat)
image(1:522,7:8, as.matrix(heat[1:522, 7:8]))
image(1:522,7:8, as.matrix(heat2[1:522, 7:8]))


#spaghetti for preterm - hard to understand

sp_pt <- data_preterm [, c(1, 2, 7)]
sp_pt <- sp_pt[order(sp_pt$rate_preterm),]

head(sp_pt)

ggplot(sp_pt,aes(x=year,y=rate_preterm,
              county,color=factor(county)))+
  geom_point()+geom_line()

#spaghetti for very preterm - hard to understand

sp_vpt <- data_preterm [, c(1, 2, 8)]
head(sp_vpt)

ggplot(sp_vpt,aes(x=year,y=rate_very_preterm,
                 county,color=factor(county)))+
  geom_point()+geom_line()


# Now we see patterns for preterm
sp_pt %>%
  ggplot( aes(x=year, y=rate_preterm, county, fill=county)) +
  geom_area() +
  scale_fill_viridis(discrete = TRUE) +
  theme(legend.position="none") +
  ggtitle("Rate by county") +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    plot.title = element_text(size=14)
  ) +
  facet_wrap(~county)

# Now we see patterns for  very preterm
sp_vpt %>%
  ggplot( aes(x=year, y=rate_very_preterm, county, fill=county)) +
  geom_area() +
  scale_fill_viridis(discrete = TRUE) +
  theme(legend.position="none") +
  ggtitle("Rate by county") +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    plot.title = element_text(size=14)
  ) +
  facet_wrap(~county)



# another way to see patterns in preterm rate

 evo_pt <- sp_pt %>%
  mutate(county2=county)

evo_pt %>%
  ggplot( aes(x=year, y=rate_preterm)) +
  geom_line( data=evo_pt %>% dplyr::select(-county), aes(group=county2), color="grey", size=1, alpha=2) +
  geom_line( aes(color=county), color="#69b3a2", size=1 )+
  scale_color_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=14),
    panel.grid = element_blank()
  ) +
  ggtitle("Counties comparison for preterm rate") +
  facet_wrap(~county)

#total prem births
evo_pt <- sp_pt %>%
  mutate(county2=county)

evo_pt %>%
  ggplot( aes(x=year, y=preterm.births)) +
  geom_line( data=evo_pt %>% dplyr::select(-county), aes(group=county2), color="grey", size=1, alpha=2) +
  geom_line( aes(color=county), color="#69b3a2", size=1 )+
  scale_color_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=14),
    panel.grid = element_blank()
  ) +
  ggtitle("Counties comparison for preterm rate") +
  facet_wrap(~county)


evo_vpt <- sp_vpt %>%
  mutate(county2=county)

evo_vpt %>%
  ggplot( aes(x=year, y=rate_very_preterm)) +
  geom_line( data=evo_vpt %>% dplyr::select(-county), aes(group=county2), color="grey", size=1, alpha=2) +
  geom_line( aes(color=county), color="#69b3a2", size=1 )+
  scale_color_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=14),
    panel.grid = element_blank()
  ) +
  ggtitle("Counties comparison for very preterm rate") +
  facet_wrap(~county)



# Calculate the correlations between total, PB, VPB.
data_preterm %>% 
  select(total.births, preterm.births, very.preterm.births) %>% 
  cor(method="pearson")
#                        total.births   preterm.births      very.preterm.births
# total.births           1.0000000      0.9988589           0.9956024
# preterm.births         0.9988589      1.0000000           0.9975419
# very.preterm.births    0.9956024      0.9975419           1.0000000


#Not very descriptive heatmap
p_rate <-data_preterm [, c(1, 2, 7)]
head(p_rate)

p_rate$county <- as.numeric(p_rate$county)
p_rate$year <- as.numeric(p_rate$year)
pb_2010 <- filter(p_rate, year == 2010)
head(pb_2010)
lapply(pb_2010, class)

heatmap(as.matrix(pb_2010),
        Rowv=NA,
        Colv=NA,
        col = heat.colors(256),
        scale="column",
        main = "Preterm Births Rate by counties")


ggplot(sp_pt, aes(year, rate_preterm)) +
  geom_point() +
  geom_smooth(color="#69b3a2") +
  theme_ipsum()


ggplot(sp_vpt, aes(year, rate_very_preterm)) +
  geom_point() +
  geom_smooth(color="#69b3a2") +
  theme_ipsum() 




################################################################################

#                                    K-means                                   #

################################################################################

data_preterm
k_pt <- data_preterm [, c(3,5,6)]
str(k_pt)

k_pt$total.births<- as.numeric(k_pt$total.births)

scaled_k_pt <- scale(k_pt)
head(scaled_k_pt)
#gap
set.seed(123)
fviz_nbclust(scaled_k_pt, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")
# k=1

# silhouette
fviz_nbclust(scaled_k_pt, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")
#k=2

# Elbow method
fviz_nbclust(scaled_k_pt, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")
# k=4, but k=3 is also an elbow
# we will use k=3



clusters <- kmeans(scaled_k_pt, centers = 3, nstart = 25)
str(clusters)

clusters$size # gives no. of records in each cluster
# [1] 436  77   9

clusters$centers # gives value of cluster center datapoint value(3 centers for k=3)
# total.births preterm.births very.preterm.births
# 1   -0.3065695     -0.3045436          -0.3072549
# 2    0.9881527      0.9687056           0.9869064
# 3    6.3973921      6.4656297           6.4412624

clusters$cluster
#[1] 2 1 1 1 1 1 1 1 1 2 1 1 1 1 1 1 1 1 3 1 1 1 1 1 1 1 1 1 1 2 1 1 2 2 1 2 2 1 1 1 1 1 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 1 1 1
#[63] 1 1 1 1 1 2 1 1 1 1 2 1 1 1 3 1 1 1 1 1 1 1 1 1 1 2 1 1 2 2 1 2 2 1 1 1 1 1 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 1 1 1 1 1 1 1
#[125] 1 2 1 1 1 1 2 1 1 1 3 1 1 1 1 1 1 1 1 1 1 2 1 1 2 2 1 2 2 1 1 1 1 1 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 1 1 1 1 1 1 1 1 2 1 1
#[187] 1 1 2 1 1 1 3 1 1 1 1 1 1 1 1 1 1 2 1 1 2 2 1 2 2 1 1 1 1 1 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 1 1 1 1 1 1 1 1 2 1 1 1 1 2 1
#[249] 1 1 3 1 1 1 1 1 1 1 1 1 1 2 1 1 2 2 1 2 2 1 1 1 1 1 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 1 1 1 1 1 1 1 1 2 1 1 1 1 2 1 1 1 3 1
#[311] 1 1 1 1 1 1 1 1 1 2 1 1 2 2 1 2 2 1 1 1 1 1 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 1 1 1 1 1 1 1 1 2 1 1 1 1 1 1 1 1 3 1 1 1 1 1
#[373] 1 1 1 1 1 2 1 1 2 2 1 2 2 1 1 1 1 1 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 1 1 1 1 1 1 1 1 2 1 1 1 1 1 1 1 1 3 1 1 1 1 1 1 1 1 1
#[435] 1 2 1 1 2 2 1 2 2 1 1 1 1 1 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 1 1 1 1 1 1 1 1 2 1 1 1 1 1 1 1 1 3 1 1 1 1 1 1 1 1 1 1 2 1 1
#[497] 2 2 1 2 2 1 1 1 1 1 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1

# Perform data visualization for clusters



#A way to visualize clusters
fviz_cluster(clusters, data = k_pt,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             main="Clusters"
)



# Perform data visualization for clusters

par(mfrow=c(2,2), mar=c(5,4,2,2))
# Plot to see how 
# Preterm and Very preterm data points have been distributed in clusters
plot(k_pt$preterm.births, k_pt$very.preterm.births, col=clusters$cluster, main="Distributed in classes")
# Plot to see how Sepal.Length and Sepal.Width data points have been distributed 
# originally as per "class" attribute in dataset
# pch=3 is a "+" symbol, and cex=3 magnifies it. 
# lwd arg is for line width for drawing symbols
points(clusters$centers, pch=3,cex=2,lwd=2)

county.class <- data_preterm[,"county"]
plot(data_preterm[c(5,6)], col=factor(county.class), main="Distributed originally")


table(clusters$cluster,county.class)

#county.class
#Alameda Alpine Amador Butte Calaveras Colusa Contra Costa Del Norte El Dorado Fresno Glenn Humboldt Imperial Inyo Kern Kings
#1       0      9      9     9         9      9            9         9         9      0     9        9        9    9    4     9
#2       9      0      0     0         0      0            0         0         0      9     0        0        0    0    5     0
#3       0      0      0     0         0      0            0         0         0      0     0        0        0    0    0     0
#county.class
#Lake Lassen Los Angeles Madera Marin Mariposa Mendocino Merced Modoc Mono Monterey Napa Nevada Orange Placer Plumas Riverside
#1    9      9           0      9     9        9         9      9     9    9        9    9      9      0      9      9         0
#2    0      0           0      0     0        0         0      0     0    0        0    0      0      9      0      0         9
#3    0      0           9      0     0        0         0      0     0    0        0    0      0      0      0      0         0
#county.class
#Sacramento San Benito San Bernardino San Diego San Francisco San Joaquin San Luis Obispo San Mateo Santa Barbara Santa Clara
#1          0          9              0         0             9           9               9         9             9           0
#2          9          0              9         9             0           0               0         0             0           9
#3          0          0              0         0             0           0               0         0             0           0
#county.class
#Santa Cruz Shasta Sierra Siskiyou Solano Sonoma Stanislaus Sutter Tehama Trinity Tulare Tuolumne Ventura Yolo Yuba
#1          9      9      9        9      9      9          9      9      9       9      9        9       9    9    9
#2          0      0      0        0      0      0          0      0      0       0      0        0       0    0    0
#3          0      0      0        0      0      0          0      0      0       0      0        0       0    0    0

