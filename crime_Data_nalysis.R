

#######Yeran Tong##########



library(psych)
library(ggplot2)
library(dplyr)
library(tidyr)
library(zoo)
library(forecast)
library(ggpmisc)
library(leaflet)



#####################DATA--MANIPULATION#######################################################################

crime_data <- data.frame(read.csv("/Users/yerantong/Desktop/CrimeData/Boston_Crime_Data.csv", na.strings=c("","NA")))
str(crime_data)
colSums(is.na(crime_data))

crime_data <- crime_data %>%
  separate(OCCURRED_ON_DATE, c("DATE", "TIME"), " ")

crime_data$DATE <- as.Date(crime_data$DATE,format = '%m/%d/%Y')
crime_data<-crime_data[order(crime_data$DATE),]

crime_data$YEARMONTH<- paste(crime_data$YEAR,crime_data$MONTH,sep = '-')

crime_data$YEARMONTH <- as.Date(as.yearmon(crime_data$YEARMONTH))
str(crime_data)

id <-c('INCIDENT_NUMBER')
crime_type <- c('OFFENSE_CODE','OFFENSE_CODE_GROUP','UCR_PART','OFFENSE_DESCRIPTION','SHOOTING')
time <- c('DATE','TIME','YEAR','MONTH','DAY_OF_WEEK','HOUR','YEARMONTH')
Location <- c('DISTRICT','STREET','Lat','Long','Location')

crime_data <- crime_data[,c(id,crime_type,time,Location)]
str(crime_data)
colSums(is.na(crime_data))

crime_dfA <- crime_data[is.na(crime_data$OFFENSE_CODE_GROUP),] 
crime_dfB <- crime_data[!is.na(crime_data$OFFENSE_CODE_GROUP),]


crime_df_unique <- distinct(crime_data, OFFENSE_CODE, .keep_all = TRUE)
crime_df_unique <- crime_df_unique[crime_df_unique$OFFENSE_CODE_GROUP!='NA',]


dict <- list()
for (i in (1:nrow(crime_df_unique))){
  key <- as.character(crime_df_unique[i,'OFFENSE_CODE'])
  value <- crime_df_unique[i,'OFFENSE_CODE_GROUP']
  dict[[key]] <- value
}

#crime_data$OFFENSE_CODE_GROUP[is.na(crime_data$OFFENSE_CODE_GROUP)] <- "None"
#unique(crime_data$OFFENSE_CODE_GROUP)

for (i in (1:nrow(crime_dfA))){
  key <- as.character(crime_dfA[i,'OFFENSE_CODE'])
  value <- dict[[key]]
  if (length(value)!=0){
    crime_dfA[i,'OFFENSE_CODE_GROUP'] <- value
  }
}
DF <- rbind.data.frame(crime_dfA,crime_dfB)
DF <- DF[order(crime_data$DATE),]

colSums(is.na(DF))
summary(DF)

#####################DATA--MANIPULATION#######################################################################


#####################Frequency    Analysis####################################################################
tbl_crime <- table(DF$OFFENSE_CODE_GROUP)
df_frequency <- as.data.frame(tbl_crime, responseName='f' ) 
df_frequency <- df_frequency[order(df_frequency$f),]
df_frequency$cf <- cumsum( df_frequency$f )
df_frequency$p  <- paste(sprintf( '%.2f', df_frequency$f*100 / max( df_frequency$cf ) ),'%')
df_frequency$cp <- sprintf( '%.2f', df_frequency$cf / max( df_frequency$cf ) )
row.names(df_frequency) <- NULL
df_frequency$Var1 <- factor(df_frequency$Var1, levels=unique(df_frequency$Var1))
df_frequency


barp <-ggplot(DF, aes(x = factor(OFFENSE_CODE_GROUP, levels=unique(df_frequency$Var1)))) + 
  geom_bar() +
  theme_minimal() + 
  ylab("Total Cases")+
  xlab("Crime Class")+
  theme(axis.text.x=element_text(angle=60, hjust=1))
barp

barp2 <- barp + facet_wrap( ~ YEAR)
barp2

barp3<-ggplot(DF, aes(x = factor(UCR_PART, levels=c('Part One','Part Two','Part Three','Other')))) + 
  geom_bar() +
  theme_minimal() + 
  ylab("Total Cases")+
  xlab("Crime Class")+
  theme(axis.text.x=element_text(angle=60, hjust=1))
barp3

barp4 <- barp3 + facet_wrap( ~ YEARMONTH)
barp4

barp5<-ggplot(DF, aes(x = factor(OFFENSE_CODE_GROUP, levels=unique(df_frequency$Var1)), fill = factor(YEAR))) + 
  geom_bar() +
  coord_flip()+
  theme_minimal() + 
  ylab("Total Cases")+
  xlab("Crime Class")
barp5


piep <- ggplot(df_frequency, aes(x = '', y = f,fill=Var1)) +  
  geom_col(color = "black") +
  coord_polar(theta = 'y') +
  geom_text(aes(label = p),
            position = position_stack(vjust = 0.5))+
  ggtitle("2015-2020 crime")+
  theme_void()
piep

##############################Seasonality Analysis##########################################################

tsdf <- DF %>% count( YEARMONTH ,sort=TRUE)
tsdf

ts1 <- ggplot(tsdf, aes(x=YEARMONTH, y=n)) +
  geom_line(color="steelblue") + 
  geom_point(color="red") +
  xlab("Time") +
  scale_x_date(date_breaks = "2 month", date_labels = "%b-%Y")
ts1

tsdf_group <- DF %>% count(OFFENSE_CODE_GROUP, YEARMONTH ,sort=TRUE)
ts2 <- ggplot(tsdf_group, aes(x=YEARMONTH,y=n)) +
  geom_point(aes(colour = factor(OFFENSE_CODE_GROUP))) +
  geom_line(aes(colour = factor(OFFENSE_CODE_GROUP)))+
  xlab("Time") +
  scale_x_date(date_labels = "%b-%Y")
ts2

ts3 <- ggplot(tsdf_group, aes(YEARMONTH, n, fill=YEARMONTH))+
  labs( x='Time', title='seasonality analysis', subtitle = 'Crime cases along the time')+
  geom_point( stat='identity', size=0.8) + facet_wrap( ~ OFFENSE_CODE_GROUP)
ts3

tsdf_larceny <- tsdf_group[tsdf_group['OFFENSE_CODE_GROUP']=='Larceny',]
tsdf_larceny <- tsdf_larceny[-nrow(tsdf_larceny),]
larcenyp <- ggplot(tsdf_larceny,aes(x=YEARMONTH, y=n)) +
  geom_line(color="steelblue") + 
  geom_point(color="red") +
  xlab("Time") +
  ylab('Larceny Cases')+
scale_x_date(date_breaks = "2 month", date_labels = "%b-%Y")
larcenyp

larcenyp1 <- larcenyp +
geom_smooth(formula = y ~ x, method='lm')
larcenyp1

summary(lm(n~YEARMONTH,data = tsdf_larceny))

larcenyp2 <- larcenyp1 +  
  stat_smooth(color = "#FC4E07", method = "loess" )
larcenyp2


tsdf_UCR <- DF %>% count(UCR_PART, YEARMONTH ,sort=TRUE)
tsdf4 <- ggplot(tsdf_UCR, aes(YEARMONTH, n))+
  labs( x='Time', title='seasonality analysis', subtitle = 'Crime cases along the time')+
  geom_point( stat='identity', size=0.8) + facet_wrap( ~ UCR_PART)
tsdf4

#################################time window-crime analysis#####################################
#Use DAY_OF_WEEK and HOUR
time_window_crime <- DF %>% count(DAY_OF_WEEK,HOUR,sort=TRUE)
time_window_crime
plot <- ggplot(time_window_crime, aes(x = HOUR, y = factor(DAY_OF_WEEK, levels = c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')), fill = n)) +
  geom_tile() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.6),panel.spacing.x=unit(0.5, "cm"))+
  labs(x = "Hour of Crime", y = "Day of Week", title = "Number of Crime in Boston from 2015 - 2020") +
  scale_fill_gradient(low = "white", high = "#000066")
plot

df_top_crimes <- DF %>%
  group_by(OFFENSE_CODE_GROUP) %>% 
  summarize(count = n()) %>%
  arrange(desc(count))

top_10_crimes <- df_top_crimes$OFFENSE_CODE_GROUP[1:10]

df_top_10_crimes <- DF[,c('OFFENSE_CODE_GROUP','HOUR','DAY_OF_WEEK')]

df_top_10_crimes <- df_top_10_crimes %>%
  filter(OFFENSE_CODE_GROUP %in% top_10_crimes) %>%
  group_by(OFFENSE_CODE_GROUP, DAY_OF_WEEK, HOUR) %>% 
  summarize(count = n())

df_top_10_crimes$DAY_OF_WEEK <- factor(df_top_10_crimes$DAY_OF_WEEK,levels = c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'))
df_top_10_crimes$HOUR <- factor(df_top_10_crimes$HOUR, level = 0:23)

df_top_10_crimes <- df_top_10_crimes %>%
  group_by(OFFENSE_CODE_GROUP) %>%
  mutate(norm = count/sum(count))

top10_p <- ggplot(df_top_10_crimes , aes(x = HOUR, y = DAY_OF_WEEK, fill = count)) +
  geom_tile() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.6, size = 4)) +
  labs(x = "Hour of Arrest", y = "Day of Week") +
  scale_fill_gradient(low = "white", high = "#2980B9") +
  facet_wrap(~ OFFENSE_CODE_GROUP, nrow = 5)
top10_p


####################District-Crime-Analysis#########################################################
names(DF)
dist_crime <- DF %>% count( DISTRICT,MONTH,sort=TRUE)
dist_crime

ggplot(dist_crime, aes(x = factor(MONTH, levels = seq(1,12,1)), y=n)) +
  geom_point(size=0.5)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.6, size = 4)) +
  labs(x = "Month", y = "Crime Cases") +
  facet_wrap(~ DISTRICT, nrow = 5)

#    mean
dist_crime %>% group_by(DISTRICT) %>% 
  summarise(n=mean(n))


df_top_10_crimes_dist <- DF[,c('OFFENSE_CODE_GROUP','DISTRICT','YEAR')]
df_top_10_crimes_dist <- df_top_10_crimes_dist %>% filter(OFFENSE_CODE_GROUP %in% top_10_crimes)
ggplot(df_top_10_crimes_dist, aes(x = OFFENSE_CODE_GROUP )) +
  geom_bar()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.6, size = 4)) +
  labs(x = "Month", y = "Crime Cases") +
  facet_wrap(~ DISTRICT, nrow = 6)


############visualize on map####################################3

DF$popup <- paste("<b>Incident #: </b>", DF$INCIDENT_NUMBER, "<br>", "<b>Category: </b>", DF$OFFENSE_CODE_GROUP,
                    "<br>", "<b>Description: </b>", DF$OFFENSE_DESCRIPTION,
                    "<br>", "<b>Day of week: </b>", DF$DAY_OF_WEEK,
                    "<br>", "<b>Date: </b>", DF$DATE,
                    "<br>", "<b>Time: </b>", DF$TIME,
                    "<br>", "<b>Address: </b>", DF$STREET,
                    "<br>", "<b>Longitude: </b>", DF$Long,
                    "<br>", "<b>Latitude: </b>", DF$Lat)

mapvis_df <- DF %>% drop_na(Long,Lat)

leaflet(mapvis_df, width = "100%") %>% addTiles() %>%
  addTiles(group = "OSM (default)") %>%
  addProviderTiles(provider = "Esri.WorldStreetMap",group = "World StreetMap") %>%
  addProviderTiles(provider = "Esri.WorldImagery",group = "World Imagery") %>%
  addMarkers(lng = ~Long, lat = ~Lat, popup = mapvis_df$popup, clusterOptions = markerClusterOptions()) %>%
  addLayersControl(
    baseGroups = c("OSM (default)","World StreetMap", "World Imagery"),
    options = layersControlOptions(collapsed = FALSE)
  )




