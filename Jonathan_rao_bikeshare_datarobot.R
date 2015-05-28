season_weather_counts <-as.data.frame(aggregate(train[,"count"], list(train$season, train$weather), mean))
 season_weather_counts$weather <- as.numeric(as.character(season_weather_counts$Group.2))
 season_weather_counts$season <- as.numeric(as.character(season_weather_counts$Group.1))
 season_weather_counts$weatherN <-factor(season_weather_counts$weather)
 season_weather_counts$weatherN <- factor(season_weather_counts$weather,labels=c("Clear, Few clouds,\n Partly cloudy, Partly cloudy","Mist + Cloudy,Mist +\n Broken clouds, Mist + Few clouds, Mist","Light Snow, Light Rain +\n Thunderstorm + Scattered clouds,\n Light Rain + Scattered clouds","Heavy Rain + Ice Pallets +\n Thunderstorm + Mist,\n Snow + Fog"))
 season_weather_counts$seasonN <-factor(season_weather_counts$season)
 season_weather_counts$seasonN <- factor(season_weather_counts$season,labels=c("spring","summer","fall","winter"))
 ggplot(season_weather_counts, aes( x = seasonN,y = weatherN)) + geom_tile(aes(fill = x)) + scale_fill_gradient(name="Season-weather wise average Counts", low="white", high="green") + theme(axis.title.y = element_blank())




 day_hour_counts <- as.data.frame(aggregate(train[,"count"], list(train$weekday, train$hour), mean))
 day_hour_counts$Group.1 <- factor(day_hour_counts$Group.1, ordered=TRUE, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
 day_hour_counts$hour <- as.numeric(as.character(day_hour_counts$Group.2))
# plot heat mat with ggplot
  ggplot(day_hour_counts, aes(x = Group.1, y = hour)) + geom_tile(aes(fill = x)) + scale_fill_gradient(name="Average Counts", low="white", high="purple") + theme(axis.title.y = element_blank())


 best_counts <-as.data.frame(aggregate(train[,"count"], list(train$season, train$weather,train$weekday), mean))
 best_counts$seasonN <- factor(best_counts$Group.1,labels=c("spring","summer","fall","winter"))
best_counts$weatherN <- factor(best_counts$Group.2,labels=c("Clear, Few clouds,\n Partly cloudy, Partly cloudy","Mist + Cloudy,Mist +\n Broken clouds, Mist + Few clouds, Mist","Light Snow, Light Rain +\n Thunderstorm + Scattered clouds,\n Light Rain + Scattered clouds","Heavy Rain + Ice Pallets +\n Thunderstorm + Mist,\n Snow + Fog"))

best_counts[which.max(best_counts$x),]
best_counts[which.min(best_counts$x),]





 # Poisson Regression
 poisson.glm <- glm(count~temp+atemp+humidity+windspeed+season+holiday+workingday+sunday+weather+daypart,family=poisson, data=train_factor)
 reg





season_weather_counts <-as.data.frame(aggregate(train[,"count"], list(train$season, train$weather), mean))
 season_weather_counts$weather <- as.numeric(as.character(season_weather_counts$Group.2))
 season_weather_counts$season <- as.numeric(as.character(season_weather_counts$Group.1))
 season_weather_counts$weatherN <-factor(season_weather_counts$weather)
 season_weather_counts$weatherN <- factor(season_weather_counts$weather,labels=c("Clear, Few clouds,\n Partly cloudy, Partly cloudy","Mist + Cloudy,Mist +\n Broken clouds, Mist + Few clouds, Mist","Light Snow, Light Rain +\n Thunderstorm + Scattered clouds,\n Light Rain + Scattered clouds","Heavy Rain + Ice Pallets +\n Thunderstorm + Mist,\n Snow + Fog"))
 season_weather_counts$seasonN <-factor(season_weather_counts$season)
 season_weather_counts$seasonN <- factor(season_weather_counts$season,labels=c("spring","summer","fall","winter"))
ggplot(season_weather_counts, aes( x = seasonN,y = weatherN)) + geom_tile(aes(fill = x)) + scale_fill_gradient(name="Season-weather wise average Counts", low="white", high="green") + theme(axis.title.y = element_blank())



poisson.lm <- glm(count~temp+atemp+humidity+windspeed+season+holiday+workingday+sunday+weather+daypart,family=poisson, data=train_factor)
