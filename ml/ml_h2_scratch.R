#points distribution looks pretty normal: 
ggplot(wine, aes(x = points)) + geom_histogram()

#some helpful aggregate numbers
points_min = min(wine$points)
points_max = max(wine$points)
avg_points = mean(wine$points)
sd_points = sd(wine$points)


wino <- wine %>% 
  mutate(lprice=log(price),
         #feature_1: is wine described as "rich"?
         is_rich = str_detect(description,"[Rr]ich"),
         #feature_2: anne Krebiehl gives highest avg_points, does her generous ratings affect price? 
         anne = str_detect(taster_name,"Krebiehl"),
         #feature_3: binning year
         epoch = case_when( 
           year %in% 1995:2001 ~ "1995-2001",
           year %in% 2002:2008 ~ "2002-2008",
           year %in% 2009:2015 ~ "2009-2015"),
         #feature_4: syrah seem have bad rep, is it a predictor of price? 
         is_syrah_grape = (variety %in% c("Syrah","Shiraz")),
         #feature_5
         chateau = str_detect(winery, "Château"),
         #feature_6
         normalized_points = (points - points_min)/(points_max - points_min),
         #feature_7
         standardized_points = (points - avg_points)/sd_points,
         #feature_8
         good_with_cheese = str_detect(description, "[Cc]heese"),
         #feature_9
         estate = designation == "Estate",
         #feature_10
         is_firm = str_detect(description, "[Ff]irm")) %>%
  drop_na(is_rich,anne,epoch,is_syrah_grape,chateau,normalized_points,standardized_points,good_with_cheese,estate,is_firm) %>% 
  select(lprice,points,is_rich,anne,epoch,is_syrah_grape,chateau,normalized_points,standardized_points,good_with_cheese,estate,is_firm)

wino = wino %>% select(-winery, -winery_Other)

wino_index = createDataPartition(wino$lprice, p  = 0.8, list = FALSE) 

wino_train <- wino[wino_index, ]
wino_test <- wino[-wino_index, ]

control <- trainControl(method="boot", number=5)

wino_model <- train(lprice ~ .,
            data = wino_train,
            method = "lm",
            trControl = control)

wino_model

m2_pred = predict(m2, m2_test)
postResample(pred = m2_pred, obs=m2_test$lprice)

importance <- varImp(wino_model, scale=TRUE)
plot(importance)

library(mlbench) 
data(Sonar)
str(Sonar[,1:10])

head(Sonar)

library(caret)
set.seed(998)
inTraining <- createDataPartition(Sonar$Class, p = .75, list = FALSE)
training <- Sonar[ inTraining,]
testing  <- Sonar[-inTraining,]


####
m2_data = wine %>% 
  mutate(
    lprice = log(price),
    #features based on grape variety: 
    pinot_noir = as.integer(variety == "Pinot Noir"),
    cab_sav = as.integer(variety == "Cabernet Sauvignon"),
    france = as.integer(country == "France"),
    usa = as.integer(country == "US"),
    oak = as.integer(str_detect(description, "[Oo]ak")),
    y2010_2015 = (year >=2010 & year <= 2015),
    france_pinot = france*pinot_noir,
    italy = as.integer(country == "Italy"),
    usa_points = points*usa, 
    plum = str_detect(description, "[Pp]lum"),
    napa = as.integer(str_detect(region_1, "[Nn]apa"))) %>% 
  select(points, y2010_2015, lprice, pinot_noir, cab_sav,france,usa_points,france_pinot,usa,oak,italy,napa,plum) %>% 
  drop_na(.)


m2_data
m2_index = createDataPartition(m2_data$lprice, p  = 0.8, list = FALSE) 

m2_train <- m2_data[m2_index, ]
m2_test <- m2_data[-m2_index, ]

control <- trainControl(method="boot", number=5)

m2 <- train(lprice ~ .,
            data = m2_train,
            method = "lm",
            trControl = control)

m2

m2_pred = predict(m2, m2_test)
postResample(pred = m2_pred, obs=m2_test$lprice)

importance <- varImp(m2, scale=TRUE)
# plot importance
plot(importance)


wine %>% filter(str_detect(region_2, "Napa")) %>% filter(str_detect(description, "[Oo]ak"))

wine %>% filter(variety == "Rosé")
#pinot_noir = as.integer(variety == "Pinot Noir")
#cab = as.integer(variety == "Cabernet Sauvignon"),
#cab_france = cab*france) 
#tobacco = as.integer(str_detect(description, "tobacco"))

