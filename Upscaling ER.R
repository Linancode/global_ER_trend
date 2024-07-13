library(h2o)
library(raster)

h2o.init()
setwd("Z:/TIF_monthly/")

path1 <- "Z:/data.csv"

ER_data<-read.csv(path1)
ER_Train<-as.h2o(ER_data)

# PFT<-c("CRO","ENF","DBF","EBF","SAV","WET","GRA","SH")
# PFT1<-c("MF")
# Train1<-ER_data[which(ER_data$PFT %in% PFT),]
# Test1<-ER_data[which(ER_data$PFT %in% PFT1),]
# 
# # Train1<-ER_data[which(ER_data$Zone %in% c("Tropical","Boreal")),]
# # Test1<-ER_data[which(ER_data$Zone %in% c("Temperate")),]
# 
# split_point <- sample(nrow(Test1), ceiling(0.9 * nrow(Test1)))
# train_ind <- sort(split_point)
# test_ind <- setdiff(1:nrow(Test1), train_ind)
# 
# train_df <- Test1[train_ind, ]
# test_df <- Test1[test_ind, ]
# 
# Train2<-rbind(Train1,train_df)
# 
# 
# ER_Train<-as.h2o(Train2)
# ER_Test<-as.h2o(test_df)

# set the predictor names and the response column name
# predictors <- colnames(ER_Train)[2:17]
predictors <- colnames(ER_Train)[2:16]
# set the response
name1<-"RECO"
response <-name1
# split into train sets
boston_splits <- h2o.splitFrame(data = ER_Train, ratios = 0.8) ##ratios = 0.7,seed = 66
train <- boston_splits[[1]]
valid <- boston_splits[[2]]
######Model
boston_rf <- h2o.randomForest(x = predictors,
                              y = response,nfolds = 5,mtries = 10,ntrees = 1000,
                              keep_cross_validation_models = TRUE,
                              training_frame = train,
                              validation_frame=valid)

# boston_rf <- h2o.automl(x = predictors,y = response,training_frame = train,
#                         nfolds = 5,max_runtime_secs =1800,max_models = 20,
#                         keep_cross_validation_models = TRUE,validation_frame=valid)

# boston_rf<-h2o.gbm(x = predictors,y = response,training_frame = train,ntrees = 500,nfolds = 5,
#                    score_tree_interval = 10,stopping_rounds = 3,stopping_metric = "MSE",
#                    stopping_tolerance = 0.00005)

# boston_rf<- h2o.xgboost(x = predictors, y = response,
#                 training_frame = train, validation_frame = valid,nfolds = 5,
#                 booster = "dart", normalize_type = "tree")

# boston_rf<- h2o.deeplearning(x = predictors, y = response, training_frame = train,
#                              keep_cross_validation_models = TRUE,
#                              nfolds = 5,validation_frame=valid)

# boston_rf<- h2o.glm(x = predictors,y = response,training_frame = train,lambda = 0.25,compute_p_values = F)


h2o.r2(boston_rf)
h2o.r2(boston_rf,valid = T)
h2o.rmse(boston_rf,valid = T)
# perf <- h2o.performance(boston_rf)
# h2o.r2(boston_rf@leader)
# h2o.r2(boston_rf@leader,valid = T)
# h2o.rmse(boston_rf@leader,valid = T)


for (m in 1:12) {
  dataInHome<-paste('Z:/Testdata/data_',c(m),"/",sep="")
  flist1<-list.files(dataInHome, pattern="Test", all.files=FALSE, full.names=FALSE)
  year<-1989
  for (i in 1:30) {
    fname1<-flist1[i]
    path2<-paste(dataInHome,fname1,sep="")
    ER_Test<-read.csv(path2)
    ER_Test<-as.h2o(ER_Test)
    
    # predict using the GLM model and the testing dataset
    predict <- h2o.predict(object = boston_rf, newdata = ER_Test)
    pre<-as.data.frame(predict)
    test<-as.data.frame(ER_Test)
    test<-test[,-c(3:18)]
    result<-cbind(test,pre)
    
    data_NA<-read.csv(path2)
    list<-which(rowSums(is.na(data_NA)) > 0)
    result2<-result[list,]
    result2$predict<-NA
    result3<-result[-list,]
    result_all<-rbind(result2,result3)
    pp<-rasterFromXYZ(result_all)
    proj4string(pp)<-CRS("+proj=longlat +datum=WGS84")
    
    if(m<10){
      savepath<-paste("ER",c(year),"0",c(m),".tif",sep = "")
    } else
    {savepath<-paste("ER",c(year),c(m),".tif",sep = "")}
    
    writeRaster(pp, savepath,overwrite=TRUE)
    year<-year+1
  }
}
