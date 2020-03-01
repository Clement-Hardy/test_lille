library(gridExtra)
library(pander)


source("data/utils.R")
source("model/arima.R")
source("model/elm.R")
source("model/gbm.R")
source("model/mlp.R")
source("model/nnetar.R")
source("model/prophet.R")
source("model/RF.R")
source("model/TBATS.R")
source("model/xgboost.R")
source("training/train.R")



data <- prepare_data(add_variable = TRUE)

print("Run all algo")
result <- data.frame()
pred_arima <- cross_val_all_store("autoarima", data)
result <- rbind(result, c(pred_arima$mean_rmse, pred_arima$mean_rmspe))
colnames(result) <- c("mean rmse", "mean rmspe")
rownames(result)[1] <- c("arima")

pred_prophet <- cross_val_all_store("prophet", data)
result <- rbind(result, c(pred_prophet$mean_rmse, pred_prophet$mean_rmspe))
rownames(result)[2] <- c("prophet")

pred_TBATS <- cross_val_all_store("tbats", data)
result <- rbind(result, c(pred_TBATS$mean_rmse, pred_TBATS$mean_rmspe))
rownames(result)[3] <- c("tbats")

pred_elm <- cross_val_all_store("elm", data)
result <- rbind(result, c(pred_elm$mean_rmse, pred_elm$mean_rmspe))
rownames(result)[4] <- c("elm")

pred_mlp <- cross_val_all_store("mlp", data)
result <- rbind(result, c(pred_mlp$mean_rmse, pred_mlp$mean_rmspe))
rownames(result)[5] <- c("mlp")

pred_nnetar <- cross_val_all_store("nnetar", data)
result <- rbind(result, c(pred_nnetar$mean_rmse, pred_nnetar$mean_rmspe))
rownames(result)[6] <- c("nnetar")



pred_gbm <- cross_val_all_store("gbm", data)
result <- rbind(result, c(pred_gbm$mean_rmse, pred_gbm$mean_rmspe))
rownames(result)[7] <- c("gbm")


pred_RF <- cross_val_all_store("RF", data)
result <- rbind(result, c(pred_RF$mean_rmse, pred_RF$mean_rmspe))
rownames(result)[8] <- c("RF")

pred_xgboost <- cross_val_all_store("xgboost", data)
result <- rbind(result, c(pred_xgboost$mean_rmse, pred_xgboost$mean_rmspe))
rownames(result)[9] <- c("xgboost")


pander(result)





print("Compare with addition regressor")
result1 <- data.frame()
pred_prophet_with <- cross_val_all_store("prophet", data, add_reg = TRUE)
result1 <- rbind(result1, c(pred_prophet$mean_rmse, pred_prophet$mean_rmspe))
rownames(result1)[1] <- c("prophet_without")
colnames(result1) <- c("mean rmse", "mean rmspe")
result1 <- rbind(result1, c(pred_prophet_with$mean_rmse, pred_prophet_with$mean_rmspe))
rownames(result1)[2] <- c("prophet_with")


pander(result1)




print("Trying to take into account time in random forest")
result2 <- data.frame()
data <- prepare_data(add_variable = TRUE, add_semi_time=TRUE)
pred_RF_with <- cross_val_all_store("gbm", data)
result2 <- rbind(result2, c(pred_RF$mean_rmse, pred_RF$mean_rmspe))
colnames(result2) <- c("mean rmse", "mean rmspe")
rownames(result2)[1] <- c("RF_without")
result2 <- rbind(result2, c(pred_RF_with$mean_rmse, pred_RF_with$mean_rmspe))
rownames(result2)[2] <- c("RF_with")

pander(result2)




print("Compare without summing sales store")
result3 <- data.frame()
data <- prepare_data(add_variable = TRUE, sum_store = FALSE)
pred_gbm_not_sum <- cross_val_all_store("gbm", data, by_dept = TRUE)
result3 <- rbind(result3, c(pred_gbm$mean_rmse, pred_gbm$mean_rmspe))
rownames(result3)[1] <- c("gbm_sum_sale")
colnames(result3) <- c("mean rmse", "mean rmspe")
result3 <- rbind(result3, c(pred_gbm_not_sum$mean_rmse, pred_gbm_not_sum$mean_rmspe))
rownames(result3)[2] <- c("gbm_not_sum_sale")

pander(result3)





print("Compare optimized gbm")
result4 <- data.frame()
data <- prepare_data(add_variable = TRUE)
pred_gbm <- cross_val_all_store("gbm", data)
pred_gbm_not_optimized <- cross_val_all_store("gbm", data, optimized = FALSE)
result4 <- rbind(result4, c(pred_gbm$mean_rmse, pred_gbm$mean_rmspe))
rownames(result4)[1] <- c("gbm_optimized")
colnames(result4) <- c("mean rmse", "mean rmspe")
result4 <- rbind(result4, c(pred_gbm_not_optimized$mean_rmse, pred_gbm_not_optimized$mean_rmspe))
rownames(result4)[2] <- c("gbm_not_optimized")

pander(result4)
