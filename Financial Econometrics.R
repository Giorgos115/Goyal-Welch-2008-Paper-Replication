#_______________________________________________________________________________
#                               Start of code
#_______________________________________________________________________________


#We utilize the following libraries
#_______________________________________________________________________________
library(data.table)
library(ggplot2)
library(lubridate)
library(dyn)
library(reshape2)
library(knitr)
library(kableExtra)

#_______________________________________________________________________________
#                                Methodology
#_______________________________________________________________________________


##1. We create the S&P 500 annual data-set. We decided to use a data-table instead of data-frame for more efficient data manipulation
#_______________________________________________________________________________

AnnualData  <- read.csv("/Users/george/Desktop/Yearlydata.csv", na.strings="NaN", stringsAsFactors=FALSE)
AnnualData  <- as.data.table(AnnualData)

##2. Begin by calculating important financial metrics 
#_______________________________________________________________________________

#Calculate IndexDiv = Sum of the S&P 500 index and its 12-month moving sum of dividends
#Calculate Dividend Price ratio = log of dividends-log of price 
#Calculate Earnings Price ratio = log of earnings-log of price

AnnualData <- AnnualData[, IndexDiv := Index + D12]
AnnualData <- AnnualData[, dpratio := log(D12) - log(Index)]
AnnualData <- AnnualData[, epratio := log(E12) - log(Index)]

#Compute the Dividend Yield ratio = log of dividends-log of lagged prices
#logret calculates the log return of the index. We added an NA in our code given that it is not possible to compute the logret and dyratio of the first year

dividend.yield <- c(NA, AnnualData[2:nrow(AnnualData), log(D12)] - AnnualData[1:(nrow(AnnualData)-1), log(Index)])
AnnualData <- AnnualData[, dyratio := dividend.yield]
AnnualData <- AnnualData[, logret :=c(NA,diff(log(Index)))] 

#Quantify log dividend returns and calculate the Equity Premium

AnnualData[, logreturndiv := log(IndexDiv) - shift(log(Index), 1, type = "lag")]
AnnualData <- AnnualData[, logRfree := log(Rfree + 1)]
AnnualData <- AnnualData[, Equityprem := logreturndiv - logRfree]

##3. Visualize the results
#_______________________________________________________________________________

#Plot the Equity Premium

ggplot(AnnualData, aes(x = Year, y = Equityprem)) +
  geom_line(color="#EE4000") +  
  labs(title = "Equity Premium ", x = "Year", y = "Value") +
  theme_minimal()  

#Plot the Dividend Price ratio

ggplot(AnnualData, aes(x = Year, y = dpratio)) +
  geom_line(color="#90EE90") +  
  labs(title = "Dividend Price ratio", x = "Year", y = "Value") +
  theme_minimal()  

#Plot the Dividend Yield ratio

ggplot(AnnualData, aes(x = Year, y = dyratio)) +
  geom_line(color="#EEEE00") + 
  labs(title = "Dividend Yield ratio", x = "Year", y = "Value") +
  theme_minimal()  

#Plot the IndexDiv

ggplot(AnnualData, aes(x = Year, y = IndexDiv)) +
  geom_line(color="#4F94CD") +  
  labs(title = "IndexDiv", x = "Year", y = "Value") +
  theme_minimal() 

#Plot the Earnings Price ratio

ggplot(AnnualData, aes(x = Year, y = epratio)) +
  geom_line(color="#4F94CD") +  
  labs(title = "Earnings Price ratio", x = "Year", y = "Value") +
  theme_minimal() 

#Calculate the Mean and Standard Deviation of the Equity Premium between the three tested periods to check if they align with the paper's results 
estimation_periods <- list(c(1872, 2005), c(1927, 2005), c(1965, 2005))
calculate_statistics <- function(Dataset, start_year, end_year) {
  subset_Dataset <- Dataset[Year >= start_year & Year <= end_year, Equityprem]
  mean_val <- mean(subset_Dataset, na.rm = TRUE)
  std_dev <- sd(subset_Dataset, na.rm = TRUE)
  return(c(mean = mean_val*100, std_dev = std_dev*100))
}

Statistics <- lapply(estimation_periods, function(period) {
calculate_statistics(AnnualData, period[1], period[2])
})

#Create a table with the results
table_data <- matrix(unlist(Statistics), ncol = 2, byrow = TRUE)
rownames(table_data) <- c("1872-2005", "1927-2005", "1965-2005")
colnames(table_data) <- c("Mean(%)", "Standard Deviation(%)")
table_df <- as.data.frame(table_data)
kable(table_df, "html") %>%
kable_styling(full_width = FALSE)

##4. Convert the annual data-set to time series to use function get_statistics
#_______________________________________________________________________________

ts_AnnualData <- ts(AnnualData, start=AnnualData[1,Year], end=AnnualData[nrow(AnnualData), Year])

##5. Conduct In-Sample and Out-of-Sample analysis 
#_______________________________________________________________________________

#Parameters:
#ts_df: The time series data-frame we seek to analyze
#indep: The independent variable used in the regression model
#dep: The dependent variable being predicted
#h: Forecast horizon, i.e., how far ahead the model will attempt to forecast the dependent variable's value from the independent variable
#start, end: The start and end years for the analysis period
#est_periods_OOS: The number of periods to be used for Out-of-Sample estimation

get_statistics <- function(ts_df, indep, dep, h=1, start=1872, end=2005, est_periods_OOS = 20)
{

#In-Sample test analysis

#5.1: Historical mean model(calculate the average of the dep variable during the pre-determined period)

avg <- mean(window(ts_df, start, end)[, dep], na.rm=TRUE)

#Calculate the historical mean model's In-Sample inaccuracy

IS_error_HM <- (window(ts_df, start, end)[, dep] - avg)

#5.2: OLS model(use dyn$lm to regress dep variable on the lagged version of the indep variable)

reg <- dyn$lm(eval(parse(text=dep)) ~ lag(eval(parse(text=indep)), -1), data=window(ts_df, start, end))

#Calculate the In-Sample error of the OLS model

IS_error_OLS <- reg$residuals
  
##6. Out-of-Sample test Analysis
#_______________________________________________________________________________

OOS_error_HM <- numeric(end - start - est_periods_OOS)
OOS_error_OLS <- numeric(end - start - est_periods_OOS)

#Ensure that the model's predictions are realistic by utilizing information available up to the forecast date, basically mimicking a real-world forecasting situation

j <- 0
for (i in (start + est_periods_OOS):(end-1)) {
j <- j + 1

#Extract the realized Equity Premium value for the year immediately after the current forecasting window
Real_ERP <- as.numeric(window(ts_df, i+1, i+1)[, dep])

#6.1: Historical mean model

#Forecast the error of the historical mean model on each Out-of-Sample period (j)

OOS_error_HM[j] <- Real_ERP - mean(window(ts_df, start, i)[, dep], na.rm=TRUE)

#6.2: OLS model

#A new OLS regression model is fitted for each Out-of-Sample period 
#The eval(parse(text=...)) construction dynamically constructs and evaluates a formula for regression using variable names specified as strings.

reg_OOS <- dyn$lm(eval(parse(text=dep)) ~ lag(eval(parse(text=indep)), -1), 
                      data=window(ts_df, start, i))

#Construct a data-table for the independent variable at time i, which is used for Out-of-Sample prediction with the dynamically fitted OLS model 

df <- data.table(x=as.numeric(window(ts_df, i, i)[, indep]))
names(df) <- indep

#Forecast the Equity Risk Premium for a specific future period based on inputs from a regression model

Predicted_ERP <- predict.lm(reg_OOS, newdata=df)

#Calculate the prediction error by subtracting the actual value from the predicted value 

OOS_error_OLS[j] <-  Predicted_ERP - Real_ERP
}

##7. Compute the Mean Squared Errors
#_______________________________________________________________________________

MSE_HM <- mean(OOS_error_HM^2, na.rm = TRUE)
MSE_OLS <- mean(OOS_error_OLS^2, na.rm = TRUE)

##8. Calculate the Un-Adjusted Out-of-Sample R-squared
OOS_R2 <- 1 - (MSE_OLS / MSE_HM)
#_______________________________________________________________________________

#Calculate the number of observations and number of predictors
n <- length(OOS_error_OLS)  # Number of out-of-sample observations
k <- 1  # We assume one predictor for simplicity

##9. Calculate the Adjusted Out-of-Sample R-squared
#_______________________________________________________________________________

OOS_R2a <- OOS_R2 - (1 - OOS_R2) * (k / (n - k - 1))

##10. Compute the difference between root mean squared error between the HM and OLS models
#_______________________________________________________________________________

dRMSE <- sqrt(MSE_HM) - sqrt(MSE_OLS)

##11. Compute the cumulative sum of squared errors for both In-Sample and Out-of-Sample predictions
#_______________________________________________________________________________

IS  <- cumsum(IS_error_HM[2:length(IS_error_HM)]^2)-cumsum(IS_error_OLS^2)
OOS <- cumsum(OOS_error_HM^2)-cumsum(OOS_error_OLS^2)
df  <- data.frame(x=seq.int(from=start + 1 + est_periods_OOS, to=end), 
                    IS=IS[(1 + est_periods_OOS):length(IS)], OOS=OOS) 

##12. Adjust the In-Sample line to be at point zero on the first Out-of-Sample prediction point
#_______________________________________________________________________________

df$IS <- df$IS - df$IS[1] 
df  <- melt(df, id.var="x") 

##13. Plot Cumulative SSE Difference
#_______________________________________________________________________________

plotGG <- ggplot(df) + 
  geom_line(aes(x=x, y=value,color=variable)) + 
  geom_rect(data=data.frame(), 
              aes(xmin=1973, xmax=1975, ymin=-0.2, ymax=0.2), 
              fill='#CD2626',
              alpha=0.1) + 
    scale_y_continuous('Cumulative SSE Difference', limits=c(-0.2, 0.2)) + 
    scale_x_continuous('Year') +
scale_color_manual(values = c("IS" = "#87CEFF", "OOS" = "#EE6363")) +
annotate("text", x=1971, y=0.15, label="Oil Shock", size=4, angle=90, color="#474747")+
annotate(geom = "text", x = 1977, y = 0.15, label = "1974", color = "#474747", angle = 90, size = 4)




return(list(IS_error_HM = IS_error_HM,
              IS_error_OLS = reg$residuals,
              OOS_error_HM = OOS_error_HM,
              OOS_error_OLS = OOS_error_OLS,
              IS_R2 = summary(reg)$r.squared, 
              IS_aR2 = summary(reg)$adj.r.squared, 
              OOS_R2  = OOS_R2,
              OOS_R2a = OOS_R2a,
              dRMSE = dRMSE,
              MSE_HM = MSE_HM,
              MSE_OLS = MSE_OLS,
              plotGG = plotGG))
}

dp_stat <- get_statistics(ts_AnnualData, "dpratio", "Equityprem", start=1872)
dp_plot1 <- dp_stat$plotGG + ggtitle("Dividend Price ratio")
print(dp_plot1) 
dy_stat <- get_statistics(ts_AnnualData, "dyratio", "Equityprem", start=1872)
dp_plot2 <- dy_stat$plotGG + ggtitle("Dividend Yield ratio")
print(dp_plot2)  
ep_stat <- get_statistics(ts_AnnualData, "epratio", "Equityprem", start=1872)
dp_plot3 <- ep_stat$plotGG + ggtitle("Earnings Price ratio")
print(dp_plot3)  

##14. Create a table including R-Squared and ΔRMSE data
#_______________________________________________________________________________

results_df <- data.frame(  Variable = c("Dividend Price ratio", "Dividend Yield ratio", "Earnings Price ratio"),  
                           IS_R2 = c(dp_stat$IS_R2, dy_stat$IS_R2, ep_stat$IS_R2)*100,   
                           IS_aR2 = c(dp_stat$IS_aR2, dy_stat$IS_aR2, ep_stat$IS_aR2)*100,  
                           OOS_R2 = c(dp_stat$OOS_R2, dy_stat$OOS_R2, ep_stat$OOS_R2)*100,  
                           OOS_R2a = c(dp_stat$OOS_R2a, dy_stat$OOS_R2a, ep_stat$OOS_R2a)*100,  
                           ΔRMSE = c(dp_stat$dRMSE, dy_stat$dRMSE, ep_stat$dRMSE)*100)
kable(results_df, caption = "Summary of statistical analysis results when forecasts begin 20 years after sample(%)", align = "c") %>%  
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))


##15. Now, we conduct the second forecast, where we ignore all data prior to 1927  
#_______________________________________________________________________________

#Calculate statistics for each variable

start_year_third <- 1927
dp_stat_1927 <- get_statistics(ts_AnnualData, "dpratio", "Equityprem", start = start_year_third)
dy_stat_1927 <- get_statistics(ts_AnnualData, "dyratio", "Equityprem", start = start_year_third)
ep_stat_1927 <- get_statistics(ts_AnnualData, "epratio", "Equityprem", start = start_year_third)

#Create a data-table containing the results

results_df_third <- data.table(
Variable = c("Dividend Price ratio", "Dividend Yield ratio", "Earnings Price ratio"),
IS_adR2 = c(dp_stat_1927$IS_aR2, dy_stat_1927$IS_aR2, ep_stat_1927$IS_aR2) * 100)
kable(results_df_third, caption = "Summary of statistical analysis results for the 1927-2005 In-Sample period(%)", align = "c") %>%
kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

#_______________________________________________________________________________
                                 #End of code
#_______________________________________________________________________________

#Note that we tried different methods to test the third tested period, i.e., 
#the one where forecasts start in 1965. However, the results were not
#consistent with those of the paper. Therefore, we decided to not include them









