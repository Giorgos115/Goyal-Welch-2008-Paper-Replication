#Summary:
# This R script is designed to replicate the work of Goyal and Welch(2008),
# "examining the performance of variables that have been suggested by the 
# academic literature to be good predictors of the equity premium".
# Therefore, our main objective is to perform an in-depth statistical analysis 
# on the annual S&P500 financial data-set, aiming to explore
# and evaluate significant financial indicators such as the Equity Premium, 
# Dividend Price Ratio, Earnings Price Ratio, and Dividend Yield ratio.

# The process begins by loading the data-set into a data-table for enhanced
# data handling efficiency.
# This is followed by the computation of several key financial metrics, 
# including the sum of the S&P 500 index and its dividend payments,
# and the use of logarithmic differences to determine ratios like the 
# Dividend Price Ratio, Earnings Price Ratio, and Dividend Yield Ratio.
# Furthermore, the script calculates dividend returns in a logarithmic 
# form and assesses the Equity Premium by comparing it to risk-free returns,
# thereby identifying the excess returns over a risk free asset.

# For visual representation, the `ggplot2` package is employed to plot these
# financial metrics over time, offering insights into their trends and
# patterns. The analysis then proceeds to a more granular level by converting
# the data-set into a time series format, which facilitates both In-Sample 
# and Out-of-Sample testing. 

# Afterwards, a function, `get_statistics` is crafted to conduct regression 
# analysis
# This function compares the performance of a Historical Mean Model against that
# of an Ordinary Least Squares (OLS) regression model by calculating
# metrics such as the Mean Squared Error (MSE), and an adjusted Out-of Sample
# R-squared value. These steps are crucial in assessing  the efficacy
# and predictive accuracy of each model.

# In the concluding phase, the script presents a summary of its findings 
# through well-organized tables. These tables highlight the efficiency 
# and predictive performance of each financial metric and model 
# analyzed in the study. By doing so, the script provides a comprehensive 
# evaluation of financial metrics, offering valuable insights into 
# financial econometrics. 
