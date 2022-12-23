## LSE Data Analytics Online Career Accelerator 

# DA301:  Advanced Analytics for Organisational Impact

###############################################################################

# Assignment template

## Scenario
## You are a data analyst working for Turtle Games, a game manufacturer and 
## retailer. They manufacture and sell their own products, along with sourcing
## and selling products manufactured by other companies. Their product range 
## includes books, board games, video games and toys. They have a global 
## customer base and have a business objective of improving overall sales 
##performance by utilising customer trends. 

## In particular, Turtle Games wants to understand:
## - how customers accumulate loyalty points (Week 1)
## - how useful are remuneration and spending scores data (Week 2)
## - can social data (e.g. customer reviews) be used in marketing 
##     campaigns (Week 3)
## - what is the impact on sales per product (Week 4)
## - the reliability of the data (e.g. normal distribution, Skewness, Kurtosis)
##     (Week 5)
## - if there is any possible relationship(s) in sales between North America,
##     Europe, and global sales (Week 6).

################################################################################

# Week 4 assignment: EDA using R

## The sales department of Turtle games prefers R to Python. As you can perform
## data analysis in R, you will explore and prepare the data set for analysis by
## utilising basic statistics and plots. Note that you will use this data set 
## in future modules as well and it is, therefore, strongly encouraged to first
## clean the data as per provided guidelines and then save a copy of the clean 
## data for future use.

# Instructions
# 1. Load and explore the data.
##  - Remove redundant columns (Ranking, Year, Genre, Publisher) by creating 
##      a subset of the data frame.
##  - Create a summary of the new data frame.

# 2. Create plots to review and determine insights into data set.
##  - Create scatterplots, histograms and boxplots to gain insights into
##      the Sales data.

##  - Note your observations and diagrams that could be used to provide
##      insights to the business.
# 3. Include your insights and observations.

###############################################################################

# 1. Load and explore the data

# Install and import Tidyverse If not installed, 
# install.packages('tidyverse')

# Import tidyverse library if it is already installed.
library(tidyverse)

# Import the data set (turtle_sales.csv).
## Change the working directory if needed. 
## setwd(dir='c:/Users/Tamas/Python/Assignment3')

## If the file does not load. 
## Choose csv file from outside the current working directory.
## sales <- read.csv(file.choose(), header=TRUE)
sales <- read.csv('turtle_sales.csv', header=TRUE)

# View the data frame.
head(sales)

# Explore the data.
dim(sales)
str(sales)
View(sales)

# Look at categorical variables.
# Publisher.
unique(sales$Publisher)
table(sales$Publisher)

# Look at the number one publisher.
View(subset(sales, Publisher == 'Nintendo'))

# Look at the other publishers.
View(subset(sales, Publisher != 'Nintendo'))

# Other categorical variables.
unique(sales$Genre)
unique(sales$Ranking)

# Year variable
unique(sales$Year)
summary(sales$Year)

# The two errors in 'Year' could be sorted by removing the rows with NA value by:
## sales2 <- filter(sales, !is.na(Year)),
## but there is a better solution here.
filter(sales, is.na(Year))
filter(sales, Product == 7141)
filter(sales, Product == 948)

## It is reasonable to set the same year that is set for the other platforms.
sales$Year[which(is.na(sales$Year) & sales$Product == 7141)] <- 2003
sales$Year[which(is.na(sales$Year) & sales$Product == 948)] <- 2010

## Check the result.
summary(sales$Year)

# Look at the relaationship between ranking and Global Sales.
qplot(Ranking,
      Global_Sales,
      colour=Platform, 
      data=sales,
      geom=c('point', 'jitter'))+
  labs(title = "Global Sales by Ranking",
       subtitle = 'Turtle Games')+
  theme(plot.title = element_text(size = 30)) +
  theme_classic()

# Look for more detail in top 100 Global Sales of Turtle Games.
sales_subset <- filter(sales, Ranking < 100)
qplot(Ranking,
      Global_Sales,
      data=sales_subset,
      geom=c('point', 'jitter'))+ 
  labs(title = "Global Sales by Ranking",
       subtitle = 'Turtle Games Top 100')+
  theme(plot.title = element_text(size = 30)) +
  theme_classic()

# Check Nintendo games.
qplot(Ranking,
      Global_Sales,
      data=sales_subset,
      colour= Publisher == 'Nintendo',
      geom=c('point', 'jitter'))+
  labs(title = "Global Sales by Ranking",
       subtitle = 'Turtle Games Top 100')+
  theme(plot.title = element_text(size = 30)) +
  theme_classic()

# Look at game releases by year.
qplot(Year,
      Global_Sales,
      data=sales)+
  labs(title = "Global Sales by release year",
       subtitle = 'Turtle Games')+
  theme(plot.title = element_text(size = 30)) +
  theme_classic()

# Find duplicate rows.
duplicated(sales)
sum(duplicated(sales)) 

# Explore more details
# DataExplorer::create_report(sales)
                      
# Correlation between sales columns is outlinned in report.      

# Check for missing values
sales[is.na(sales)]

# Create a new data frame from a subset of the sales data frame.
df <- sales

# Create a cleaned version of the sales data.
save(sales, file = "sales_clean.r")

# Look at the new dataframe.
head(df)

# Remove unnecessary columns. 
df$Ranking <- df$Year <- df$Genre <- df$Publisher <- NULL

# View the data frame.
head(df)
class(df)
typeof(df)
dim(df)
str(df)
View(df)

# Look at the remaining categorical variables.
##Look at the Product column.
unique(df$Product)

# Product should be converted from numeric to string
str(df)
df$Product <- as.character(df$Product)

# Check the result.
is.character(df$Product)
str(df)

# Look into one product.
View(subset(df, Product == '9080'))

# Look at the Platform column.
unique(df$Platform)
table(df$Platform)

# Look at the numeric variables.
# Explore the sales figures.
print(sum(df$NA_Sales))
print(sum(df$EU_Sales))
print(sum(df$Global_Sales))

# Printing sales headline details.
df_sales_total <- NULL
df_sales_total <- c('SALES FIGURES & CALCULATIONS: \n',
                    'North America: ', 
                    sum(df$NA_Sales),
                    'Europe: ', 
                    sum(df$EU_Sales), 
                    'Global sales total: ',
                    sum(df$Global_Sales),
                    'North America sells more by : ',
                    sum(df$NA_Sales)-sum(df$EU_Sales),
                    'North America (pct of global): ',
                    sum(df$NA_Sales)/sum(df$Global_Sales)*100,
                    'Europe (pct of global): ',
                    sum(df$EU_Sales)/sum(df$Global_Sales)*100)
df_sales_total

# View the descriptive statistics.
summary(df)

# Checking if the minimum value (0) is correct, adding new column to store other sales 
# and checking that there is no negative value, to verify Global_Sales colunm.
df$Other_Sales = (df$Global_Sales - df$NA_Sales - df$EU_Sales)
summary(df$Other_Sales)

# Calculating the difference between NA and EU sales.
df$Sales_gap = (df$NA_Sales - df$EU_Sales)

################################################################################

# 2. Review plots to determine insights into the data set.
# Explore sales by different categories.
qplot(Global_Sales, Platform, data=sales)+
  labs(title = "Global Sales by platform",
       subtitle = 'Turtle Games')+
  theme(plot.title = element_text(size = 30)) +
  theme_classic()

# Checking the other categories, a barplot could be more useful.
qplot(Global_Sales, Genre, data=sales) +
  theme_classic()
qplot(Global_Sales, Publisher , data=sales) +
  theme_classic()

# Checking if sales is impacted by how long it has been released.
qplot(Year, Global_Sales,
      data=sales)+
  labs(title = "Global Sales by Year of First Release ",
       subtitle = 'Turtle Games')+
  theme(plot.title = element_text(size = 30)) +
  theme_classic()

# Checking distribution by product.
is.character(df$Product)
qplot(Product, Global_Sales, data=df) +
  theme_classic()

# Found one outlier.
View(subset(df, Product == '107'))

# Histogram to see distribution of numeric variables.
qplot(Global_Sales, data=sales)+
  labs(title = 'Distribution of Global Sales',
  subtitle = 'Turtle Games')+
  theme(plot.title = element_text(size = 30)) +
  theme_classic()

qplot(NA_Sales, data=sales)+
  labs(title = 'Distribution of NA_Sales',
       subtitle = 'Turtle Games')+
  theme(plot.title = element_text(size = 30)) +
  theme_classic()

qplot(EU_Sales, data=sales)+
  labs(title = 'Distribution of EU_Sales',
       subtitle = 'Turtle Games')+
  theme(plot.title = element_text(size = 30)) +
  theme_classic()

# Scatterplot to see relationship of numeric variables.
# North America and Europe:
qplot(NA_Sales,
      EU_Sales,
      data=df,
      geom=c('point', 'jitter'))+
  labs(title = 'North America vs Europe Sales',
       subtitle = 'Turtle Games')+
  theme(plot.title = element_text(size = 30)) +
  theme_classic()

# Confirm similar distribution for regions.
# North America and Global sales:
qplot(NA_Sales,
      Global_Sales,
      data=df,
      geom=c('point', 'jitter'))+
  labs(title = 'North America vs Global Sales',
       subtitle = 'Turtle Games')+
  theme(plot.title = element_text(size = 30)) +
  theme_classic()

# Europe and Global sales:
qplot(EU_Sales,
      Global_Sales,
      data=df,
      geom=c('point', 'jitter'))+
  labs(title = 'Europe vs Global Sales',
       subtitle = 'Turtle Games')+
  theme(plot.title = element_text(size = 30)) +
  theme_classic()


# EU sales to global sales by platform.
qplot(EU_Sales,
      Global_Sales,
      colour=Platform, 
      data=df,
      geom=c('point', 'jitter')) +
  theme_classic()

# NA sales to global sales by platform.
qplot(NA_Sales,
      Global_Sales,
      colour=Platform, 
      data=df,
      geom=c('point', 'jitter')) +
  theme_classic()

# NA sales to global sales by product is similar.
qplot(EU_Sales,
      NA_Sales,
      colour=Product, 
      data=df,
      geom=c('point', 'jitter')) +
  theme_classic()

# Create a barplot to look into products.
qplot(x=Product,
      y=Global_Sales,
      data=df)+
  labs(title = "Global Sales by Product",
       subtitle = 'Turtle Games')+
  theme(plot.title = element_text(size = 30)) +
  theme_classic()

# Adjust the style.
qplot(x=Product,
      y=Global_Sales,
      data=df,
      colour=Platform,
      geom='col')+
  labs(title = "Global Sales by Product",
       subtitle = 'Turtle Games')+
  theme(plot.title = element_text(size = 30)) +
  theme_classic()

# Try plots with one variable.
qplot(Product, data=df, geom='bar') +
  theme_classic()
qplot(y=Product, data=df, colour=Platform) +
  theme_classic()
qplot(y=Product, data=sales, x=Platform, colour=Platform) +
  theme_classic()
qplot(y=Product, data=sales, colour=Platform) +
  theme_classic()

# These were not very useful to analyse the data but helps to understand the structure.

# Look into Nintendo sales to understand these visualisations.
sales_nintendo <- subset(sales, Publisher == 'Nintendo')
sales_other_platforms <- subset(sales, Publisher != 'Nintendo')

# The visualisation can be checked together with the data in View.
View(sales_nintendo)
qplot(y=Product, data=sales_nintendo, colour=Platform) +
  theme_classic()

# The visualisation can be checked together with the data in View.
# View(sales_other_platforms)
qplot(y=Product, data=sales_other_platforms, colour=Platform) +
  theme_classic()

# From the data it is not clear how to separate Nintendo products and platforms, some 
# consultation with the stakeholders could be useful.

# Create boxplots.
qplot(Global_Sales, data=sales, geom='boxplot')+
  labs(title = "Global Sales",
       subtitle = 'Turtle Games ((£mio)')+
  theme(plot.title = element_text(size = 30)) +
  theme_classic()

# Regions show similar distribution.
qplot(NA_Sales, data=sales, geom='boxplot')+
  labs(title = "North America Sales",
       subtitle = 'Turtle Games ((£mio)')+
  theme(plot.title = element_text(size = 30)) +
  theme_classic()

qplot(EU_Sales, data=sales, geom='boxplot')+
  labs(title = "Europe Sales",
       subtitle = 'Turtle Games ((£mio)')+
  theme(plot.title = element_text(size = 30)) +
  theme_classic()

# Explore the categories using barcharts.
# Total sales by platform, barchart.
qplot(Platform, Global_Sales, data=sales, geom='col')+
  labs(title = "Global Sales by Platform",
       subtitle = 'Turtle Games')+
  coord_flip()+
  theme(plot.title = element_text(size = 30)) +
  theme_classic()


# Total Sales by genre, barchart.
qplot(Genre, Global_Sales, data=sales, geom='col')+
  labs(title = "Global Sales by Genre",
       subtitle = 'Turtle Games')+
  coord_flip()+
  theme(plot.title = element_text(size = 30)) +
  theme_classic()
                         
# Total Sales by publisher, barchart.                   
## Three publishers represent over 50 per cent of sales
qplot(Publisher, Global_Sales, data=sales, geom='col')+
  labs(title = "Global Sales by Publisher ",
       subtitle = 'Turtle Games')+
  coord_flip()+
  theme(plot.title = element_text(size = 30)) +
  theme_classic()

# Try stacked bar qplot
qplot(Product, Global_Sales, fill=Platform, data=df, geom='col') +
  theme_classic()

# Visualise the difference between EU and NA sales.
qplot(Sales_gap, data=df, colour=Platform, geom='boxplot')+
  labs(title = "Difference of NA and EU Sales by Platform ",
       subtitle = 'Turtle Games')+
  theme(plot.title = element_text(size = 30))

## Further improvement should be done:
# Maybe less categories would be useful, like PC, Sony, Microsoft, Nintendo.
# Try to improve the original plot for the stakeholders.
# Checking if sales is impacted by how long it has been released.
qplot(Year, Global_Sales,
      data=sales)+
  labs(title = "Global Sales by Year of First Release ",
       subtitle = 'Turtle Games')+
  theme(plot.title = element_text(size = 30))
###############################################################################

# 3. Observations and insights

# Product numbers are not separately assigned for each product by platform. One product
# number covers the same product for all platforms. Perhaps it worth to look into
# this later, which platform works for products.

# Raw data loaded from CSV file. 352 observations in 9 columns.

# Fairly clean data, additionally corrected 2 rows where year value was NA.

# I identified unusual pattern between the Global Sales and the Ranking column.
# It needs to be checked why global ranking looks to be the same as
# Turtle Games sales ranking. 

# Generally there must be potential in exploring on a more granular level
# and some clarification before that with stakeholders can be useful.

# Trends can be identified by comparing recent releases with earlier games.
# Sales figures could include accumulation of yearly sales so it needs to be clarified,
# otherwise comparison with new products does not make sense. The pattern shows that from 
# 2010 figures get slightly lower. Why is the last release date 2016? 

# From the business perspective important details are missing like profitability and sales price.
# This could impact the direction of further analysis. Otherwise it is just comparing products
# that have higher sales numbers to those which have lower, trying to find patterns.

# The difference between the NA and EU sales varies by product and by sales platform. This
# can be looked at how to improve EU sales, NA sales or Other sales. There are platforms
# where the difference between EU and NA is smaller.

# I included a simple metric, difference between NA and EU sales (Sales_gap) and added
# a new calculated columen (Other_Sales).

# Data explorer shows high correlation between sales regions. Perhaps worth to visit 
# the data explorer function later for a deeper analysis.




###############################################################################
###############################################################################


# Week 5 assignment: Cleaning and manipulating data using R

## Utilising R, you will explore, prepare and explain the normality of the data
## set based on plots, Skewness, Kurtosis, and a Shapiro-Wilk test. Note that
## you will use this data set in future modules as well and it is, therefore, 
## strongly encouraged to first clean the data as per provided guidelines and 
## then save a copy of the clean data for future use.

## Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 4 assignment. 
##  - View the data frame to sense-check the data set.
##  - Determine the `min`, `max` and `mean` values of all the sales data.
##  - Create a summary of the data frame.
# 2. Determine the impact on sales per product_id.
##  - Use the group_by and aggregate functions to sum the values grouped by
##      product.
##  - Create a summary of the new data frame.
# 3. Create plots to review and determine insights into the data set.
##  - Create scatterplots, histograms, and boxplots to gain insights into 
##     the Sales data.
##  - Note your observations and diagrams that could be used to provide 
##     insights to the business.
# 4. Determine the normality of the data set.
##  - Create and explore Q-Q plots for all sales data.
##  - Perform a Shapiro-Wilk test on all the sales data.
##  - Determine the Skewness and Kurtosis of all the sales data.
##  - Determine if there is any correlation between the sales data columns.
# 5. Create plots to gain insights into the sales data.
##  - Compare all the sales data (columns) for any correlation(s).
##  - Add a trend line to the plots for ease of interpretation.
# 6. Include your insights and observations.

################################################################################

# 1. Load and explore the data

# View data frame created in Week 4.
# load('sales.r')
head(sales)

# Change product to category.
sales$Product <- as.character(sales$Product)
df2 <- sales

# Check output: Determine the min, max, and mean values.
head(df2)

min(sales$NA_Sales)
max(sales$NA_Sales)
mean(sales$NA_Sales)

min(sales$EU_Sales)
max(sales$EU_Sales)
mean(sales$EU_Sales)

min(sales$Global_Sales)
max(sales$Global_Sales)
mean(sales$Global_Sales)

# View the descriptive statistics.
summary(df2)

###############################################################################

# 2. Determine the impact on sales per product_id.

## 2a) Use the group_by and aggregate functions.
# Group data based on Product and determine the sum per Product.
df3 <- aggregate(cbind(NA_Sales, EU_Sales, Global_Sales)
                                ~Product, df2, sum)
# View the data frame.
head(df3)
dim(df3)

# Group data based on year.
df3_year <- aggregate(cbind(NA_Sales, EU_Sales, Global_Sales)
                 ~Year, df2, sum)
# View the data frame.
head(df3_year)
dim(df3_year)

# Group data based on platform.
df3_platform <- aggregate(cbind(NA_Sales, EU_Sales, Global_Sales)
                      ~Platform, df2, sum)
# View the data frame.
head(df3_platform)
dim(df3_platform)

# Group data based on publisher.
df3_publisher <- aggregate(cbind(NA_Sales, EU_Sales, Global_Sales)
                      ~Publisher, df2, sum)
# View the data frame.

df3_publisher$pct_total <- round(df3_publisher$Global_Sales/sum(df3_publisher$Global_Sales)*100)
dim(df3_publisher)
df3_publisher

# Group data based on genre.
df3_genre <- aggregate(cbind(NA_Sales, EU_Sales, Global_Sales)
                           ~Genre, df2, sum)
# View the data frame.
head(df3_genre)
dim(df3_genre)

# Continue with the product grouping:
# Change the order of columns for clarity.
df3$Other_Sales <- df3$Global_Sales-df3$NA_Sales-df3$EU_Sales
col_order <- c('Product','Global_Sales','NA_Sales','EU_Sales','Other_Sales')
df3 <- df3[, col_order]

# Explore the data frame.
summary(df3)

## 2b) Determine which plot is the best to compare game sales.
# Create scatterplots.
pairs(~Global_Sales+NA_Sales+EU_Sales+Other_Sales, data = df3)+
  theme_classic()

# Refine the visualisation.
# install.packages("GGally")
library(GGally)
pairs_df3 <- df3
pairs_df3$Product <- NULL
ggpairs(pairs_df3)+
  labs(title= 'Distribution by region',
       subtitle='Turtle Games',
       x="",
       y="Global_Sales")

# computing correlation matrix
cor_df3 <- df3
cor_df3$Product <- NULL
head(cor_df3)
cor_df3 = cor(cor_df3)

# print("Correlation matrix")
print(cor_df3)

# Create histograms of each sales column.
qplot(data=df3, x=Global_Sales)
qplot(data=df3, x=NA_Sales)
qplot(data=df3, x=EU_Sales)
qplot(data=df3, x=Other_Sales)

# Stack the dataframe to change visualisation options.
df3_long <- cbind(df3[1:1], stack(df3[2:5]))
colnames(df3_long) <- c('Product', 'Sales_mio' ,'Region')
head(df3_long)

# Show distribution of regions together.
qplot(data=df3_long, x=Sales_mio) + facet_wrap(~Region) +
  theme_classic()+
  labs(title= 'Distribution by region',
       subtitle='Turtle Games', x="",
       y="Global_Sales")

# Create boxplots of regions together.
qplot(x=df3_long$Region, y=df3_long$Sales_mio, geom="boxplot") +
  theme_classic()+
labs(title= 'Distribution by region',
     subtitle='Turtle Games', x="region",
     y="Global_Sales")

# Some additional visuals were already created earlier in Week 4 to help 
# understand impact on sales.

###############################################################################

# 3. Determine the normality of the data set.
# Calculate Other_Sales in df3.
df3$Other_Sales = (df3$Global_Sales - df3$NA_Sales - df3$EU_Sales)
summary(df3$Other_Sales)

## 3a) Create Q-Q Plots to compare data to normal distribution line.
# Global sales Q-Q plot.
qqnorm(df3$Global_Sales, main="Global Sales QQ Plot")
# Add line to show normal distribution
qqline(df3$Global_Sales, col='red')

# Repeat Q-Q plot with the other columns.
# North America Q-Q plot.
qqnorm(df3$NA_Sales, main="NA Sales QQ Plot")
# Add line to show normal distribution
qqline(df3$NA_Sales, col='red')
# Europe Q-Q plot.
qqnorm(df3$EU_Sales, main="EU Sales QQ Plot")
# Add line to show normal distribution
qqline(df3$EU_Sales, col='red')
# Other Q-Q plot.
qqnorm(df3$Other_Sales, main="Other Sales QQ Plot")
# Add line to show normal distribution
qqline(df3$Other_Sales, col='red')

## 3b) Perform Shapiro-Wilk test
shapiro.test((df3$Global_Sales))
# Repeat with the other columns.
shapiro.test((df3$NA_Sales))
shapiro.test((df3$EU_Sales))
shapiro.test((df3$Other_Sales))


## 3c) Determine Skewness and Kurtosis

# Install the moments package and load the library.
# install.packages('moments') 
library(moments)

# Skewness and Kurtosis.
skewness(df3$Global_Sales)
kurtosis(df3$Global_Sales)

# Look at distribution on histogram.
hist(df3$Global_Sales)
hist(df3$Global_Sales, breaks=100)
# Check the distribution on boxplot.
boxplot(df3$Global_Sales)
summary(df3$Global_Sales)

skewness(df3$NA_Sales)
kurtosis(df3$NA_Sales)

skewness(df3$EU_Sales)
kurtosis(df3$EU_Sales)

skewness(df3$Other_Sales)
kurtosis(df3$Other_Sales)


## 3d) Determine correlation
# Determine correlation.
print("Correlation matrix")
print(cor_df3)

###############################################################################

# 4. Plot the data
# Create plots to gain insights into data.
# Choose the type of plot you think best suits the data set and what you want 
# to investigate. Explain your answer in your report.

## Scatterplot with more variables for comparison.
ggplot(data=df3, aes(x=NA_Sales, y=Global_Sales)) +
  geom_point(colour='red') +
  geom_point(data=df3, aes(x=EU_Sales, y=Global_Sales)) +
  theme_classic() +
  labs(title= 'NA Sales and EU Sales',
     subtitle='Turtle Games', x="NA and EU Sales",
     y="Global_Sales")

# Relationship between NA and Global Sales
ggplot(data=df3, aes(x=NA_Sales, y=Global_Sales)) +
  geom_point() +
  theme_classic() +
  labs(title= 'NA Sales and EU Sales and Global sales',
       subtitle='Turtle Games',
       x="NA_Sales",
       y="Global_Sales")

# Visualise the difference between EU and NA sales.
head(df3_platform)
df3_new <- df3_platform
df3_new$Sales_gap = (df3_new$NA_Sales - df3_new$EU_Sales)
df3_new <- arrange(df3_new, Sales_gap)
head(df3_new)
# Create the new plot.
ggplot(data=df3_new) + aes(x=reorder(Platform, Sales_gap), y=Sales_gap)+
  geom_bar(stat='identity', fill='steelblue')+
  labs(title = 'Difference of NA and EU Sales by Platform ',
       subtitle = 'Turtle Games')+
  theme(plot.title = element_text(size = 30))+
  coord_flip() +
  theme_classic()

# Create the new plot global sales by platform.
ggplot(data=df3_new) + aes(x=reorder(Platform, Global_Sales), y=Global_Sales)+
  geom_bar(stat='identity', fill='steelblue')+
  labs(title = 'Global Sales by Platform ',
       subtitle = 'Turtle Games')+
  theme(plot.title = element_text(size = 30))+
  coord_flip() +
  theme_classic()

# Create the new plot global sales by genre.
ggplot(data=df3_genre) + aes(x=reorder(Genre, Global_Sales), y=Global_Sales)+
  geom_bar(stat='identity', fill='steelblue')+
  labs(title = 'Global Sales by Genre ',
       subtitle = 'Turtle Games')+
  theme(plot.title = element_text(size = 30))+
  coord_flip() +
  theme_classic()

# Show total sales by publisher
ggplot(data=df3_publisher, aes(x=reorder(Publisher, Global_Sales), y=Global_Sales))+
  geom_bar(stat='identity', fill='steelblue')+
  labs(title = 'Global Sales by Publisher',
       subtitle = 'Turtle Games')+
  coord_flip()+
  theme_classic()

# Show total sales by year.
ggplot(df3_year, aes(x=Year, y=Global_Sales))+
  geom_bar(stat='identity', fill='darkblue')+
  labs(title = 'Global Sales by release year',
       subtitle = 'Turtle Games')+
  theme_classic()


# Group data based on platform for mean of Global_Sales instead of sum.
df3_platform_mean <- aggregate(cbind(NA_Sales, EU_Sales, Global_Sales)
                          ~Platform, df2, mean)
# View the data frame.
head(df3_platform_mean)
dim(df3_platform_mean)

# Add new column.
df3_mean <- df3_platform_mean
df3_mean$Sales_gap = (df3_mean$NA_Sales - df3_mean$EU_Sales)
df3_mean

# Create the new plot.
ggplot(data=df3_mean) + aes(x=reorder(Platform, Sales_gap), y=Sales_gap)+
  geom_bar(stat='identity', fill='steelblue')+
  labs(title = 'Mean of the Difference of NA and EU Sales by Platform ',
       subtitle = 'Turtle Games')+
  theme(plot.title = element_text(size = 30))+
  coord_flip() +
  theme_classic()

# Improve this boxplot of the distribution of difference between EU and NA sales.
qplot(Sales_gap, data=df, colour=Platform, geom='boxplot')+
  labs(title = "Difference of NA and EU Sales by Platform ",
       subtitle = 'Turtle Games')+
  theme(plot.title = element_text(size = 30))+
  theme_classic()

# Create the new version of the same plot.
ggplot(data=df) + aes(x=reorder(Platform, Sales_gap), y=Sales_gap)+
  geom_boxplot(notch=FALSE, fill='steelblue')+
  labs(title = 'Distribution of Difference - NA and EU Sales by Platform ',
       subtitle = 'Turtle Games')+
  theme(plot.title = element_text(size = 30))+
  coord_flip() +
  theme_classic()


#############################################################
# Plot combination of relationships.
head(df3)

# Scatterplot without regression line.
ggplot(data=df3, aes(x=NA_Sales, y=Global_Sales)) +
  geom_point() +
  theme_classic() +
  labs(title= 'NA Sales and Global sales',
       subtitle='Turtle Games',
       x="NA_Sales",
       y="Global_Sales")

# With regression line.
ggplot(data=df3, aes(x=NA_Sales, y=Global_Sales)) +
  geom_point() +
  geom_smooth(method=lm, se = FALSE) +
  theme_classic() +
  labs(title= 'NA Sales and Global sales',
       subtitle='Turtle Games',
       x="NA_Sales",
       y="Global_Sales")

ggplot(data=df3, aes(x=NA_Sales, y=Global_Sales)) +
  geom_point() +
  geom_smooth() +
  theme_classic() +
  labs(title= 'NA Sales and Global sales',
       subtitle='Turtle Games',
       x="NA_Sales",
       y="Global_Sales")

ggplot(data=df3, aes(x=EU_Sales, y=Global_Sales)) +
  geom_point() +
  theme_classic() +
  labs(title= 'EU Sales and Global sales',
       subtitle='Turtle Games',
       x="EU_Sales",
       y="Global_Sales")

ggplot(data=df3, aes(x=EU_Sales, y=NA_Sales)) +
  geom_point() +
  theme_classic() +
  labs(title= 'EU Sales and NA sales',
       subtitle='Turtle Games',
       x="EU_Sales",
       y="NA_Sales")
  
ggplot(data=df3, aes(x=EU_Sales, y=Other_Sales)) +
  geom_point() +
  theme_classic() +
  labs(title= 'EU Sales and Other sales',
       subtitle='Turtle Games',
       x="EU_Sales",
       y="Other_Sales")
  
###############################################################################

# 5. Observations and insights

# Product column had to be changed to character from numerical as it is in fact a
# categorical variable.
# Visualisations highlight the relationship between sales in different regions and the
# different scale in the regions. This must be presented to stakeholders with good quality
# visualisations. The correlation matrix shows highest value 0.916 between NA and Global sales.

# Analysis of normality for global sales:
# The data points on the QQ plots look to be far to the normal distribution line 
# The hypothesis test confirms that there is no normal distribution, the p-value is very low..
# The boxplot and the QQ plot both show the impact of outliers.
# The strong positive skew seen on the histogram is evidenced by the values measured. 
# The excess kurtosis is very high indicating heavy tails and large outliers.
# This pattern is repeated with the different regions

# Beyond the initial findings, further analysis can explore more of the 
# hypothesis, pattern and anomaly.
# Would it add information to merge of two datasets?
# Is there a trend in sales based on genre, year or other variable?
# Is there correlation between age and remuneration?
# How sales and loyalty points are impacted by customer satisfaction based on 
# sentiment analysis.
# Does the number or type of platforms available for the product impact sales?
# Why the game 107 is such an outlier product, the most successful with 67.85 mio £ global sales?
# EU outperforms NA for some games?
# Age only starts from 17?
# Year is only until 2016?

# Overall, some information like pricing and profit margins are not there so 
# it would make sense to consult stakeholders for direction.

# Most importantly, based on the grouping of products and platforms the business seems to be
# heavily relying on sales of Nintendo products, approximately 48 percent of the Global_Sales and
# representing 17 products in the top 20 ranking. It must be checked how the sales of these
# products is achieved and ensured by Turtle Games. Loosing this business due to external factors 
# would have an extreme impact on the company too.


###############################################################################
###############################################################################

# Week 6 assignment: Making recommendations to the business using R

## The sales department wants to better understand if there is any relationship
## between North America, Europe, and global sales. Therefore, you need to
## investigate any possible relationship(s) in the sales data by creating a 
## simple and multiple linear regression model. Based on the models and your
## previous analysis (Weeks 1-5), you will then provide recommendations to 
## Turtle Games based on:
##   - Do you have confidence in the models based on goodness of fit and
##        accuracy of predictions?
##   - What would your suggestions and recommendations be to the business?
##   - If needed, how would you improve the model(s)?
##   - Explain your answers.

# Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 5 assignment. 
# 2. Create a simple linear regression model.
##  - Determine the correlation between the sales columns.
##  - View the output.
##  - Create plots to view the linear regression.
# 3. Create a multiple linear regression model
##  - Select only the numeric columns.
##  - Determine the correlation between the sales columns.
##  - View the output.
# 4. Predict global sales based on provided values. Compare your prediction to
#      the observed value(s).
##  - NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.
##  - NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
##  - NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.
##  - NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
##  - NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52.
# 5. Include your insights and observations.

###############################################################################

# 1. Load and explore the data
# View data frame created in Week 5.
head(df3)

# Determine a summary of the data frame.
summary(df3)


###############################################################################

# 2. Create a simple linear regression model
## 2a) Determine the correlation between columns
# Model1 Global vs NA sales.
model1 <- lm(Global_Sales~NA_Sales, data=df3)

# View the model.
model1

# View the full regression table.
summary(model1)



# Model2 Global vs EU sales.
model2 <- lm(Global_Sales~EU_Sales, data=df3)

# View the model.
model2

# View the full regression table.
summary(model2)


# Model3 NA vs EU sales.
model3 <- lm(NA_Sales~EU_Sales, data=df3)

# View the model.
model3

# View the full regression table.
summary(model3)


## 2b) Create a plot (simple linear regression)
# View residuals for each model on a plot.
plot(model1$residuals)
plot(model2$residuals)
plot(model3$residuals)

# Basic visualisation of relationship using base R graphics
# Model 1:
plot(df3$NA_Sales, df3$Global_Sales)
coefficients(model1)

# Add line-of-best-fit.
abline(coefficients(model1))

# Model 2:
plot(df3$EU_Sales, df3$Global_Sales)
coefficients(model2)

# Add line-of-best-fit.
abline(coefficients(model2))

# Model 3:
plot(df3$NA_Sales, df3$EU_Sales)
coefficients(model3)

# Add line-of-best-fit.
abline(coefficients(model3))

###############################################################################

# 3. Create a multiple linear regression model
# Select only numeric columns from the  data frame.
model4 = lm(Global_Sales~NA_Sales + EU_Sales, data=df3)

# Multiple linear regression model.
summary(model4)

###############################################################################

# 4. Predictions based on given values
# Compare with observed values for a number of records.

forecast_global <- data.frame(NA_Sales=c(3.93, 2.73, 2.26, 22.08),
                              EU_Sales=c(1.56, 0.65, 0.97, 0.52))
forecast_global
predict(model4, newdata = forecast_global)

# Predictions from model4.
# 7.356754  4.908353  4.761039 26.625558 

# Actual Global sales values corresponding to these predictors (NA_Sales, EU_Sales) in
# the df2 dataframe from week4: 6.04, 4.32, 3.53, 23.21

# The problem seems to be that the model used the aggregated dataframe, df3 which has no respective 
# predictor values for two of the four records: 
subset(df3, NA_Sales == 3.93 & EU_Sales == 1.56)
subset(df3, NA_Sales == 2.73 & EU_Sales == 0.65)  
subset(df3, NA_Sales == 2.26 & EU_Sales == 0.97) 
subset(df3, NA_Sales == 22.08 & EU_Sales == 0.52)

# However, all records can be found in the ungrouped dataframe:
subset(df2, NA_Sales == 3.93 & EU_Sales == 1.56) # actual 6.04 vs predict 7.35 product 3267 / 99
subset(df2, NA_Sales == 2.73 & EU_Sales == 0.65) # actual 4.32 vs predict 4.90 product 6815 
subset(df2, NA_Sales == 2.26 & EU_Sales == 0.97) # actual 3.53 vs predict 4.76 product 2877 / 211
subset(df2, NA_Sales == 22.08 & EU_Sales == 0.52) # actual 23.21 vs predict 26.62 product 326

# Checking the pruducts
subset(df2, Product == 2877)
subset(df3, Product == 2877)

# Subset from df2 showing these products
matchingList <- c(3267, 6815, 2877, 326)
df2_actual <- df2[df2$Product %in% matchingList==TRUE,]
df2_actual

# Subset from df3 showing these products
df3_actual <- df3[df3$Product %in% matchingList==TRUE,]
df3_actual

# Comparison can be made to given values by changing them:
# 4. Predictions based on given values
# Compare with observed values for a number of records.
forecast_global2 <- data.frame(NA_Sales=c(7.63, 2.73, 8.56, 22.08),
                              EU_Sales=c(3.89, 0.65, 6.71, 0.52))
forecast_global2
predict(model4, newdata = forecast_global2)
# Modified predictions 14.335029,  4.908353, 18.770062, 26.625558
# Actual values of Global_Sales for the given values: 13.02, 4.32, 17.96, 23.21


###############################################################################

# 5. Observations and insights.

# NA_Sales is a highly significant predictor of Global_Sales, it explains 83.95% of the variability,
# with a very low p-value. Residuals show no pattern which can be seen on the scatterplot from the
# distance from the regression line. 
# Multiple linear regression improves predictions and the combined accuracy increases to 0.967. 
# However there is the problem of the normality of the variables. The scatterplots indicate that it 
# should be possible to adjust the model by removing some outliers.

###############################################################################
###############################################################################




