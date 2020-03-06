# Predict-Future-Sales

This is a project posted on Kaggle - https://www.kaggle.com/c/competitive-data-science-predict-future-sales

Data Preparation, Exploratory Analysis:
a.	The missing data were represented by -1  
b.	Which we removed
c.	Plot of missing value and the rest of data.
d.	As you can see missing makes only a small percentage of the chart hence we removed it.
  
Outlier Detection
a.	We recognized outlier for the dataset as anything above 200 item count since any value above that had lesser than 4 data points neighboring it. 
b.	We tried using K means to eliminate outliers more minutely, but it took 4 hours to comb through the entire dataset. Hence, we didn’t go ahead with that idea.
c.	Graphs are given below for before eliminating outliers and after respectively.

Feature selection / Engineering
a.	We combine sales.csv and items.csv to produce a data frame with complete records
b.	We then separated and got the month from the date for aggregating later
c.	Then aggregated the datapoints by month, shop id, item id, cost and category
d.	We feature engineered the average item count according to shop id, item id, cost and category.
e.	We also normalized the above columns and created an ordered column of the above averages.

Modeling
a.	For modeling we considered KNN, Kmeans, Linear Regression, random forest and Naïve Bayes.
b.	We chose KNN and Random forest because the had the least RMSE values.
c.	Random Forest:
i.	For random forest we fed the normalized averages mentioned above as experimentally, we found that to yield better results.
ii.	Additionally, we only took months of July, August and September of 2015 as training set because we found these give better results and exponentially lesser training time compared to other options.
iii.	We separated the count into 100s 10s and 1s and trained the classifier to classify in each of these results. Hence there were approximately 10 possible values.
iv.	We add these classes after the results
d.	KNN:
i.	For KNN we fed the ordinal values mentioned above as experimentally, we found that to yield better results.
ii.	Additionally, we only took months of 12 months prior to October, 2015 as training set because we found these give better results and exponentially lesser training time compared to other options.
iii.	We separated the count into 100s 10s and 1s and trained the classifier to classify in each of these results. Hence there were approximately 10 possible values.
iv.	We add these classes after the results

Validation
a.	For validation we fed October 2015 as testing data and compared the count generated by the models with the existing count grouped by each item id and shop id.
b.	We calculated MSE values as 21.42014 for random forest and 14.24083 for KNN
c.	We calculated RMSE values as 4.62819 for random forest and 3.773702 for KNN
d.	The graphs seem different visually, but we checked the calculation multiple times and those were the RMSE and MSE we got.
e.	Random Forest Graphs:
  
Confidence interval
a.	We calculated confidence value of 24.5 compared to 1.984 for null hypothesis for 95 percent, 1.646, 1.282 for 90 and 80 percent respectively.
b.	By R t.test() function, p value is 2.2e-16.
c.	Hence, we reject null hypothesis.
d.	Model 1 and 2 are significantly different. 
7.	Comparing the models
a.	Time taken as 169.67 seconds for random forest and 158.38 for KNN
b.	Comparatively, since null hypothesis was rejected and KNN yields lower RMSE with a lower time for training, so KNN is a better model.










	
