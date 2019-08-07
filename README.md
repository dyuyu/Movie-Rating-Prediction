# Movie-Rating-Prediction

Description:
---
For our MATH 448 Data Mining project we decided to work with the Movie lense movie data. 
This data contained information about movie data collected from 1996 until 2018. 
The data consisted of movie ratings from users, movie genre, movie tags, and the year the movie was made. 
The goal of this project was to train a model to see how accurately we were able to predict the average movie rating of movies based off of their previous average rating from users of IMDB.com,the year the movie was made, and what genre the movie is. 
**The models we tried were different multiple regression models and decision trees models such as Linear Regression, Best Subset Selection, PCR, Lasso, Ridge, Bagging, RandomForest, and Boosting.** 
We found that Lasso gives the best accuracy for predicting the average movie rating based off of our predictors. 

**The test MSE’s are as follows:**
* PCR: test MSE = 0.7390
* Lasso: test MSE = 0.7362864
* Ridge: Test MSE = 0.7377501
* Bagging: Test MSE = 0.8083859 
* RandomForest: test MSE = 0.7778292
* Boosting: test MSE = 0.7376404


As for the best predictors throughout the whole data set, out of the 21 predictors used
the best subset were **“Year”, “Number.of.ratings”, “Action”, “Adventure”, “Children”,
“Comedy”, “Film.Noir”, and “Horror” in which “Year”, “Number.of.ratings”** showed to have
the highest significance from the Linear regression, Best Subset, and Boosting summaries.
Which in real life, it makes a lot of sense that these predictors will affect the response.
Moreover, the most important genres for the response are **“Comedy”, “Horror”,
“Action”, and “Adventure”.**


Here's a link of the data set --> 
https://grouplens.org/datasets/movielens/

Authors
-------------
* Diana Yu Yu
* Carina Kalaydjian
