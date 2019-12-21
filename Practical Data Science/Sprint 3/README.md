# Sprint 3 - Using NLP to predicting the performance of the stock market

I use news headlines for a binary classification problem in this Kaggle challenge: predicting whether the stock market's performance rose (or stayed the same - both represented by a 1), or decreased (represented by a 0). 

To pre-process the text for the models, I use TF-IDF vectoriser and other NLP techniques in the notebook labelled NLP Stock Market Prediction - TF-IDF Vectoriser.ipynb. I wrote this Medium article - https://medium.com/@enahoroprecious/c192f7f44c62 - to explain my process in tackling this competition.

and Word2Vec in the second notebook.

Classification models used in both notebooks: Random Forest Classifier (‘RF’), XG Boost (‘XGB’), Logistic Regression (‘LogReg’), Support Vector Classifier (‘SVC’), and the Multi-Layer Perceptron classifier (‘MLP’) 

---------

Data gotten from: Sun, J. (2016). Daily News for Stock Market Prediction. Retrieved 25 November 2019, from https://www.kaggle.com/aaron7sun/stocknews#Combined_News_DJIA.csv

Kaggle Challenge: https://www.kaggle.com/aaron7sun/stocknews
