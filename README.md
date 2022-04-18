# DataAnalysis
Bank churn rate regression and prediction  

The dataset we chose is based on information regarding credit card customers. 
The dataset came from the website Kaggle.com, which is traditionally known for having datasets that are conducive to Machine Learning oriented problems, but the one we chose works very well for this project as well. 
The original dataset, before we did any cleaning, contained over 10000 observations, each with 24 variables. 
Each observation (row) represents a customer and the variables consist of information about the customer.  

Our first hypothesis tests the mean of classes within the gender variable with respect to the age variable in our datset. 
The second hypothesis tests the significance of the coefficients in our logistic regression model. 
The logistic regression model that we made was successful in showing that all of our features that we chose after going through our process of feature selection, were significant features. 
Also, our code shows the process of checking the linearity assumptions of the continuous variables in the model as well as checking the multicollinearity of the features that we chose. 
Finally, we manipulated our data to have one of the few features that is used in our model to have data that is MCAR as well as a separate datset where that same feature is MNAR. 
