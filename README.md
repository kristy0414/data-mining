# Data Mining and Machine Learning Method
Used classification tree, random forest, boosting etc.

Use classification tree to see the decision rule.
Use randome forest or boosting to lower the ensemble variance and to get a better prediction, it is used in the case that has low bais but high variance.

Prune the decision tree: use 1+SE rule to avoid overfit.
Data mining could be used in both classification and regression.

Data mining has two branches: supervised and unpervised. One has a label for obersevation, the other don't. One is to do prediction, the other is to find association among oberservations.

Main superivised methods includes: classcification tree, regression tree...
Main unsuperivsed methods includes: clustering analysis, association rule, kmeans(KNN)...
Main ensemble supervised methods includes: bagging(an extesnsion of bootstrapping), boosting, randomforest...

General Comments: Ensemble models are ideal for low bias and high variance datasets. Because the ensemble results lower the variance and                     lead to enhance model performance. Usually, the most powerful model is random forest.
                  Decision tree is unstable and easily overfit, but it is easily to interpret. Meanwhile, the ensemble models are not able                   to interpret by a series of visible seperating rules. Ensemble models use perturb and combine methods to generate                           multiple models and then average the results.
                  

There are several projects that has been analyzed by data mining method through various topics to show you how it works. 

1. <women contraceptive methods>, final model is random forest.

2. <spam email filter>, use classfication tree and stratified sampling.
  
3. <the realtion between levels of income and citizens charateristics>, use classification tree.


