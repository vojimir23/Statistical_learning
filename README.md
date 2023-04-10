# Statistical_learning

Dataset for Unsupervised part is downloaded from Kaggle at: https://www.kaggle.com/code/rishitjavia/movie-recommendation-using-netflix-movie-reviews/data


Dataset for Supervised part is downloaded from Kaggle at: https://www.kaggle.com/datasets/zzettrkalpakbal/full-filled-brain-stroke-dataset?datasetId=2343381&language=R

-Supervised part-

In this project decision trees are used to classify patients according to key health indicators if they have stroke or not. It was found that dataset is fairly unbalanced, and combination of undersampling and oversampling was used to overcome this problem. In this situation accuracy measure should be taken with caution. Also, it was found that decision trees can easily become very complex – to overfit.

According to World Health Organization and World Stroke Organization, stroke has already reached epidemic proportions and globally 1 in 4 adults over age of 25 will have a stroke in their lifetime and 12 million people globally will have their first stroke this year and 6 million will die as a result. These devastating results are shifting goals of many scientists to prevent it, also the programmers and data scientists who can help it to predict it and react on time. The goal of this project is to predict if patient would have a stroke or not according to 10 predictors: gender, age, if he/she has hypertension or hearth disease, marital status, residence and work type, BMI, smoking status and glucose level.


-Unsupervised part-

In this project, the Apriori algorithm is used to find association rules in Netflix's movies database. The goal is to use them for future recommendation purposes. Apriori managed to find around 65 thousand rules in the dataset of 16 million different ratings and threshold set at 20% of transactions.

Association Rules are used to discover relationships between features in a massive dataset by establishing rules that are based on how frequently the features occur together in the dataset. A famous example of association rules and data mining is Market basket analysis, as the name said it is usually used to analyze store transactions. By analyzing the transactions and finding frequent itemsets and rules, store managers can make new pricing and marketing decisions. Usually, there are trivial rules such as a well-known bread/milk combination that is often bought together, or some unexplainable rules that are not worth action. On the other side, unfortunately rarely, there are actionable rules like the famous example of beer/diapers. Association rules are not used only in retail, they could also be used in medicine, User experience (UX) design, and recommendation system in websites or platforms like Netflix etc. In this project, association rules will be used to find patterns in the Netflix movie database, with the goal to make appropriate recommendations to customer, based on choices made by previous customers.
