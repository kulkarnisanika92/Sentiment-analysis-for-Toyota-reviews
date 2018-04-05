# Sentiment-analysis-for-Toyota-reviews
3 Objectives
1. Download the training data (reviews) from the online link. Include
the star rating given by the reviewer. Display this as a table where each row
corresponds to a single review. The table should have three columns: year of the
review, star rating given by the reviewer, text of review.
2. Download the test data (reviews) from the online link. Include the star
rating given by the reviewer. Display this as a table where each row corresponds
to a single review. The table should have three columns: year of the review, star
rating given by the reviewer, text of review.
1
3. For each review in train and test set, remove all punctuation, convert
uppercase to lowercase. The results of the new 'normalized' review should be in
its own column (this will be the fourth column).
4. Tag each review according to the presence of the following words: 'ser-
vice', 'price', 'handling', 'interior'. The tag can be in the fifth column. It is possible
that you will have multiple tags per review.
5. There are several lexicons that help compute sentiment analysis (bing,
afinn, nrc etc.). Use any sentiment analysis text to compute the sentiment of each
review.
6. Give the following information:
(a) What is the average sentiment rating of the reviews in the training
set of the Toyota Camry? Compare to average star rating provided by user.
(b) What is the average sentiment rating of the reviews corresponding
to each of the 4 tags? Compare to average star rating provided by user and
also to average star rating from 6a
7. Build a model that can predict the star rating given the sentiment
analysis you compute. The model should be built using just the training data.
Use any type of model you choose
8. Use the model you built in 7 to predict the star rating of the test reviews.
What is the accuracy of your model. Remember this is not an exercise in optimizing
your model, so don't spend too much time on this part of the assignment.
9. For each tag in the training dataset compute the TF-IDF for every
word in the set. Remove stop words. For each tag set visualize the TF-IDF scores
for the top 10 words.
