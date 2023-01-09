# Sensitivity and specificity

# To define sensitivity and specificity, we need a binary outcome.
# When the outcomes are categorical, we can define these terms for 
# a specific category. In the digits example, we can ask for the 
# specificity in the case of correctly predicting 2 as opposed to 
# some other digit. Once we specify a category of interest, then 
# we can talk about positive outcomes, Y=1, and negative outcomes, Y=0.

# In general, sensitivity is defined as the ability of an algorithm 
# to predict a positive outcome when the actual outcome
# is positive: ^Y=1 when Y=1. Because an algorithm that calls 
# everything positive (^Y=1 no matter what) has perfect sensitivity, 
# this metric on its own is not enough to judge an algorithm. For this
# reason, we also examine specificity, which is generally defined 
# as the ability of an algorithm to not predict a positive ^Y=0
# when the actual outcome is not a positive Y=0. We can summarize
# in the following way:

# High sensitivity: Y=1⟹^Y=1
# High specificity: Y=0⟹^Y=0

# Although the above is often considered the definition of specificity,
# another way to think of specificity is by the proportion of
# positive calls that are actually positive:
# High specificity: ^Y=1⟹Y=1.

# To provide precise definitions, we name the four entries of the
# confusion matrix:

#                     Actually Positive 	  Actually Negative
# Predicted positive 	True positives (TP) 	False positives (FP)
# Predicted negative 	False negatives (FN) 	True negatives (TN) 

# Sensitivity is typically quantified by TP/(TP+FN), the proportion of
# actual positives (the first column = TP+FN) that are called positives
# (TP). This quantity is referred to as the true positive
# rate (TPR) or recall.

# Specificity is defined as TN/(TN+FP) or the proportion of negatives 
# (the second column = FP+TN) that are called negatives (TN). This 
# quantity is also called the true negative rate (TNR). There is
# another way of quantifying specificity which is TP/(TP+FP) or 
# the proportion of outcomes called positives (the first row or TP+FP) 
# that are actually positives (TP). This quantity is referred to as 
# positive predictive value (PPV) and also as precision. Note that, 
# unlike TPR and TNR, precision depends on prevalence since higher
# prevalence implies you can get higher precision even when guessing.

# The multiple names can be confusing, so we include a table to help us 
# remember the terms. The table includes a column that shows the 
# definition if we think of the proportions as probabilities.

# Measure of 	Name 1 	Name 2 	Definition 	Probability representation
# sensitivity 	TPR 	Recall 	TPTP+FN     Pr(^Y=1∣Y=1)
# specificity 	TNR 	1-FPR 	TNTN+FP     Pr(^Y=0∣Y=0)
# specificity 	PPV 	Precision 	TPTP+FP Pr(Y=1∣^Y=1)

# Here TPR is True Positive Rate, FPR is False Positive Rate, and PPV 
# is Positive Predictive Value. The caret function confusionMatrix 
# computes all these metrics for us once we define what category 
# “positive” is. The function expects factors as input, and the first
#level is considered the positive outcome or Y=1. In our example, 
# Female is the first level because it comes before Male 
# alphabetically. If you type this into R you will see several 
# metrics including accuracy, sensitivity, specificity, and PPV.

confusion_matrix = confusionMatrix(y_hat, reference = test_set$sex)

# You can acceess these directly, for example, like this:
confusion_matrix$overall["Accuracy"]
confusion_matrix$byClass[c("Sensitivity", "Specificity", "Prevalence")]

# We can see that the high overall accuracy is possible despite 
# relatively low sensitivity. As we hinted at above, the reason 
# this happens is because of the low prevalence (0.23): the proportion 
# of females is low. Because prevalence is low, failing to predict 
# actual females as females (low sensitivity) does not lower the
# accuracy as much as failing to predict actual males as males 
# (low specificity). This is an example of why it is important 
# to examine sensitivity and specificity and not just accuracy. 
# Before applying this algorithm to general datasets, we need to 
# ask ourselves if prevalence will be the same.
