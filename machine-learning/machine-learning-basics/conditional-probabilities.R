# Conditional Probabilities

# In machine learning applications, we rarely can predict outcomes 
# perfectly. For example, spam detectors often miss emails that are 
# clearly spam, Siri often misunderstands the words we are saying,
# and your bank at times thinks your card was stolen when it was not.
# The most common reason for not being able to build perfect algorithms 
# is that it is impossible. To see this, note that most datasets will 
# include groups of observations with the same exact observed values 
# for all predictors, but with different outcomes. Because our prediction
# rules are functions, equal inputs (the predictors) implies equal 
# outputs (the predictions). Therefore, for a challenge in which the same 
# predictors are associated with different outcomes across different 
# individual observations, it is impossible to predict correctly for
# all these cases. We saw a simple example of this in the previous section: 
# for any given height x, you will have both males and females that are x
# inches tall. However, none of this means that we can’t build useful
# algorithms that are much better than guessing, and in some cases
# better than expert opinions. To achieve this in an optimal way, we 
# make use of probabilistic representations of the problem based on 
# the ideas presented in Section 17.3. Observations with the same
# observed values for the predictors may not all be the same, but we
# can assume that they all have the same probability of this class 
# or that class. We will write this idea out mathematically for the 
# case of categorical data.

# We use the notation (X1=x1,…,Xp=xp) to represent the fact that we 
# have observed values x1,…,xp for covariates X1,…,Xp. This does not 
# imply that the outcome Y will take a specific value. Instead, 
# it implies a specific probability. In particular, we denote the 
# conditional probabilities for each class 
# k: Pr(Y=k∣X1=x1,…,Xp=xp),fork=1,…,K
# To avoid writing out all the predictors, we will use the bold letters 
# like this: X≡(X1,…,Xp) and x≡(x1,…,xp). We will also use the f
# ollowing notation for the conditional probability of being class k:
# pk(x)=Pr(Y=k∣X=x),fork=1,…,K Note: We will be using the p(x)
# notation to represent conditional probabilities as functions 
# of the predictors. Do not confuse it with the p that represents 
# the number of predictors. These probabilities guide the construction 
# of an algorithm that makes the best prediction: for any given x, we will 
# predict the class k with the largest probability among p1(x),p2(x),…pK(x). 
# In mathematical notation, we write it like this: ^Y=maxkpk(x).
# In machine learning, we refer to this as Bayes’ Rule. But keep in 
# mind that this is a theoretical rule since in practice we don’t know 
# pk(x),k=1,…,K. In fact, estimating these conditional probabilities 
# can be thought of as the main challenge of machine learning. The 
# better our probability estimates ^pk(x), the better our predictor:
# ^Y=maxk^pk(x) So what we will predict depends on two things: 1) how 
# close are the maxkpk(x) to 1 or 0 (perfect certainty) and 2) how
# close our estimates ^pk(x) are to pk(x). We can’t do anything about
# the first restriction as it is determined by the nature of the
# problem, so our energy goes into finding ways to best estimate
# conditional probabilities. The first restriction does imply that
# we have limits as to how well even the best possible algorithm
# can perform. You should get used to the idea that while in some 
# challenges we will be able to achieve almost perfect accuracy, with 
# digit readers for example, in others our success is restricted by
# the randomness of the process, with movie recommendations for example.

# Before we continue, it is important to remember that defining our 
# prediction by maximizing the probability is not always optimal in 
# practice and depends on the context. As discussed above, sensitivity 
# and specificity may differ in importance. But even in these cases, 
# having a good estimate of the pk(x),k=1,…,K
# will suffice for us to build optimal prediction models,
# since we can control the balance between specificity and 
# sensitivity however we wish. For instance, we can simply
# change the cutoffs used to predict one outcome or the other. In the 
# plane example, we may ground the plane anytime the probability of 
# malfunction is higher than 1 in a million as opposed to the default 1/2
# used when error types are equally undesired.
