# First, we need to extract data form task

data <- as.data.frame(tsk('pima')$data(cols = tsk('pima')$feature_names))
target <- as.data.frame(tsk('pima')$data(cols = tsk('pima')$target_names))

# Because before using learners we need to impute missing data 
# we need to create split manually

ind <- sample(1:nrow(data),floor(nrow(data)/2))

# We need to manuly impute data in 4 data frames

data1_mice <- complete(mice(data[ind,]))
data2_mice <- complete(mice(data[-ind,]))

# In the case of missMDA, we have to previously include the   
# preparation step

ncp_1 <- estim_ncpPCA(data[ind,])
ncp_2 <- estim_ncpPCA(data[-ind,])
data1_MDA <- as.data.frame(imputePCA(data[ind,],ncp_1$ncp)$completeObs)
data2_MDA <- as.data.frame(imputePCA(data[-ind,],ncp_2$ncp)$completeObs)

# First i need to add back column with target 

data1_mice$diabetes <- target[ind,]
data2_mice$diabetes <- target[-ind,]
data1_MDA$diabetes <- target[ind,]
data2_MDA$diabetes <- target[-ind,]

# I have to manually create all tasks 

task1_mice <- TaskClassif$new('mice1',data1_mice,'diabetes')
task2_mice <- TaskClassif$new('mice2',data2_mice,'diabetes')

task1_MDA <- TaskClassif$new('MDA1',data1_MDA,'diabetes')
task2_MDA <- TaskClassif$new('MDA2',data1_MDA,'diabetes')


# Train and evaluate them 

Learner<- lrn('classif.glmnet')

# Calulating acc for mice 

  # Fold 1 
  Learner$train(task1_mice)
  acc1 <- Learner$predict(task2_mice)$score(msrs('classif.acc'))
  Learner$reset()
  
  # Fold 2
  Learner$train(task2_mice)
  acc2 <- Learner$predict(task1_mice)$score(msrs('classif.acc'))
  Learner$reset()
  
  # mice acc 
  acc_mice <- (acc1+acc2)/2 
  
# Calulating acc for missMDA 
  
  # Fold 1 
  Learner$train(task1_MDA)
  acc1 <- Learner$predict(task2_MDA)$score(msrs('classif.acc'))
  Learner$reset()
  
  # Fold 2
  Learner$train(task2_MDA)
  acc2 <- Learner$predict(task1_MDA)$score(msrs('classif.acc'))
  Learner$reset()
  
  # missMDA acc 
  acc_MDA <- (acc1+acc2)/2 

# Calculated accuracy   
  
acc_mice
acc_MDA