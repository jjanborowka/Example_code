# Creating Graph

graph_mice <- PipeOpMice$new() %>>% lrn('classif.glmnet')
graph_missMDA <-
  PipeOpMissMDA_PCA_MCA_FMAD$new() %>>% lrn('classif.glmnet')

# Creating Graphe Learners

Learner_mice <-  GraphLearner$new(graph_mice)
Learner_missMDA <- GraphLearner$new(graph_missMDA)

# Cross-Validation

set.seed(1)
rr_mice <- resample(tsk('pima'), Learner_mice, rsmp('cv', folds = 2))
set.seed(1)
rr_missMDA <-
  resample(tsk('pima'), Learner_missMDA, rsmp('cv', folds = 2))

# Compering accuracy

rr_mice$aggregate(msr('classif.acc'))
rr_missMDA$aggregate(msr('classif.acc'))