
################################################################################
# mxnetR - Custom model #       
#!#######################

# Step by step neural network creation

library(mxnet)

#####################
# Callback function #
#####################

# custom callback funtion for making model checkpoints and error log saving 
# simultaneously
mx.callback.cus <- function(prefix, period, logger=NULL) {
  # code for log saving
  function(iteration, nbatch, env, verbose) {
    if (nbatch %% period == 0 && !is.null(env$metric)) {
      result <- env$metric$get(env$train.metric)
      if (nbatch != 0)
        if(verbose) cat(paste0("Batch [", nbatch, "] Train-", result$name, 
                               "=", result$value, "\n"))
      if (!is.null(logger)) {
        if (class(logger) != "mx.metric.logger") {
          stop("Invalid mx.metric.logger.")
        }
        logger$train <- c(logger$train, result$value)
        if (!is.null(env$eval.metric)) {
          result <- env$metric$get(env$eval.metric)
          if (nbatch != 0)
            cat(paste0("Batch [", nbatch, "] Validation-", result$name, 
                       "=", result$value, "\n"))
          logger$eval <- c(logger$eval, result$value)
        }
      }
    }
    # code for saving model - checkpoint 
    # https://mxnet.incubator.apache.org/versions/master/tutorials/r/CallbackFunction.html
    if (iteration %% period == 0) {
      mx.model.save(env$model, prefix, iteration)
      if(verbose) cat(sprintf("Model checkpoint saved to %s-%04d.params\n", 
                              prefix, iteration))
    }
    return(TRUE)
  }
}


#############################
# Creating ANN step by step #
#############################

data <- mx.symbol.Variable("data")
# First layer
fc1 <- mx.symbol.FullyConnected(data, num_hidden=12)
# Activation function for parameters explosion prevention
act1 = mx.symbol.Activation(data=fc1, name='sig', act_type="sigmoid")
# 2nd layer
fc2 = mx.symbol.FullyConnected(data=act1, name='fc2', num_hidden=18)
# 3rd layer
fc3 = mx.symbol.FullyConnected(data=fc2, name='fc3', num_hidden=24)
# 4th layer
fc100 = mx.symbol.FullyConnected(data=fc3, name='fc100', num_hidden=1)
# Output
lro1 <- mx.symbol.LogisticRegressionOutput(fc100)

# error logging to this variable
logger <- mx.metric.logger$new()


# MLP model creation
model <- mx.model.FeedForward.create(lro1, # activation function
                                     X=training2.x, 
                                     y=training2.y, 
         ctx=mx.cpu(), # cpu job
         num.round=400, # number of epochs
         eval.metric = mx.metric.logloss, # options:  logloss, logistic_acc, 
         # accuracy, top_k_accuracy, mse, rmse
         # rmsle - Root Mean Squared Logarithmic Error
         # Perplexity
         # mae - Mean Absolute Error
         eval.data = list(data=valid2.x, label = valid2.y), # data for 
         # instant evaluation
         
         optimizer = "adam", # options: sgd, rmsprop, adagrad, AdaDelta, 
                             # nag (Nesterov Accelerated SGD)
         array.batch.size = 128, # 
         learning.rate=0.003, # rate of wages corrections power
         # momentum = 0.15, # not needed with adam optimizer
         array.layout = "rowmajor",
         verbose=T,
         
         # model checkpoint
         # epoch.end.callback = mx.callback.save.checkpoint("model_gen_1"),
         
         # error logging to "logger"
         # epoch.end.callback = mx.callback.log.train.metric(1, logger))  
         
         # custom callback function
         epoch.end.callback = mx.callback.cus("model_gen_1", 1, logger)) 
         # Arguments:
         # (1) model prefix (just name for model)
         # (2) number of epochs per 1 model saving 
         # (3) variable name where logs will be saved


########################
# Best model selection #
########################

# plot the training process errors 
plot(logger$eval, type="l")

# check minimal error
min(logger$eval) 

# check position of minimal error model
min <- which.min(logger$eval)

# load model from tracking which minimize error
model <- mx.model.load(prefix="model_gen_1", iteration = min)

# General structure of MLP on chart
# graph.viz(model$symbol)


##############
# Prediction #
##############

# prediction on training and validation set
pred_training <- predict(model, training2.x)
pred_valid <- predict(model, valid2.x)

# transforming training prediction into df
pred_training2 <- t(as.data.frame(pred_training))
pred_valid2 <- t(as.data.frame(pred_valid))

##############
# Evaluation #
##############

Gini_value(pred_training2, training$DefFlag)
Gini_value(pred_valid2, valid$DefFlag)


# Example results:

# layers 12 18 24 1
# [1] 0.5363481 # gini on training set
# [1] 0.5151612 # gini on valid set




