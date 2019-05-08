
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


