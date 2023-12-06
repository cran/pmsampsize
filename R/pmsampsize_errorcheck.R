pmsampsize_errorcheck <- function(type,csrsquared,nagrsquared,rsquared,parameters,shrinkage,cstatistic,prevalence,
                                  rate,timepoint,meanfup,intercept,sd,mmoe) {

  # syntax checks
  if (type != "c" & type != "b" & type != "s") stop('type must be "c" for continuous, "b" for binary or "s" for survival')
  if (is.numeric(parameters) == F) stop('parameters must be an integer')
  if (parameters != round(parameters)) stop('parameters must be an integer')


  # parameter restrictions
  if (shrinkage <= 0 | shrinkage >= 1) stop('shrinkage must be between 0 and 1')

  # parameters for continuous
  if (type == "c") {
    # syntax
    if (is.na(rsquared)) stop('rsquared must be specified for continuous outcome models')
    # parameters needed
    if (is.na(sd)) stop('sd must be specified for continuous sample size')
    if (is.na(intercept)) stop('intercept must be specified for continuous sample size')
    # parameter conditions
    if (is.numeric(rsquared) == F) stop('rsquared must be numeric')
    if (is.numeric(intercept) == F) stop('intercept must be numeric')
    if (is.numeric(sd) == F) stop('sd must be numeric')
    # parameters not needed
    if (is.na(prevalence) == F) stop('prevalence not required for continuous sample size')
    if (is.na(rate) == F) stop('rate not required for continuous sample size')
    if (is.na(timepoint) == F) stop('timepoint not required for continuous sample size')
    if (is.na(meanfup) == F) stop('meanfup not required for continuous sample size')
  }


  # parameters for binary
  if (type == "b") {
    # parameters needed
    if (is.na(prevalence)) stop('prevalence must be specified for binary sample size')
    # parameter conditions
    if (is.numeric(prevalence) == F) stop('prevalence must be numeric')
    if (is.na(cstatistic) == F) {
      if (cstatistic < 0 | cstatistic > 1) stop('cstatistic must be between 0 and 1')
      if (is.numeric(cstatistic) == F) stop('cstatistic must be numeric')
    }
    if (is.na(nagrsquared) == F) {
      if (nagrsquared < 0 | nagrsquared > 1) stop('nagrsquared must be between 0 and 1')
      if (is.numeric(nagrsquared) == F) stop('nagrsquared must be numeric')
    }
    if (is.na(csrsquared) == F) {
      if (csrsquared < 0 | csrsquared > 1) stop('csrsquared must be between 0 and 1')
      if (is.numeric(csrsquared) == F) stop('csrsquared must be numeric')
    }
    if (prevalence <= 0 | prevalence >= 1) stop('prevalence must be between 0 and 1')
    # parameters not needed
    if (is.na(rate) == F) stop('rate not required for binary sample size')
    if (is.na(timepoint) == F) stop('timepoint not required for binary sample size')
    if (is.na(meanfup) == F) stop('meanfup not required for binary sample size')
    if (is.na(intercept) == F) stop('intercept not required for binary sample size')
    if (is.na(sd) == F) stop('sd not required for binary sample size')
  }

  # parameters for survival
  if (type == "s") {
    # parameters needed
    if (is.na(rate)) stop('rate must be specified for survival sample size')
    if (is.na(timepoint)) stop('timepoint must be specified for survival sample size')
    if (is.na(meanfup)) stop('meanfup must be specified for survival sample size')
    # parameter conditions
    if (is.na(nagrsquared) == F) {
      if (nagrsquared < 0 | nagrsquared > 1) stop('nagrsquared must be between 0 and 1')
      if (is.numeric(nagrsquared) == F) stop('nagrsquared must be numeric')
    }
    if (is.na(csrsquared) == F) {
      if (csrsquared < 0 | csrsquared > 1) stop('csrsquared must be between 0 and 1')
      if (is.numeric(csrsquared) == F) stop('csrsquared must be numeric')
    }
    if (is.numeric(rate) == F) stop('rate must be numeric')
    if (is.numeric(timepoint) == F) stop('timepoint must be numeric')
    if (is.numeric(meanfup) == F) stop('meanfup must be numeric')
    if (rate <= 0) stop('the specified overall event rate must be greater than 0')
    if (timepoint <= 0) stop('the timepoint of interest for prediction must be greater than 0')
    if (meanfup <= 0) stop('the average mean follow-up time must be greater than 0')
    # parameters not needed
    if (is.na(prevalence) == F) stop('prevalence not required for survival sample size')
    if (is.na(intercept) == F) stop('intercept not required for survival sample size')
    if (is.na(sd) == F) stop('sd not required for survival sample size')
  }
}

