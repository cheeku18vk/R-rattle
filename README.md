# R-rattle
#useing R
#=======================================================================

# Rattle is Copyright (c) 2006-2018 Togaware Pty Ltd.
# It is free (as in libre) open source software.
# It is licensed under the GNU General Public License,
# Version 2. Rattle comes with ABSOLUTELY NO WARRANTY.
# Rattle was written by Graham Williams with contributions
# from others as acknowledged in 'library(help=rattle)'.
# Visit https://rattle.togaware.com/ for details.

#=======================================================================
# Rattle timestamp: 2019-06-14 15:27:44 x86_64-w64-mingw32 

# Rattle version 5.2.0 user 'USER'

# This log captures interactions with Rattle as an R script. 

# For repeatability, export this activity log to a 
# file, like 'model.R' using the Export button or 
# through the Tools menu. Th script can then serve as a 
# starting point for developing your own scripts. 
# After xporting to a file called 'model.R', for exmample, 
# you can type into a new R Console the command 
# "source('model.R')" and so repeat all actions. Generally, 
# you will want to edit the file to suit your own needs. 
# You can also edit this log in place to record additional 
# information before exporting the script. 
 
# Note that saving/loading projects retains this log.

# We begin most scripts by loading the required packages.
# Here are some initial packages to load and others will be
# identified as we proceed through the script. When writing
# our own scripts we often collect together the library
# commands at the beginning of the script here.

  # Access the weather dataset and utilities.
 # Utilise %>% and %<>% pipeline operators.

# This log generally records the process of building a model. 
# However, with very little effort the log can also be used 
# to score a new dataset. The logical variable 'building' 
# is used to toggle between generating transformations, 
# when building a model and using the transformations, 
# when scoring a dataset.

building <- TRUE
scoring  <- ! building

# A pre-defined value is used to reset the random seed 
# so that results are repeatable.

crv$seed <- 42 

#=======================================================================
# Rattle timestamp: 2019-06-14 15:28:34 x86_64-w64-mingw32 

# Load a dataset from file.

library(readxl, quietly=TRUE)

 crs$dataset <- read_excel("C:/Users/USER/Desktop/Charity rstudio.xlsx", guess_max=1e4)

 crs$dataset

#=======================================================================
# Rattle timestamp: 2019-06-14 15:28:37 x86_64-w64-mingw32 

# Action the user selections from the Data tab. 

# Build the train/validate/test datasets.

# nobs=2398 train=1679 validate=360 test=359

set.seed(crv$seed)

crs$nobs <- nrow(crs$dataset)

crs$train <- sample(crs$nobs, 0.7*crs$nobs)

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  sample(0.15*crs$nobs) ->
crs$validate

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  setdiff(crs$validate) ->
crs$test

# The following variable selections have been noted.

crs$input     <- c("response", "orispend", "orivisit", "spendb",
                   "visitb", "promspd", "promvis", "promspdb",
                   "promvisb", "totvisit", "totspend", "forpcode",
                   "mos", "mosgroup", "title", "sex", "yob", "age")

crs$numeric   <- c("response", "orispend", "orivisit", "spendb",
                   "visitb", "promspd", "promvis", "promspdb",
                   "promvisb", "totvisit", "totspend", "mos",
                   "mosgroup", "sex", "yob", "age")

crs$categoric <- c("forpcode", "title")

crs$target    <- "ageband"
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- NULL
crs$weights   <- NULL

#=======================================================================
# Rattle timestamp: 2019-06-14 15:29:04 x86_64-w64-mingw32 

# Action the user selections from the Data tab. 

# Build the train/validate/test datasets.

# nobs=2398 train=1679 validate=360 test=359

set.seed(crv$seed)

crs$nobs <- nrow(crs$dataset)

crs$train <- sample(crs$nobs, 0.7*crs$nobs)

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  sample(0.15*crs$nobs) ->
crs$validate

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  setdiff(crs$validate) ->
crs$test

# The following variable selections have been noted.

crs$input     <- NULL

crs$numeric   <- NULL

crs$categoric <- NULL

crs$target    <- "response"
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- c("orispend", "orivisit", "spendb", "visitb", "promspd", "promvis", "promspdb", "promvisb", "totvisit", "totspend", "forpcode", "mos", "mosgroup", "title", "sex", "yob", "age", "ageband")
crs$weights   <- NULL

#=======================================================================
# Rattle timestamp: 2019-06-14 15:29:13 x86_64-w64-mingw32 

# The 'Hmisc' package provides the 'contents' function.

library(Hmisc, quietly=TRUE)

# Obtain a summary of the dataset.

contents(crs$dataset[crs$train, c(crs$input, crs$risk, crs$target)])
summary(crs$dataset[crs$train, c(crs$input, crs$risk, crs$target)])

#=======================================================================
# Rattle timestamp: 2019-06-14 15:29:48 x86_64-w64-mingw32 

# Display histogram plots for the selected variables. 

# Use ggplot2 to generate histogram plot for response

# Generate the plot.

p01 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::mutate(response=as.factor(response)) %>%
  dplyr::select(response, response) %>%
  ggplot2::ggplot(ggplot2::aes(x=response)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::geom_density(ggplot2::aes(fill=response, colour=response), alpha=0.55) +
  ggplot2::xlab("response\n\nRattle 2019-Jun-14 15:29:48 USER") +
  ggplot2::ggtitle("Distribution of response (sample)\nby response") +
  ggplot2::labs(fill="response", y="Density")

# Use ggplot2 to generate histogram plot for sex

# Generate the plot.

p02 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::mutate(response=as.factor(response)) %>%
  dplyr::select(sex, response) %>%
  ggplot2::ggplot(ggplot2::aes(x=sex)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::geom_density(ggplot2::aes(fill=response, colour=response), alpha=0.55) +
  ggplot2::xlab("sex\n\nRattle 2019-Jun-14 15:29:48 USER") +
  ggplot2::ggtitle("Distribution of sex (sample)\nby response") +
  ggplot2::labs(fill="response", y="Density")

# Display the plots.

gridExtra::grid.arrange(p01, p02)

#=======================================================================
# Rattle timestamp: 2019-06-14 15:30:09 x86_64-w64-mingw32 

# Remap variables. 

# Transform into a factor.

  crs$dataset[["TFC_response"]] <- as.factor(crs$dataset[["response"]])

  ol <- levels(crs$dataset[["TFC_response"]])
  lol <- length(ol)
  nl <- c(sprintf("[%s,%s]", ol[1], ol[1]), sprintf("(%s,%s]", ol[-lol], ol[-1]))
  levels(crs$dataset[["TFC_response"]]) <- nl

#=======================================================================
# Rattle timestamp: 2019-06-14 15:30:10 x86_64-w64-mingw32 

# Action the user selections from the Data tab. 

# The following variable selections have been noted.

crs$input     <- "TFC_response"

crs$numeric   <- NULL

crs$categoric <- "TFC_response"

crs$target    <- NULL
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- c("response", "orispend", "orivisit", "spendb", "visitb", "promspd", "promvis", "promspdb", "promvisb", "totvisit", "totspend", "forpcode", "mos", "mosgroup", "title", "sex", "yob", "age", "ageband")
crs$weights   <- NULL

#=======================================================================
# Rattle timestamp: 2019-06-14 15:30:15 x86_64-w64-mingw32 

# Remap variables. 

# Transform into a factor.

  crs$dataset[["TFC_sex"]] <- as.factor(crs$dataset[["sex"]])

  ol <- levels(crs$dataset[["TFC_sex"]])
  lol <- length(ol)
  nl <- c(sprintf("[%s,%s]", ol[1], ol[1]), sprintf("(%s,%s]", ol[-lol], ol[-1]))
  levels(crs$dataset[["TFC_sex"]]) <- nl

#=======================================================================
# Rattle timestamp: 2019-06-14 15:30:16 x86_64-w64-mingw32 

# Action the user selections from the Data tab. 

# The following variable selections have been noted.

crs$input     <- c("TFC_response", "TFC_sex")

crs$numeric   <- NULL

crs$categoric <- c("TFC_response", "TFC_sex")

crs$target    <- NULL
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- c("response", "orispend", "orivisit", "spendb", "visitb", "promspd", "promvis", "promspdb", "promvisb", "totvisit", "totspend", "forpcode", "mos", "mosgroup", "title", "sex", "yob", "age", "ageband")
crs$weights   <- NULL

#=======================================================================
# Rattle timestamp: 2019-06-14 15:30:49 x86_64-w64-mingw32 

# Action the user selections from the Data tab. 

# Build the train/validate/test datasets.

# nobs=2398 train=1679 validate=360 test=359

set.seed(crv$seed)

crs$nobs <- nrow(crs$dataset)

crs$train <- sample(crs$nobs, 0.7*crs$nobs)

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  sample(0.15*crs$nobs) ->
crs$validate

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  setdiff(crs$validate) ->
crs$test

# The following variable selections have been noted.

crs$input     <- c("orispend", "orivisit", "spendb", "promspd",
                   "mosgroup", "sex", "yob", "age", "ageband",
                   "TFC_response", "TFC_sex")

crs$numeric   <- c("orispend", "orivisit", "spendb", "promspd",
                   "mosgroup", "sex", "yob", "age", "ageband")

crs$categoric <- c("TFC_response", "TFC_sex")

crs$target    <- "response"
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- c("visitb", "promvis", "promspdb", "promvisb", "totvisit", "totspend", "forpcode", "mos", "title")
crs$weights   <- NULL

#=======================================================================
# Rattle timestamp: 2019-06-14 15:32:06 x86_64-w64-mingw32 

# Action the user selections from the Data tab. 

# Build the train/validate/test datasets.

# nobs=2398 train=1679 validate=360 test=359

set.seed(crv$seed)

crs$nobs <- nrow(crs$dataset)

crs$train <- sample(crs$nobs, 0.7*crs$nobs)

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  sample(0.15*crs$nobs) ->
crs$validate

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  setdiff(crs$validate) ->
crs$test

# The following variable selections have been noted.

crs$input     <- c("response", "orispend", "orivisit", "spendb",
                   "promspd", "mosgroup", "sex", "yob", "age",
                   "ageband", "TFC_sex")

crs$numeric   <- c("response", "orispend", "orivisit", "spendb",
                   "promspd", "mosgroup", "sex", "yob", "age",
                   "ageband")

crs$categoric <- "TFC_sex"

crs$target    <- "TFC_response"
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- c("visitb", "promvis", "promspdb", "promvisb", "totvisit", "totspend", "forpcode", "mos", "title")
crs$weights   <- NULL

#=======================================================================
# Rattle timestamp: 2019-06-14 15:32:21 x86_64-w64-mingw32 

# Regression model 

# Build a Regression model.

crs$glm <- glm(TFC_response ~ .,
    data=crs$dataset[crs$train, c(crs$input, crs$target)],
    family=binomial(link="logit"))

# Generate a textual view of the Linear model.

print(summary(crs$glm))

cat(sprintf("Log likelihood: %.3f (%d df)\n",
            logLik(crs$glm)[1],
            attr(logLik(crs$glm), "df")))

cat(sprintf("Null/Residual deviance difference: %.3f (%d df)\n",
            crs$glm$null.deviance-crs$glm$deviance,
            crs$glm$df.null-crs$glm$df.residual))

cat(sprintf("Chi-square p-value: %.8f\n",
            dchisq(crs$glm$null.deviance-crs$glm$deviance,
                   crs$glm$df.null-crs$glm$df.residual)))

cat(sprintf("Pseudo R-Square (optimistic): %.8f\n",
             cor(crs$glm$y, crs$glm$fitted.values)))

cat('\n==== ANOVA ====\n\n')
print(anova(crs$glm, test="Chisq"))
cat("\n")

# Time taken: 0.95 secs

#=======================================================================
# Rattle timestamp: 2019-06-14 15:32:37 x86_64-w64-mingw32 

# Evaluate model performance on the full dataset. 

# Generate an Error Matrix for the Linear model.

# Obtain the response from the Linear model.

crs$pr <- as.vector(ifelse(predict(crs$glm, 
   type    = "response",
   newdata = crs$dataset[c(crs$input, crs$target)]) > 0.5, "(0,1]", "[0,0]"))

# Generate the confusion matrix showing counts.

rattle::errorMatrix(crs$dataset[c(crs$input, crs$target)]$TFC_response, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(crs$dataset[c(crs$input, crs$target)]$TFC_response, crs$pr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))
