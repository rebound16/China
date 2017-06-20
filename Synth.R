library(Synth)
library(stargazer)
library(data.table)
library(ggplot2)



### Summary function (yields mean of predictor and predicted variables for real city, synthetic city, and all control cities, as well as sample size)
summary <- function(dataprep.out, synth.tables, firstpreyear, lastpreyear, vardescriptions){
  # Tab.pred (to be expanded)
  tab.pred <- synth.tables$tab.pred
  
  # Synth dependent var
  weighted.dep <- dataprep.out[["Y0plot"]][rownames(dataprep.out[["Y0plot"]]) %in% c(firstpreyear:lastpreyear), ] %*% diag(synth.tables$tab.w$w.weights)
  synth.dep <- rowSums(weighted.dep)
  
  # Dependent var means
  treat.dep.mean <- mean(dataprep.out[["Y1plot"]][rownames(dataprep.out[["Y1plot"]]) %in% c(firstpreyear:lastpreyear), ])
  synth.dep.mean <- mean(synth.dep)
  contr.dep.mean <- mean(dataprep.out[["Y0plot"]][rownames(dataprep.out[["Y0plot"]]) %in% c(firstpreyear:lastpreyear), ])
  
  # Combine them to a vector
  dep.row <- c(treat.dep.mean, synth.dep.mean, contr.dep.mean)
  
  # Sample Size vector
  N <- ncol(dataprep.out[["Y0plot"]])
  N.row <- c(1, 1, N)
  
  # Add to tab.pred
  new.tab.pred <- rbind(N.row, dep.row,  tab.pred)
  colnames(new.tab.pred) <- c("Real", "Synthetic", "Control Cities")
  rownames(new.tab.pred) <- c("N", vardescriptions)
  
  # Print
  new.tab.pred
}



### Ggplot data function (prepares data for use with ggplot2)
synth.plot.data <- function(dataprep.out, synth.tables){
  # Synthetic data
  weighted <- dataprep.out[["Y0plot"]] %*% diag(synth.tables$tab.w$w.weights)
  synthetic <- rowSums(weighted)
  
  # Real data
  real <- dataprep.out[["Y1plot"]]
  
  # Combine
  comb <- cbind.data.frame(synthetic, real)
  comb <- as.data.frame(setDT(comb, keep.rownames = TRUE)[])
  colnames(comb) <- c("year", "Synthetic", "Real")
  comb$year <- as.numeric(comb$year)
  
  # Convert to long format
  comb.data <- as.data.frame(reshape(comb, 
                                     varying = c(list(2:3)),
                                     direction = "long", 
                                     idvar = "year", 
                                     sep = "."))
  comb.data[comb.data$time == 1, "time"] <- "synthetic"
  comb.data[comb.data$time == 2, "time"] <- "real"
  colnames(comb.data) <- c("year", "group", "outcome")
  comb.data
}



### Results data frame function (yields real and synthtic outcome data, absolute gap, and relative gap between them)
f.synth.results <- function(dataprep.out, synth.tables, tryear, var){
  # Synth data
  synth.dep <- as.data.frame(rowSums(dataprep.out[["Y0plot"]][rownames(dataprep.out[["Y0plot"]]) %in% c(tryear:2010), ] %*% diag(synth.tables$tab.w$w.weights)))
  synthvarname <- paste(var, "_synth", sep = "")
  colnames(synth.dep) <- synthvarname
  
  # Real data
  real.dep <- as.data.frame(dataprep.out[["Y1plot"]][rownames(dataprep.out[["Y1plot"]]) %in% c(tryear:2010), ])
  colnames(real.dep) <- var
  
  # Absolute Effect
  abs.effect <- as.data.frame(real.dep[ , var] - synth.dep[ , synthvarname])
  
  # Relative effect
  rel.effect <- as.data.frame((real.dep[ , var] - synth.dep[ , synthvarname]) /  real.dep[ , var])
  
  # Combine 
  results.frame <- cbind(real.dep, synth.dep, abs.effect, rel.effect)
  
  # Rename
  colnames(results.frame) <- c(var, synthvarname, "abs.effect", "rel.effect")
  
  # Print
  results.frame
}
