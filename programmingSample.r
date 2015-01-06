# An experiment has been performed to test the effects of a fertilizer.
# Four plots were randomly assigned to either control or fertilized treatment.
# Please provide code (not output) to explore the data and describe the results.
# You can see how I simulated the data, but 
dat4 <- data.frame(treatment = factor(rep(c("control", "fertilized"), each = 20)),
                   plot = factor(rep(c("C", "D", "B", "A"), each = 10)),
                   y = c(10.58,  9.67, 10.21, 10.68,  9.98, 10.56, 10.54,  9.01, 10.46,  9.74,
                         10.18, 10.25, 10.20, 10.00,  9.13, 10.02, 10.20, 10.09,  9.67, 10.08,
                          9.34, 10.56 10.60 10.32 9.60 10.62,  9.97,  9.20,  8.93,  9.48 10.94 10.47,  9.66,  9.71 10.34,  9.97,  9.48 10.51,  9.94 10.07))