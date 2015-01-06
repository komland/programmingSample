# An experiment has been performed to test the effects of a fertilizer.
# Four plots were randomly assigned to either control or fertilized treatment.
# Please provide code (not output) to explore the data and describe the results.
dat4 <- data.frame(treatment = factor(rep(c("control", "fertilized"), each = 20)),
                   plot = factor(rep(c("C", "D", "B", "A"), each = 10)),
                   y = c( 9.99, 10.31,  9.64, 10.51,  9.31,  9.55, 10.23,  9.95,  9.96, 10.49,
                          9.17,  9.23,  9.35,  9.71,  8.92, 10.23, 10.15,  9.70, 10.09, 10.46,
                         10.94,  9.75, 10.44, 10.09, 10.53, 10.43,  9.75, 10.58, 11.06,  9.72,
                          9.98, 10.44,  9.78, 10.01, 10.12, 11.84, 10.64, 10.80,  9.75,  9.99))
