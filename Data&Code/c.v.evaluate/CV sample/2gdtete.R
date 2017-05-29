
# Compute the training and test errors for each model
pred <- list()
mse <- te <- matrix(NA, nrow = n_df, ncol = n_rep)
for (i in 1:n_rep) {
  mse[, i] <- sapply(res[[i]], function(obj) deviance(obj)/nobs(obj))
  pred[[i]] <- mapply(function(obj, degf) predict(obj, data.frame(x = xy_test$x)), 
                      res[[i]], df)
  te[, i] <- sapply(as.list(data.frame(pred[[i]])), function(y_hat) mean((xy_test$y - 
                                                                            y_hat)^2))
}

# Compute the average training and test errors
av_mse <- rowMeans(mse)
av_te <- rowMeans(te)

# Plot the errors
plot(df, av_mse, type = "l", lwd = 2, col = gray(0.4), ylab = "Prediction error", 
     xlab = "Flexibilty (spline's degrees of freedom [log scaled])", ylim = c(0, 
                                                                              1), log = "x")
abline(h = sigma_eps, lty = 2, lwd = 0.5)
for (i in 1:n_rep) {
  lines(df, te[, i], col = "lightpink")
}
for (i in 1:n_rep) {
  lines(df, mse[, i], col = gray(0.8))
}
lines(df, av_mse, lwd = 2, col = gray(0.4))
lines(df, av_te, lwd = 2, col = "darkred")
points(df[1], av_mse[1], col = "palegreen3", pch = 17, cex = 1.5)
points(df[1], av_te[1], col = "palegreen3", pch = 17, cex = 1.5)
points(df[which.min(av_te)], av_mse[which.min(av_te)], col = "darkorange", pch = 16, 
       cex = 1.5)
points(df[which.min(av_te)], av_te[which.min(av_te)], col = "darkorange", pch = 16, 
       cex = 1.5)
points(df[25], av_mse[25], col = "steelblue", pch = 15, cex = 1.5)
points(df[25], av_te[25], col = "steelblue", pch = 15, cex = 1.5)
legend(x = "top", legend = c("Training error", "Test error"), lwd = rep(2, 2), 
       col = c(gray(0.4), "darkred"), text.width = 0.3, cex = 0.85)