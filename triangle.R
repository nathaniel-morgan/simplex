{
    mean_x <- mean(x)
    sd_x <- sd(x)
    outlier_range <- sd_x * 2.5
    outlier <- data.frame(x = x[x - outlier_range > 0 | x + outlier_range < 
        0])
    x_trim <- x[x - outlier_range < 0 & x + outlier_range > 0]
    qx <- quantile(x_trim, probs = c(0, 0.25, 0.5, 0.75, 1))
    x_trim <- data.frame(x = x_trim)
    print(ggplot(outlier, aes(x = 0)) + geom_point(aes(y = x), 
        pch = 24) + geom_errorbar(ymin = qx[1], ymax = qx[5], 
        width = 0.1) + geom_crossbar(ymin = qx[2], y = qx[3], 
        ymax = qx[4], width = 0.2, fill = "white") + expand_limits(y = range(x)))
}
