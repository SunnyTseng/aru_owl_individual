
### Frequency
CV_all <- c(33, 31, 32, 37, 32, 36, 30, 31, 6, 18, 8, 5, 6, 5, 6, 5, 5, 5)
CV_site <- c(31, 29, 32, 37, 26, 34, 32, 28, 5, 15, 6, 4, 4, 4, 4, 3, 4, 3)

t.test(CV_all, CV_site, paired = TRUE, alternative = "two.sided")


### Temporal
CV_all <- c(25, 16, 25, 46, 19, 14, 23, 26, 5, 9, 5, 5)
CV_site <- c(21, 12, 18, 28, 17, 12, 18, 24, 3, 5, 4, 3)
t.test(CV_all, CV_site, paired = TRUE, alternative = "two.sided")
