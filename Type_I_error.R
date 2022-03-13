# Simulace chyba I druhu ---------------------------------------------
set.seed(123)
N <- 1e6
x1 <- rnorm(N, 4, 2)
x2 <- rnorm(N, 2, 5)
x3 <- runif(N, 1, 4)
x4 <- rnorm(N, 1, 3)
error <- rnorm(N, 0, 10)
y <- 0.5 + 1.5 * x1 + 25 * x2 + 400 * x3 + 150 * x4 + error
populace <- data.frame(y, x1, x2, x3, x4)
p_hodnoty <- c()

for (i in 1:1000) {
  vyber <- dplyr::sample_n(populace, size = 500)
  reg1 <- lm(x1 ~ x2 + x3 + x4, data = vyber)
  sum1 <- summary(reg1)
  p_hodnoty[i] <- sum1$coefficients[2, 4]
}

plot(p_hodnoty, pch = 20, col = ifelse(p_hodnoty >= 0.05, "blue", "red"))
abline(h = 0.05)

zamitame <- length(which(p_hodnoty < 0.05))
nezamitame <- length(which(p_hodnoty >= 0.05))
# v kolika procentech případů jsme zamítli platnou hypotézu?
zamitame / (zamitame + nezamitame)
