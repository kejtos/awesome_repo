# Simulace ne?varu koeficientu determinace  -------------------------------

library("dplyr")
set.seed(55)
N <- 100
C <- c()
D <- c()
const <- cbind(rep(1, N))
X <- cbind(const)
Y <- rnorm(N, 2, 2)

for (i in 1:100) {
   xi <- rnorm(N, 1, 2)
   X <- cbind(X, xi)
   k <- ncol(X) - 1
   beta <- solve(t(X) %*% X) %*% t(X) %*% Y
   Y_hat <- X %*% beta
   SSR <- sum((Y_hat - Y)^2)
   SST <- sum((Y - mean(Y))^2)
   SSE <- SST - SSR
   R_squared <- SSE / SST
   R_adj <- 1 - (((1 - R_squared) * (N - 1)) / (N - k - 1))
   print(i)
   C[i] <- R_squared
   D[i] <- R_adj
}

par(
   mfrow = c(2, 1),
   mar = c(2, 4, 1, 2)
)
plot.ts(C,
        col = "red",
        ylab = "R Squared",
        xlab = NULL
)
abline(h = 0, lty = 2)
abline(h = 1, lty = 2)
plot.ts(D,
        col = "blue",
        ylab = "Adj. R^2",
        xlab = "Po?et regresor?",
        ylim = c(-1.5, 1.1)
)
abline(
   h = 0,
   lty = 2
)
par(mfrow = c(1, 1))
###########################################################################

# Simulace vlastnost? extim?tor? OLS

library("tidyverse")

# Populace vs v?b?r

set.seed(55) # zajist?, ?e jsou generov?na ta sam? ??sla
populace <- rnorm(10000, 180, 20) # populace s norm?ln?m rozd?len?m

par(mfrow = c(2, 1), mar = c(2, 4, 1, 2)) # dovol? 2 grafy pod sebe
hist(populace, main = "Histogram populace")

set.seed(42)
vyber1 = sample(populace, 50) # v?b?r z populace
hist(vyber1, main = "Histogram v?b?ru z populace")
par(mfrow = c(1, 1))

# Vytvo?en? dat a modelu s jednou exogenn? prom?nnou pro OLS
n <- 1000 # po?et pozorov?n?
x <- seq(from = 1, to = n)
e <- rnorm(n, 0, 800) # residua s norm?ln?m rozd?len?m
y <- 0.5 + 3 * x + e # beta_0 = 0.5 a beta_1 = 3

plot(x, y, col = "#a6a6a6", cex = 0.5) # plot y a x
populace <- data.frame(y, x) # p?eveden? na dataframe 
intercept <- c() # beta_0 (zat?m pr?zdn?)
slope <- c() # beta_1 (zat?m pr?zdn?)

# OLS opdhady ze 100 r?zn?ch v?b?r? 50 pozorov?n?
for (i in 1:100){ # 100 r?zn?ch v?b?r?
   set.seed(3*i)
   vyber <- sample(n, 50) # v?b?r 50 pozorov?n?
   xs <- x[vyber] # x z v?b?ru
   ys <- y[vyber] # y z v?b?ru
   vyber <- data.frame(y = ys, x = xs) # p?eveden? na dataframe
   reg <- summary(lm(y ~ x, vyber)) # OLS regrese 
   intercept[i] <- reg$coefficients[1,1] # vyta?en? a ulo?en? beta_0
   slope[i] <- reg$coefficients[2,1] # vyta?en? a ulo?en? beta_1
   abline(a = intercept[i], slope[i], col = "#6a6a6a") # nakreslen? regresn? p??mky do grafu
}

abline(a = 0.5, b = 3, col = 'red', lwd = 3) # nakreslen? re?ln? p??mky do grafu
head(cbind('Intercept (beta_0)' = intercept, 'Slope (beta_1)' = slope), n = 20) # koeficienty prvn?ch 20 regres?
mean(intercept) # pr?m?r beta_0
mean(slope) # pr?m?r beta_1

par(mfrow = c(2, 1), mar = c(2, 4, 1, 2))
hist(intercept, main = "Histogram interceptu (beta_0)") # histogram beta_0
abline(v = mean(intercept), col = 'blue', lwd = 5) # nakreslen? pr?m?ru beta_0 do grafu

hist(slope, main = "Histogram sklonu (beta_0)") # histogram beta_1
abline(v = mean(slope), col = 'blue', lwd = 5) # nakreslen? pr?m?ru beta_1 do grafu
par(mfrow = c(1, 1))

###########################################################################
# test change
