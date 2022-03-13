# Simulace nesvaru koeficientu determinace  -------------------------------

library("dplyr")
set.seed(55)
N <- 100
C <- c()
D <- c()
const <- cbind(rep(1, N))
X <- cbind(const)
Y <- rnorm(N, 2, 2)
mean_gen <- rnorm(seq(from = 1, to = N), 0, 10)
sd_gen <- abs(rnorm(N, 0, 10))

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

# Simulace vlastnosti extim?toru OLS

library("tidyverse")

# Populace vs vyber

set.seed(55) # zajisti, ze jsou generovana ta sama cisla
populace <- rnorm(10000, 180, 20) # populace s normalnim rozdilenem

par(mfrow = c(2, 1), mar = c(2, 4, 1, 2)) # dovoli 2 grafy pod sebe
hist(populace, main = "Histogram populace")

set.seed(42)
vyber1 = sample(populace, 50) # vyber z populace
hist(vyber1, main = "Histogram vyberu z populace")
par(mfrow = c(1, 1))

# Vytvoreni dat a modelu s jednou exogenni promennou pro OLS
n <- 10000 # pocet pozorovani
x <- seq(from = 1, to = n)
e <- rnorm(n, 0, 8000) # residua s normalnim rozdilen?m
y <- 0.5 + 3 * x + e # beta_0 = 0.5 a beta_1 = 3

plot(x, y, col = "#a6a6a6", cex = 0.5) # plot y a x
populace <- data.frame(y, x) # prevedeni na dataframe 
intercept <- c() # beta_0 (zatim prazdny)
slope <- c() # beta_1 (zatim prazdny)

# OLS opdhady ze 100 ruznych vyberu o 50 pozorovani
for (i in 1:100){ # 100 vyberu
   set.seed(3*i) # zaruci, ze se jedna o 100 ruznych vyberu, a zaroven ze to bude pokazde tech samych 10 vyberu.
   vyber <- sample(n, 50) # vyber 50 pozorovani
   xs <- x[vyber] # x z vyberu
   ys <- y[vyber] # y z vyberu
   vyber <- data.frame(y = ys, x = xs) # prevedeni na dataframe
   reg <- summary(lm(y ~ x, vyber)) # OLS regrese
   intercept[i] <- reg$coefficients[1,1] # vytazeni a ulozeni beta_0
   slope[i] <- reg$coefficients[2,1] # vytazeni a ulozeni beta_1
   abline(a = intercept[i], slope[i], col = "#6a6a6a") # nakresleni regresni primky do grafu
}

abline(a = 0.5, b = 3, col = 'red', lwd = 3) # nakresleni realne primky do grafu
head(cbind('Intercept (beta_0)' = intercept, 'Slope (beta_1)' = slope), n = 20) # koeficienty prvnich 20 regresi
mean(intercept) # prumer beta_0
mean(slope) # prumer beta_1

par(mfrow = c(2, 1), mar = c(2, 4, 1, 2))
hist(intercept, main = "Histogram interceptu (beta_0)") # histogram beta_0
abline(v = mean(intercept), col = 'blue', lwd = 5) # nakresleni prumeru beta_0 do grafu

hist(slope, main = "Histogram sklonu (beta_0)") # histogram beta_1
abline(v = mean(slope), col = 'blue', lwd = 5) # nakresleni prumeru beta_1 do grafu
par(mfrow = c(1, 1))

###########################################################################

library("ggplot2")

intercept <- data.frame()
slope <- data.frame()

# OLS opdhady ze 100 ruznych vyberu o 50 pozorovani
for (i in 1:100){
   for (j in 1:100){ # 100 x 100 výběrů
      set.seed(3*i) # zaruci, ze se jedna o ruznych vybery, a zaroven ze budou pri kazde simulaci stejne
      vyber <- sample(n, 20*j) # postupne vetsi a vetsi vybery (1. - 100 výběrů o velikosti 20, 2. 100 výběrů o velikosti 40, ..., 100. 100 výběrů o velikosti 2000)
      xs <- x[vyber] # x z vyberu
      ys <- y[vyber] # y z vyberu
      vyber <- data.frame(y = ys, x = xs) # prevedeni na dataframe
      reg <- summary(lm(y ~ x, vyber)) # OLS regrese
      intercept[i,j] <- reg$coefficients[1,1] # vytazeni a ulozeni beta_0
      slope[i,j] <- reg$coefficients[2,1] # vytazeni a ulozeni beta_1
   }
}

for (k in 1:100){
   var[k] = var(slope[,k])
}

par(mfrow = c(2, 1), mar = c(2, 4, 1, 2))
plot(var, type = "b", col = 'blue', pch = 19, lwd = .5)
abline(h = 0, lty = 'dashed')


plot(var, type = "b", col = 'blue', pch = 19, lwd = .5, xlim = c(50, 100), ylim = c(0, 0.0097))
abline(h = 0, lty = 'dashed')
par(mfrow = c(1, 1))


ggplot(diamonds, aes(price, colour = cut)) +
   geom_freqpoly(binwidth = 500)


slope_20 <- data.frame(slope = slope[, 1])
slope_100 <- data.frame(slope = slope[, 5])
slope_500 <- data.frame(slope = slope[, 25])
slope_1000 <- data.frame(slope = slope[, 50])
slope_2000 <- data.frame(slope = slope[, 100])
slope_20$vyber <- 'Slope s vyberem N = 20'
slope_100$vyber <- 'Slope s vyberem N = 100'
slope_500$vyber <- 'Slope s vyberem N = 500'
slope_1000$vyber <- 'Slope s vyberem N = 1000'
slope_2000$vyber <- 'Slope s vyberem N = 2000'
vyber_vyberu <- rbind(slope_20, slope_100, slope_500, slope_1000, slope_2000)
vyber_vyberu$vyber <- factor(vyber_vyberu$vyber, levels = c('Slope s vyberem N = 20',
                                                            'Slope s vyberem N = 100',
                                                            'Slope s vyberem N = 500',
                                                            'Slope s vyberem N = 1000',
                                                            'Slope s vyberem N = 2000'))


ggplot(vyber_vyberu, aes(slope, colour = vyber)) +
   geom_density(size = .8) +
   xlim(2.3, 3.7) +
   scale_colour_brewer("Ruzne vybery", palette = 'YlOrBr') +
   theme(panel.background = element_rect(fill = 'lightblue', color = 'black'),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank()) +
   geom_vline(aes(xintercept = mean(slope)), color = "blue", linetype = "dashed", size = .7)







