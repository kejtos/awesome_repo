library(dplyr)
# Simulace dat ------------------------------------------------------------
set.seed(155)
N <- 1e6
IQ <- rnorm(N,100,7) 
wage <- rnorm(N, 35, 2)
educ <- (runif(N,9,20)/10)+sqrt(50.5*IQ)
u <- rnorm(N,0,16)
expert <- runif(N, min=0, max=20) 
wage <- 2.5 + 1.5*educ + 2*expert +3.5*IQ +u
populace <- data.frame(wage = wage,
                       educ = educ,
                       expert = expert,
                       IQ = IQ)
# inicializace pro graf distribuce
x=seq(-20,20,by=0.1) 
norm_dens = dnorm(x)
t_ratio <- c()

#inicializace sekvence pro různý počet výběrů
sekvence <- seq(40,1000, by=10)

# for-loop pro kreslení grafu ---------------------------------------------
for (i in 1:length(sekvence)) {
  # provedeme výběr
  vyber <- dplyr::sample_n(populace, sekvence[i])
  mod1 <- lm(wage ~ educ + expert + IQ, data = vyber)
  sum1 <- summary(mod1)
  # Uložíme t-statistiku u proměnní IQ
  t_ratio[i] <- sum1$coefficients[4, 3]
  Sys.sleep(.4)
  # Vykreslíme distribuci t-statistiky
  plot(x,
       norm_dens, 
       type = "l", 
       lwd = 2, 
       xlab = "", ylab = "",
       xlim = c(-4,4),ylim=c(0,0.4),
       main = paste("Počet pozorování:",sekvence[i]))
  abline(v= c(qt(.975, 150-3-1),
              -qt(.975, 150-3-1)))
  # vykreslíme hodnotu t-statistiky v dané iteraci
  points(y=0,x=t_ratio[i])
  Sys.sleep(.4)
  
  print(i)
}

# Graf vývoje hodnoty t-statistiky s početm pozorování --------------------
# ...v případě vysokého stupně multikolinearity

plot(t_ratio, type = "l", col="blue", xlab = "počet pozorování", ylab = "t-ratio",
     main = "Vývoj t-statistiky v závislosti \n na počtu pozorování při multikolinearitě")
abline(h=1.96, lty = "dashed", col = "red")
