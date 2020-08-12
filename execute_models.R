# execute model a - f


# Load packages 
library(deSolve)

# Set WD
setwd("~/OneDrive - University of Glasgow/research/current/AMR_denovo_transmitted_model_compare/AMR_model_comparison")

# Load function 
source("SI_NoProphylaxis.R")
source("SIR_NoProphylaxis.R")

# model a

x <- c(98,1,1)
parms <- c(beta.s = 0.05,beta.r = 0.05, rho = 0.01, epsilon = 0.001)
times <- seq(0, 2000, 1)


out <- as.data.frame(ode(x, times, model.a, parms))

head(out)
tail(out)

plot(out[,2] ~ out[,1], ylim = c(0,120), type = "l", lwd ="3", col = "darkgreen",
    main = "SI (no prophylaxis)", xlab = "Time", ylab = "N")
lines(out[,3] ~ out[,1], col = "red", lwd = 3)
lines(out[,4] ~ out[,1], col = "orange", lwd = 3)
legend("topright", c("Susceptible","Infected (susceptible strain)","Infected (resistant strain)"), col = c("darkgreen", "red", "orange"), lty = 1, lwd = 3)

mod.a.resistant <- (out[max(times),4])
prop.mod.a.res <- (out[max(times),4])/ ((out[max(times),3]) + (out[max(times),4]))

# model b

x <- c(98,1,1,0)
parms <- c(beta.s = 0.05,beta.r = 0.05, rho = 0.01, epsilon = 0.001, gamma = 0.01)
times <- seq(0, 2000, 1)


out <- as.data.frame(ode(x, times, model.b, parms))

head(out)
tail(out)

plot(out[,2] ~ out[,1], ylim = c(0,120), type = "l", lwd ="3", col = "darkgreen",
     main = "SIR (no prophylaxis)", xlab = "Time", ylab = "N")
lines(out[,3] ~ out[,1], col = "red", lwd = 3)
lines(out[,4] ~ out[,1], col = "orange", lwd = 3)
lines(out[,5] ~ out[,1], col = "pink", lwd = 3)
legend("topright", c("Susceptible","Infected (susceptible strain)","Infected (resistant strain)", "Recovered"), col = c("darkgreen", "red", "orange", "pink"), lty = 1, lwd = 3)


mod.b.resistant <- (out[max(times),4])
prop.mod.b.res <- (out[max(times),4])/ ((out[max(times),3]) + (out[max(times),4]))

# model c

x <- c(98,0,0,1,1,0)
parms <- c(beta.s = 0.05,beta.r = 0.05, rho = 0.01, epsilon = 0.001, gamma = 0.01, lambda = 0.05)
times <- seq(0, 2000, 1)


out <- as.data.frame(ode(x, times, model.c, parms))

head(out)
tail(out)

plot(out[,2] ~ out[,1], ylim = c(0,120), type = "l", lwd ="3", col = "darkgreen",
     main = "SEIR (no prophylaxis)", xlab = "Time", ylab = "N")
lines(out[,3] ~ out[,1], col = "lightblue", lwd = 3)
lines(out[,4] ~ out[,1], col = "blue", lwd = 3)
lines(out[,5] ~ out[,1], col = "red", lwd = 3)
lines(out[,6] ~ out[,1], col = "orange", lwd = 3)
lines(out[,7] ~ out[,1], col = "pink", lwd = 3)
legend("topright", c("Susceptible","Exposed (susceptible strain)","Exposed (resistant strain)","Infected (susceptible strain)","Infected (resistant strain)", "Recovered"), col = c("darkgreen","lightblue","blue", "red", "orange", "pink"), lty = 1, lwd = 3)

mod.c.resistant <- (out[max(times),6])
prop.mod.c.res <- (out[max(times),6])/ ((out[max(times),5]) + (out[max(times),6]))


# comparison plots between a, b, c

final.tally <- c(mod.a.resistant, mod.b.resistant, mod.c.resistant)
xlabs <- c("SI", "SIR", "SEIR")
barplot(final.tally, names.arg = xlabs, col = c("green", "blue","red"), ylim = c(0,90))


# preparatory/test code for all 6 models
par(mfrow=c(2,1))
par(mar = c(4, 4, 0.1, 0.1))
test <- c(final.tally, final.tally)
xlabs <- c("SI", "SIR", "SEIR", "SI", "SIR", "SEIR")
barplot(test, col = c("mediumslateblue", "mediumslateblue","mediumslateblue", "mediumspringgreen","mediumspringgreen", "mediumspringgreen"), ylim = c(0,90), 
         ylab = "Cases of AMR Infection")
legend("topright", c("No Prophylaxis","Prophylaxis"), col = c("mediumslateblue", "mediumspringgreen"), pch = 15, pt.cex = 2, bty = "n")

final.tally.pc <- c(prop.mod.a.res, prop.mod.b.res, prop.mod.c.res)
test2 <- c(final.tally.pc, final.tally.pc)
xlabs <- c("SI", "SIR", "SEIR", "SI", "SIR", "SEIR")
barplot(test2*100, names.arg = xlabs, col = c("mediumslateblue", "mediumslateblue","mediumslateblue", "mediumspringgreen","mediumspringgreen", "mediumspringgreen"), ylim = c(85,100), 
        xlab = "Model Type", ylab = "Percentage of Infection that is AMR (%)", cex.names = 1.3, xpd = FALSE)

