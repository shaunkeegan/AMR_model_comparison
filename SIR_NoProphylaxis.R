# Model B: SI (no prophylaxis)
# 
# Shaun Keegan (keega.s.p@gmail.com)
# August 2020
#
# This code produces a SI compartmental model inclusive of both de novo and 
# transmitted antimicrobial resistance (AMR).

# PARAMETER KEY:
#   beta - transmission rate        lambda - becoming infectious from exposed
#   gamma - recovery rate           rho - resusceptibility
#   p - proportion of prophylaxis   epsilon - de novo emergence of AMR
#
#   Each parameter may be specified to a specific strain, e.g. beta.s would
#   represent transmission rate of the susceptible strain.


# Model Function

model.b <- function(times, init, parms){
  
  S <- init[1]
  Is <- init[2]
  Ir <- init[3]
  R <- init[4]
  
  with(as.list(parms),{
    
    # population total
    N <- S + Is + Ir
    
    # susceptibles
    dS.dt <- - beta.s * S * Is/N - beta.r * S * Ir/N + rho * R
    
    # infected with susceptible strain
    dIs.dt <- beta.s * S * Is/N  - epsilon * Is - gamma * Is
    
    # infected with resistant strain
    dIr.dt <- beta.r * S * Ir/N  + epsilon * Is - gamma * Ir
    
    # recovereds
    dR.dt <- gamma * Is + gamma * Ir - rho * R
    
    # model output
    dX <- c(dS.dt, dIs.dt, dIr.dt, dR.dt)
    list(dX)
  })
}


