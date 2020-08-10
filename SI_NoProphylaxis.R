# Model A: SI (no prophylaxis)
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

model.a <- function(times, init, parms){
  
  S <- init[1]
  Is <- init[2]
  Ir <- init[3]
  
  with(as.list(parms),{
    
    # population total
    N <- S + Is + Ir
    
    # susceptibles
    dS.dt <- - beta.s * S * Is/N - beta.r * S * Ir/N + rho * Is + rho * Ir
    
    # infected with susceptible strain
    dIs.dt <- beta.s * S * Is/N - rho * Is - epsilon * Is
    
    # infected with resistant strain
    dIr.dt <- beta.r * S * Ir/N - rho * Ir + epsilon * Is
    
    # model output
    dX <- c(dS.dt, dIs.dt, dIr.dt)
    list(dX)
  })
}