# Model C: SEIR (no prophylaxis)
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

model.c <- function(times, init, parms){
  
  S <- init[1]
  Es <- init[2]
  Er <- init[3]
  Is <- init[4]
  Ir <- init[5]
  R <- init[6]
  
  with(as.list(parms),{
    
    # population total
    N <- S + Es + Er + Is + Ir + R
    
    # susceptibles
    dS.dt <- - beta.s * S * Is/N - beta.r * S * Ir/N + rho * R
    
    # exposed with susceptible strain
    dEs.dt <- beta.s * S * Is/N  - lambda * Es
    
    # exposed with resistant strain
    dEr.dt <- beta.r * S * Ir/N  - lambda * Er
    
    # infected with susceptible strain
    dIs.dt <- lambda * Es  - epsilon * Is - gamma * Is
    
    # infected with resistant strain
    dIr.dt <- lambda * Er  + epsilon * Is - gamma * Ir
    
    # recovereds
    dR.dt <- gamma * Is + gamma * Ir - rho * R
    
    # model output
    dX <- c(dS.dt, dEs.dt, dEr.dt, dIs.dt, dIr.dt, dR.dt)
    list(dX)
  })
}


