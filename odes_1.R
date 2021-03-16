library(deSolve)
library(ggplot2)
library(tidyverse)

# ============== TAKE ONE ==============

# These parameters are a bit arbitrary, besides b, which is the beta variable in 
# the Gintis paper. However, changing them had no effect on the round 1 results.
parameters <- c(a = 1,
                b = .98,
                c = 1,
                g = 1.1,
                s = 1)

# Similarly, these are arbitrary, just tried with some different initial values 
# to see what happened, including those around the equilibrium. Again, didn't 
# really change the fact that kappa just explodes at some step in 20-50. 
state <- c(p = 3,
           k = 40,
           n = 10,
           kappa = 0.5,
           w = 0.6,
           e = 0.9)

economy <- function(t, state, parameters){
  with(as.list(c(state, parameters)),{
  dp <- (1 - b)*(kappa - 1)
  dk <- a*(w - g*e)
  dn <- p*k*e*kappa - w*k
  dkappa <- (c - p)/s
  dw <- kappa*(35 - k)
  de <- kappa*(k - 35)
  return(list(c(dp, dk, dn, dkappa, dw, de)))
})}

times <- seq(0, 90, by=0.01)

out <- ode(y = state, times = times, func = economy, parms = parameters)
head(out)
# We see a huge explosion in kappa right at the end of the solver's runtime -- 
# no matter what initial conditions, 
tail(out)
out

# Plotting each variable as a function of time.
out %>%
  as_tibble() %>%
  ggplot(aes(x = time, y = p)) +
  geom_line() +
  theme_bw() + 
  labs(x = "Time", y = "Price")

out %>%
  as_tibble() %>%
  ggplot(aes(x = time, y = n)) +
  geom_line() +
  theme_bw() + 
  labs(x = "Time", y = "Firms")

out %>%
  as_tibble() %>%
  ggplot(aes(x = time, y = k)) +
  geom_line() +
  theme_bw() + 
  labs(x = "Time", y = "Employees")

out %>%
  as_tibble() %>%
  ggplot(aes(x = time, y = e)) +
  geom_line() +
  theme_bw() + 
  labs(x = "Time", y = "Effort")

out %>%
  as_tibble() %>%
  ggplot(aes(x = time, y = w)) +
  geom_line() +
  theme_bw() + 
  labs(x = "Time", y = "Wages")

out %>%
  as_tibble() %>%
  ggplot(aes(x = time, y = kappa)) +
  geom_line() +
  theme_bw() + 
  labs(x = "Time", y = "Kappa")

# I calculated the "equilibrium" Jacobian by hand, on my iPad, and manually 
# inputted it here. I thought this would be easier than doing so on R.

M <- matrix(c(0,0,0,0,.02,0,
              0,0,0,-1,0,0,
              0,0,0,1,0,0,
              0,1,-1.1,0,0,0,
              -1,0,0,0,0,0,
              35,-35,35,-0.1,35,0), nrow = 6, byrow = TRUE)

# These eigenvalues unfortunately were not helpful. All the real parts were 
# zero, which gives us no information on the stability/instability of the 
# system. However, we do know that experimentally, this is not stable. Not even 
# close, really.
eigen(M)


# ============== TAKE TWO ==============

parameters <- c(a = 1,
                b = .98,
                c = 1,
                g = 1.1,
                h = 1)

state <- c(p = 1,
           k = 36,
           n = 12,
           d = 100,
           s = 120,
           w = 1.1,
           e = 1)

economy <- function(t, state, parameters){
  with(as.list(c(state, parameters)),{
    dp <- (1 - b)*((d/s) - 1)
    dk <- a*(w - g*e)
    dn <- p*k*e*(d/s) - w*k
    dd <- c - p
    ds <- h*(a*(w - g*e)*e + k*(d/s)*(k - 35))
    dw <- (d/s)*(35 - k)
    de <- (d/s)*(k - 35)
    return(list(c(dp, dk, dn, dd, ds, dw, de)))
  })}

times <- seq(0, 90, by=0.01)

out <- ode(y = state, times = times, func = economy, parms = parameters)

tail(out)

out %>%
  as_tibble() %>%
  mutate(kappa = d/s) %>%
  ggplot(aes(x = time, y = kappa)) +
  geom_line() +
  theme_bw() + 
  labs(x = "Time", y = "Kappa")

out %>%
  as_tibble() %>%
  ggplot(aes(x = time, y = p)) +
  geom_line() +
  theme_bw() + 
  labs(x = "Time", y = "Price")

out %>%
  as_tibble() %>%
  ggplot(aes(x = time, y = n)) +
  geom_line() +
  theme_bw() + 
  labs(x = "Time", y = "Firms")

out %>%
  as_tibble() %>%
  ggplot(aes(x = time, y = k)) +
  geom_line() +
  theme_bw() + 
  labs(x = "Time", y = "Employees")

out %>%
  as_tibble() %>%
  ggplot(aes(x = time, y = e)) +
  geom_line() +
  theme_bw() + 
  labs(x = "Time", y = "Effort")

out %>%
  as_tibble() %>%
  ggplot(aes(x = time, y = w)) +
  geom_line() +
  theme_bw() + 
  labs(x = "Time", y = "Wages")

out %>%
  as_tibble() %>%
  ggplot(aes(x = time, y = d)) +
  geom_line() +
  theme_bw() + 
  labs(x = "Time", y = "Demand")

out %>%
  as_tibble() %>%
  ggplot(aes(x = time, y = s)) +
  geom_line() +
  theme_bw() + 
  labs(x = "Time", y = "Supply")

