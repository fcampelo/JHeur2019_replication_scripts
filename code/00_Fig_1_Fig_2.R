# F. Campelo, E. Wanner: 
# "Sample size calculations for the experimental comparison of multiple 
# algorithms on multiple problem instances."
# Submitted, Journal of Heuristics, 2019

# Generate figures 1 and 2 of the manuscript
# ============================================================================ #

library(ggplot2)
library(tikzDevice)
library(dplyr)
library(CAISEr)

# Test parameters
K      <- 2:20 # number of comparisons
alpha  <- 0.05 # significance level
pistar <- 0.9  # desired power
d      <- 0.5  # MRES

# Calculate sample sizes for Bonferroni-corrected tests as the number
# of hypotheses grows
df1 <- data.frame(Method = character(), K = numeric(),
                  N = numeric(), N.exact = numeric(),
                  stringsAsFactors = FALSE)

for (k in K){
  # Get Bonferroni results
  x <- power.t.test(delta = d, sd = 1, sig.level = alpha/k, power = pistar,
                    type = "paired", alternative = "two")
  df1 <- rbind(df1,
               data.frame(Method = "Bonferroni", K = k,
                          N = ceiling(x$n),
                          N.exact = x$n,
                          stringsAsFactors = FALSE))
}

# Calculate power of tests Holm-corrected for the mean power case,
# as the number of hypotheses grows
df2 <- data.frame(Method = character(),
                  K = numeric(),
                  r = numeric(),
                  N = numeric(),
                  Power = numeric(),
                  stringsAsFactors = FALSE)

for (k in K){
  N <- power.t.test(delta = d, sd = 1, sig.level = alpha, power = pistar,
                    type = "paired", alternative = "two")$n - 1

  p.bar <- 0
  while (p.bar < pistar){
    N <- ceiling(N + 1)
    p <- numeric(k)
    for (i in seq_along(p)){
      p[i] <- power.t.test(delta = d, sd = 1, sig.level = alpha/i, n = N,
                           type = "paired", alternative = "two")$power
    }
    p.bar <- mean(p)
  }
  df2 <- rbind(df2,
               data.frame(Method = rep("Holm", k),
                          K = rep(k, k),
                          r = 1:k,
                          N = rep(N, k),
                          Power = p,
                          stringsAsFactors = FALSE))
}

df2.agg <- df2 %>%
  group_by(K) %>%
  summarise(mean.power  = mean(Power),
            best.power  = max(Power),
            worst.power = min(Power),
            N           = first(N)) %>%
  ungroup()


df3 <- df2.agg[, c("K", "N")]
df3 <- cbind(Method = "Holm (mean power)", df3)
df3 <- rbind(df1[, 1:3], df3)


# Figure 1
tikz("../figures/HolmVsBonf.tex", width = 6, height = 2)
ggplot(df3, aes(x = K, y = N, colour = Method, shape = Method)) +
  geom_point(size = 2) +
  geom_line(alpha = 0.25) +
  scale_x_continuous(breaks = 2 * (1:10)) +
  scale_y_continuous(breaks = 5 * (9:17)) +
  xlab("Comparisons") + ylab("Required Instances") +
  theme_minimal() +
  theme(legend.position = c(.75, .2), legend.title = element_blank())
dev.off()


# Figure 2
tikz("../figures/HolmPower.tex", width = 6, height = 2.25)
ggplot(df2.agg, aes(x = K, y = mean.power)) +
  geom_line() +
  geom_line(aes(y = worst.power), col = "red", lty = 2) +
  geom_line(aes(y = best.power), col = "blue", lty = 2) +
  geom_point(data = df2, aes(x = K, y = Power), alpha = 0.2) +
  geom_text(aes(label = N), nudge_y = 0.007, size = 4) +
  ylim(.83, 1) +
  xlab("Comparisons") +
  ylab("Power") +
  #labs(title = "Mean, best and worst case power for Holm's method",
  #     subtitle = "[for $\\alpha = 0.05, \\pi^*_{mean} = 0.9, d^* = 0.5$]") +
  scale_x_continuous(breaks = 1:k, minor_breaks = NULL) +
  theme_minimal()
#  theme(text = element_text(size = 12))
dev.off()
