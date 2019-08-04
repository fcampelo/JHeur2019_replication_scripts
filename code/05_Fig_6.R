library(ggplot2)
library(CAISEr)
library(tikzDevice)

# Power curve calculations
d       <- seq(0.1, 0.3, by = 0.005)
mypower <- 0 * d # initialise with all zeroes

for (i in seq_along(d)){
  sscalc <- calc_instances(ncomparisons = 7, d = d[i], ninstances = 200, sig.level = 0.05, power.target = "mean")
  mypower[i] <- sscalc$mean.power
}

df <- data.frame(d = d, power = mypower)

ind <- which(df$d >= 0.25)[1]
xp <- df$d[ind]
yp <- df$power[ind]

tikz("../figures/FinalPower.tex", width = 6, height = 2)
ggplot(df, aes(x = d, y = mypower)) + 
  geom_point(alpha = .5) + 
  theme_minimal() + 
  xlab("$d$") + 
  ylab("Power") + 
  scale_y_continuous(breaks = 0.2 * (0:5), limits = c(0, 1)) +
  annotate(geom = "segment", x = .25, y = 0, 
           xend = .25, yend = df$power[df$d == 0.25],
           lty = 3) + 
  annotate(geom = "segment", x = min(df$d), y = df$power[df$d == 0.25], 
           xend = .25, yend = df$power[df$d == 0.25],
           lty = 3) + 
  annotate(geom = "text", x = min(df$d) + 0.02, 
           y = df$power[df$d == 0.25] + .05, 
           label = "Mean power $= 0.853$",
           size = 4) + 
  annotate(geom = "text", x = .30, 
           y = 0.75, 
           label = "$\\alpha = 0.05$",
           size = 4, hjust = 1) + 
  annotate(geom = "text", x = .30, 
           y = 0.65, 
           label = "$N = 200$",
           size = 4, hjust = 1) + 
  theme(axis.text = element_text(size = 12))
dev.off()
