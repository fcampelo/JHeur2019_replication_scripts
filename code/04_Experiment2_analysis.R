# F. Campelo, E. Wanner: 
# "Sample size calculations for the experimental comparison of multiple 
# algorithms on multiple problem instances."
# Submitted, Journal of Heuristics, 2019

# analysis script for experiment number 1: Comparison of algorithm 
# configurations for the parallel machine scheduling problem with 
# setup times.
# ============================================================================ #

library(CAISEr)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(tikzDevice)
library(gridExtra)


mydata <- readRDS("../data/CAISEr_results_20190730115735.rds")

# Plot confidence intervals for pairwise comparisons
myplots <- plot(mydata, latex = TRUE, 
                show.text = FALSE, reorder = TRUE, digits = 2)

mp1 <- myplots[[1]][[1]] + 
  geom_text(aes(label = CItxt),
            nudge_x = .35, size = 2.5, col = 1) + 
  theme(axis.text = element_text(size = 8))


# Plot sample sizes/instance/algorithm
df <- myplots$dfs[[2]]
df$fill <- "black"
df$fill[df$Algorithm == "Full"] <- "blue30"
df$fill["TSK" %in% df$Algorithm] <- "red30"

mp2 <- ggplot(df, aes(x = Algorithm, y = n, group = Algorithm)) + 
  geom_boxplot(aes(fill = fill), alpha = .5, 
               outlier.shape = NA, show.legend = FALSE) + 
  geom_jitter(width = .2, alpha = .4, pch = 20, size = 1) + 
  theme_minimal() + 
  theme(axis.text = element_text(size = 8),
        axis.text.x = element_text(angle = 45, hjust = 1)) + 
  xlab("") + ylab("Runs/instance")

tikz("../figures/Final_CI_SS.tex", width = 8, height = 3)
gridExtra::grid.arrange(grobs = list(mp1, mp2), 
                        layout_matrix = rbind(c(1,1,1,2,2), c(1,1,1,2,2)))
dev.off()



# Extract results table
ignore <- capture.output(mytests <- summary(mydata)$test.info)

myres <- lapply(mytests, 
                function(x){
                  res <- data.frame(Alg1  = x$data$Alg1[1], 
                                    Alg2  = x$data$Alg2[1], 
                                    pval  = signif(x$pval, 2),
                                    est   = signif(x$test$estimate, 2),
                                    CIhw  = signif(x$test$estimate - x$test$conf.int[1], 2),
                                    dhat  = signif(mean(x$data$Phi) / sd(x$data$Phi), 2))})
myres <- do.call(rbind, myres)
rownames(myres) <- NULL
myres <- cbind(myres, Alpha = signif(mydata$samplesize.calc$sig.level, 2))
myres$Comparison <- paste(myres$Alg1, myres$Alg2, sep = " $\times$ ")
myres$CI <- paste0("$", myres$est, "\\pm ", myres$CIhw, "$")
myres$Alpha <- paste0("$", myres$Alpha, "$")
myres$pval <- paste0("$", myres$pval, "$")
myres$dhat <- paste0("$", myres$dhat, "$")
myres <- myres[, c(8, 7, 3, 9, 6)]

#library(xtable)
#xtable(myres)




# Plot standard errors and total sample sizes per instance

# Extract total runs / instance
df <- mydata$data.raw %>%
  group_by(Instance) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(Ypos = 0.075 + 0.025 * (count - min(count)) / diff(range(count)),
         Instname = sapply(as.character(Instance), sanitizeTexString),
         Instname = factor(Instname, levels = unique(Instname)))

# Generate LaTeX-friendly labels and order instances by size.
df2 <- mydata$data.summary %>%
  mutate(M = as.numeric(sapply(strsplit(as.character(Instance), split = "_"), 
                               function(y) y[2])),
         J = as.numeric(sapply(strsplit(as.character(Instance), split = "_"), 
                               function(y) y[3]))) %>%
  arrange(M, J) %>%
  mutate(Instname = sapply(as.character(Instance), sanitizeTexString),
         Instname = factor(Instname, levels = unique(Instname))) %>%
  as_tibble()

# Plot figure
ggplot(df2, aes(x = Instname, y = SE, colour = SE, group = Alg2)) + 
  # Graphical components:
  # horizontal line at 0.05
  annotate(geom = "segment", x = 0, xend = 200, y = .05, yend = .05) +
  # jittered points
  geom_jitter(alpha = .6, size = 2, pch = 20, 
              width = .35, height = 0,
              show.legend = FALSE) + 
  # line and points at sample sizes
  geom_line(data = df, 
            aes(x = Instname, y = Ypos, colour = NULL, group = NA),
            show.legend = FALSE, alpha = .2) + 
  geom_point(data = df, 
             aes(x = Instname, y = Ypos, colour = NULL, group = NA),
             show.legend = FALSE, size = .5) + 
  # Annotations and cosmetic adjustments
  annotate(geom = "text", label = "(Total runs/instance)", 
           x = 170, y = .102, 
           size = 4, hjust=0) + 
  annotate(geom = "segment", alpha = .7, 
           x = 0, xend = 201, y = .075, yend = .075) + 
  annotate(geom = "text", label = seq(220, 1100, length.out = 5), 
           x = rep(210, 5), y = seq(.075, .1, length.out = 5), 
           size = 3.5, hjust = 1, col = "grey30") + 
  xlab("") + ylab("$\\widehat{se}_{1j}$") + 
  scale_x_discrete(expand = expand_scale(mult = c(0, .05))) + 
  scale_y_continuous(breaks = c(0, .025, .05, .075, .0875, .1), 
                     labels = c("0", "0.025", "0.05", "0.075", "", "")) +
  scale_color_gradient(low = "#8888cc", high = "#882222") + 
  theme_minimal() + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12))
