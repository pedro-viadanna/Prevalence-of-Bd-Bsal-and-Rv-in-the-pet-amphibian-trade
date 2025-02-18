library(ggplot2)
library(tidyverse)
library(binom)
library(epiR)

pos <- 1; pop <- 54
dat.m07 <- as.matrix(cbind(pos, pop))
epi.conf(dat = dat.m07, ctype = "prevalence", method = "exact", N = 277, 
         design = 1, conf.level = 0.95) * 100

pos <- 2; pop <- 127
dat.m07 <- as.matrix(cbind(pos, pop))
epi.conf(dat = dat.m07, ctype = "prevalence", method = "exact", N = 127, 
         design = 1, conf.level = 0.95) * 100

data <- data.frame(
  group = c("Salamander", "Frog"),
  est = c(1.88, 1.85),
  lower = c(0.04, 0.04),
  upper = c(10.07, 9.89)
)

# Plot
ggplot(data, aes(x = group, y = est)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "black", width = 0.3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1, color = "black", linewidth = 0.8) +
  scale_y_continuous(limits = c(0, 25)) +
  labs(x ="", y = "Estimate Prevalence of pathogens") +
  theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

ggsave("C:/Users/pedro.viadanna/OneDrive - Washington State University (email.wsu.edu)/Documents/pet trade project/article/pathogen_prevalence.svg", plot = last_plot(), device = "svg", width = 3, height = 4, units = "in")
