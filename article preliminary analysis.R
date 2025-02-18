library(ggplot2)
library(tidyverse)
library(dunn.test)

data <- read.csv("C:/Users/pedro.viadanna/OneDrive - Washington State University (email.wsu.edu)/Documents/pet trade project/article/economic data business.csv")

mean(data$Q33_1, na.rm=TRUE)

table(data$Group, data$Q4)

p1 <- data %>% 
  select(Group, Q4) %>% 
  ggplot(aes(x = Group, fill = Q4))
p1 + geom_bar(position = "dodge",na.rm = TRUE) +
  xlab("Group") +
  ylab("Businesses number") +
  theme_classic()

p2 <- data %>% 
  select(Group, Q6) %>% 
  ggplot(aes(x = Group, fill = Q6))
p2 + geom_bar(position = "dodge",na.rm = TRUE) +
  xlab("Group") +
  ylab("Businesses number") +
  theme_classic()


contingency_table <- table(data$Group, data$Q4)
fisher_test <- fisher.test(contingency_table)
print(fisher_test)

contingency_table <- table(data$Group, data$Q6)
fisher_test <- fisher.test(contingency_table)
print(fisher_test)


data$Q4_code <- as.integer(factor(data$Q4, levels = c("Less than $5,000", "$5,000 to 50,000", "$50,001 to $200,000", "$200,001 to $1,000,000", "Over $1,000,000")))
data$Q6_code <-as.integer(factor(data$Q6, levels = c("Less than 10%", "10% to 25%", "26% to 50%", "51% to 75%", "76% to 100%")))

print(data$Q4_code)
print(data$Q6_code)


ggplot(data = data, aes(x = Q4_code, y = Q6_code, color = Group)) +
  geom_point(position = "jitter") +
  labs(x = "Q4 Code", y = "Q6 Code", color = "Group") +
  theme_minimal()

ggsave("C:/Users/pedro.viadanna/OneDrive - Washington State University (email.wsu.edu)/Documents/pet trade project/article/scatterplot_1.svg", plot = last_plot(), device = "svg", width = 6, height = 4, units = "in")


kruskal.test(data$Q4_code, data$Group)

dunn.test(data$Q4_code, data$Group, method = "bonferroni")


kruskal.test(data$Q6_code, data$Group)

dunn.test(data$Q6_code, data$Group, method = "bonferroni")

shapiro.test(data$Q4_code)

kruskal.test(data$Q33_5, data$Group)
dunn.test(data$Q33_3, data$Group, method = "bonferroni")


