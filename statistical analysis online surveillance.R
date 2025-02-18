library(ggplot2)
library(tidyverse)
library(dunn.test)
library(dplyr)


data <- read.csv("C:/Users/pedro.viadanna/OneDrive - Washington State University (email.wsu.edu)/Documents/pet trade project/article/online surveillance data without excluded.csv")

#some table to test if it is working

table(data$Are.you.willing.to.share.the.following.information.about.your.business..provided.that.it.remains.confidential....Economics..e.g...broad.categories.of.volume. , data$status)

p1 <- data %>% 
  select(status, gross.annual.sales.of.your.business) %>% 
  ggplot(aes(x = status, fill = gross.annual.sales.of.your.business))
p1 + geom_bar(position = "dodge",na.rm = TRUE) +
  xlab("Group") +
  ylab("Businesses number") +
  theme_classic()

p2 <- data %>% 
  select(status, percentage.of.annual.sales.for.amphibians) %>% 
  ggplot(aes(x = status, fill = percentage.of.annual.sales.for.amphibians))
p2 + geom_bar(position = "dodge",na.rm = TRUE) +
  xlab("Group") +
  ylab("Businesses number") +
  theme_classic()

#fisher test section

contingency_table <- table(data$status, data$gross.annual.sales.of.your.business)
fisher_test <- fisher.test(contingency_table)
print(fisher_test)

contingency_table <- table(data$status, data$percentage.of.annual.sales.for.amphibians)
fisher_test <- fisher.test(contingency_table)
print(fisher_test)

contingency_table <- table(data$status, data$percentage.of.the.amphibians...Breeding.in.house )
fisher_test <- fisher.test(contingency_table)
print(fisher_test)

contingency_table <- table(data$status, data$percentage.of.the.amphibians...Hobbyists..sales.or.trades. )
fisher_test <- fisher.test(contingency_table)
print(fisher_test)

contingency_table <- table(data$status, data$percentage.of.the.amphibians...Retail.pet.stores )
fisher_test <- fisher.test(contingency_table)
print(fisher_test)

contingency_table <- table(data$status, data$percentage.of.the.amphibians...Wholesaler.distributors )
fisher_test <- fisher.test(contingency_table)
print(fisher_test)

contingency_table <- table(data$status, data$percentage.of.the.amphibians...Imports )
fisher_test <- fisher.test(contingency_table)
print(fisher_test)

contingency_table <- table(data$status, data$percentage.of.the.amphibians...Wild.caught )
fisher_test <- fisher.test(contingency_table)
print(fisher_test)

contingency_table <- table(data$status, data$percentage.of.the.amphibians...Other..please.list. )
fisher_test <- fisher.test(contingency_table)
print(fisher_test)

contingency_table <- table(data$status, data$percentage.you.sell.to...Pet.owners..directly )
fisher_test <- fisher.test(contingency_table)
print(fisher_test)

contingency_table <- table(data$status, data$percentage.you.sell.to...Retail.pet.stores )
fisher_test <- fisher.test(contingency_table)
print(fisher_test)

contingency_table <- table(data$status, data$percentage.you.sell.to...Wholesale.distributor )
fisher_test <- fisher.test(contingency_table)
print(fisher_test)

contingency_table <- table(data$status, data$percentage.you.sell.to...Exported.to.other.countries )
fisher_test <- fisher.test(contingency_table)
print(fisher_test)

contingency_table <- table(data$status, data$percentage.you.sell.to...Hobbyists )
fisher_test <- fisher.test(contingency_table)
print(fisher_test)

contingency_table <- table(data$status, data$Did.your.business.participate.in.the.pilot.amphibian.project )
fisher_test <- fisher.test(contingency_table)
print(fisher_test)


contingency_table <- table(data$status, data$How.frequently.are.you.willing.to.collect.samples.for.this.project. )
fisher_test <- fisher.test(contingency_table)
print(fisher_test)


contingency_table <- table(data$status, data$Which.type.of.samples.are.you.willing.to.collect.....Shipments.arriving.at.my.business )
fisher_test <- fisher.test(contingency_table)
print(fisher_test)

contingency_table <- table(data$status, data$Which.type.of.samples.are.you.willing.to.collect....Shipments.departing.from.my.business )
fisher_test <- fisher.test(contingency_table)
print(fisher_test)

contingency_table <- table(data$status, data$Which.type.of.samples.are.you.willing.to.collect....Animals.within.my.business )
fisher_test <- fisher.test(contingency_table)
print(fisher_test)

contingency_table <- table(data$status, data$What.is.the.approximate.total.number.of.independent.amphibian.habitats.in.your.business. )
fisher_test <- fisher.test(contingency_table)
print(fisher_test)

contingency_table <- table(data$status, data$Aquatic )
fisher_test <- fisher.test(contingency_table)
print(fisher_test)

contingency_table <- table(data$status, data$Partially )
fisher_test <- fisher.test(contingency_table)
print(fisher_test)

contingency_table <- table(data$status, data$Terrestrial )
fisher_test <- fisher.test(contingency_table)
print(fisher_test)

contingency_table <- table(data$status, data$Are.you.willing.to.share.the.following.information.about.your.business..provided.that.it.remains.confidential....Basic.operations..e.g...biosecurity.practices. )
fisher_test <- fisher.test(contingency_table)
print(fisher_test)

contingency_table <- table(data$status, data$Are.you.willing.to.share.the.following.information.about.your.business..provided.that.it.remains.confidential....Economics..e.g...broad.categories.of.volume. )
fisher_test <- fisher.test(contingency_table)
print(fisher_test)

#t-test section
str(data)

data$status <- as.factor(data$status)

t_test_result <- t.test(Duration.to.fill.up.online.form..minutes. ~ status, data = data)

print(t_test_result)



str(data)

data$status <- as.factor(data$status)

t_test_result <- t.test(percentage.of.the.amphibians...Hobbyists..sales.or.trades. ~ status, data = data)

print(t_test_result)


str(data)

data$status <- as.factor(data$status)

t_test_result <- t.test(Terrestrial ~ status, data = data)

print(t_test_result)

percentage.of.the.amphibians...Imports
percentage.of.the.amphibians...Wild.caught
percentage.of.the.amphibians...Other..please.list.
percentage.you.sell.to...Pet.owners..directly
percentage.you.sell.to...Retail.pet.stores
percentage.you.sell.to...Wholesale.distributor
percentage.you.sell.to...Exported.to.other.countries
percentage.you.sell.to...Hobbyists
What.is.the.approximate.total.number.of.independent.amphibian.habitats.in.your.business.
Aquatic
Partially
Terrestrial

##########################


data1 <- c(13, 5, 50, 10, 4, 12, 4, 40, 4, 4, 40, 3, 10, 2, 2, 2, 50, 4, 8, 8, 12, 10, 25, 13, 2)
data2 <- c(50, 6, 24, 15, 4, 4, 4, 20, 6, 134, 2, 2, 2, 2, 2)

result <- wilcox.test(data1, data2)

print(result)

############plots for testing###################

ggplot(data, aes(x = status, y = Duration.to.fill.up.online.form..minutes.)) +
  geom_point() +
  labs(
    title = "Boxplot of Duration to Fill Up Online Form",
    x = "Status",
    y = "Duration to Fill Up Online Form (minutes)"
  ) +
  theme_minimal()

ggplot(data, aes(x = status, y = Duration.to.fill.up.online.form..minutes.)) +
  geom_violin(fill = "blue", alpha = 0.5) +
  labs(
    title = "Violin Plot of Duration to Fill Up Online Form",
    x = "Status",
    y = "Duration to Fill Up Online Form (minutes)"
  ) +
  theme_minimal()

ggplot(data, aes(x = status, y = Duration.to.fill.up.online.form..minutes.)) +
  geom_boxplot() +
  labs(
    title = "Boxplot of Duration to Fill Up Online Form",
    x = "Status",
    y = "Duration to Fill Up Online Form (minutes)"
  ) +
  theme_minimal()


######################################

# plotting 2 categorical values together



data$gross.annual.sales.of.your.business <- as.integer(factor(data$gross.annual.sales.of.your.business, levels = c("Less than $5,000", "$5,000 to 50,000", "$50,001 to $200,000", "$200,001 to $1,000,000", "Over $1,000,000")))
data$percentage.of.annual.sales.for.amphibians <- as.integer(factor(data$percentage.of.annual.sales.for.amphibians, levels = c("Less than 10%", "10% to 25%", "26% to 50%", "51% to 75%", "76% to 100%")))

print(data$gross.annual.sales.of.your.business)
print(data$percentage.of.annual.sales.for.amphibians)

data <- data %>%
  group_by(gross.annual.sales.of.your.business, status) %>%
  mutate(vertical_offset = row_number() * 0.1) %>%
  ungroup()

ggplot(data = data, aes(x = gross.annual.sales.of.your.business, y = percentage.of.annual.sales.for.amphibians + vertical_offset, color = status)) +
  geom_point(position = position_dodge(width = 0.5)) +
  labs(x = "Gross Annual Sales of Your Business", y = "% of Annual Sales for Amphibians", color = "Status") +
  theme_minimal() +
  theme(
    axis.line = element_line(color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
ggsave("C:/Users/pedro.viadanna/OneDrive - Washington State University (email.wsu.edu)/Documents/pet trade project/article/scatterplot_article.svg", plot = last_plot(), device = "svg", width = 6, height = 4, units = "in")

###################################################

#Just some fun trying to learn some other way of analysing

by(data$Duration.to.fill.up.online.form..minutes., data$status, summary)


boxplot(data$Duration.to.fill.up.online.form..minutes.~data$status, notch = TRUE, col = c("grey", "gold"), main= "duration to fill up online form")

library(sm)

sm.density.compare(data$Duration.to.fill.up.online.form..minutes., data$status, ylab= "time", xlab="status")

xtabs(~ data$gross.annual.sales.of.your.business + data$percentage.of.annual.sales.for.amphibians + data$status)

plot(xtabs(~ data$gross.annual.sales.of.your.business + data$percentage.of.annual.sales.for.amphibians + data$status), main="gross income and completing test"
)

library(gmodels)

CrossTable(data$status, data$gross.annual.sales.of.your.business, chisq = TRUE, prop.t = F)
CrossTable(data$status, data$percentage.of.annual.sales.for.amphibians, chisq = TRUE, prop.t = F)

CrossTable(data$status, data$gross.annual.sales.of.your.business+data$percentage.of.annual.sales.for.amphibians, chisq = TRUE, prop.t = F)
############################################################################


