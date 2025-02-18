library(tidyverse)
library(ggpubr)
library(scatterpie)


df <- read_csv("TradeSurveillance.csv")

# Graph of size
df %>% 
  mutate(`Gross annual sales` = factor(`Gross annual sales`, 
                                       levels = c("< $5,000", "$5,000 to 50,000", "$50,001 to $200,000", "$200,001 to $1,000,000", "> $1,000,000"))) %>% 
  group_by(`Gross annual sales`, `Percentage amphibian sales`, Kit) %>% 
  summarise(N=n()) %>%  
  ungroup() %>% 
  ggplot(., aes(x=`Gross annual sales`, y=`Percentage amphibian sales`, 
                fill = Kit, size=N)) +
  geom_point(pch=21, alpha=2/3) + 
  scale_size_area(max_size = 9) + 
  scale_fill_manual(values = c("black", "white")) + 
  labs(y="Percentage of sales from amphibians") +
  theme_bw() +
  theme(axis.text.x = element_text(hjust=0, angle = -25)) 
  
ggsave("Sales.pdf", height = 3.5, width=5)


# Graph of source of animals
source <- df %>% 
  select(!"From: Other") %>% 
  pivot_longer(starts_with("From: "), 
               values_to = "Percent", 
               names_to = "Source", 
               names_prefix = "From: ") %>% 
  mutate(Source = factor(Source, levels = c("Breeding", "Hobbyists", "Retail pet stores", "Wholesale/distributors",   "Imports", "Wild-caught"))) %>% 
  ggplot(., aes(x=Percent, fill = Kit)) + 
  geom_histogram(position="identity", bins = 21, alpha = 2/3, color="black") + 
  facet_grid(Source ~ ., scales="free_y", space="free_y") +
  scale_fill_manual(values = c("black", "white")) +
  scale_y_continuous("Number of participating facilities", breaks = c(5*(0:5)), minor_breaks = 0:25) + 
 labs(title="Recieve animals from", x="Percent of animals recieved") +
  theme_bw()
  
# Graph of destination of animals
destination <- df %>% 
  select(!"Sell to: Other") %>% 
  pivot_longer(starts_with("Sell to: "), 
               values_to = "Percent", 
               names_to = "Destination", 
               names_prefix = "Sell to: ") %>% 
  mutate(Destination = factor(Destination, levels = c("Pet owners", "Hobbyists", "Retail pet stores",   "Wholesale/distributors", "Exports"))) %>%
  ggplot(., aes(x=Percent, fill = Kit)) + 
  geom_histogram(position="identity", bins = 21, alpha = 2/3, color="black") + 
  facet_grid(Destination ~ ., scales="free_y", space="free_y") +
  scale_fill_manual(values = c("black", "white")) +
  scale_y_continuous("Number of participating facilities", breaks = c(5*(0:5)), minor_breaks = 0:25) + 
  labs(title="Sell animals to", x="Percent of animals sold") +
  theme_bw()

# put together
ggarrange(source, destination, ncol=2, common.legend = TRUE, legend = "bottom")

ggsave("SourceDesignation.pdf", height = 6, width=5)

# Graph of glove use
df_sum <- df %>% 
  filter(Kit == "Returned") %>% 
  group_by(Quarantine, Gloves) %>% 
  summarise(N=n(), 
            nHandling=sum(Handling=="Yes"&Feeding=="No"&Cleaning=="No"),
            nFeeding=sum(Handling=="No"&Feeding=="Yes"&Cleaning=="No"),
            nCleaning=sum(Handling=="No"&Feeding=="No"&Cleaning=="Yes"),
            #nHandlingFeeding=sum(Handling=="Yes"&Feeding=="Yes"&Cleaning=="No"),
            `Handling & Cleaning`=sum(Handling=="Yes"&Feeding=="No"&Cleaning=="Yes"),
            #nFeedingCleaning=sum(Handling=="No"&Feeding=="Yes"&Cleaning=="Yes"), 
            `Handling, Feeding, & Cleaning`=sum(Handling=="Yes"&Feeding=="Yes"&Cleaning=="Yes"), 
            Never=N-nHandling-nFeeding-nCleaning-`Handling & Cleaning`-`Handling, Feeding, & Cleaning`) %>% 
  mutate(Quarantine= if_else(Quarantine=="Yes", 1,0), 
         Gloves = if_else(Gloves=="Yes", 1, 0), 
         Handling = nHandling, 
         Feeding=nFeeding,
         Cleaning=nCleaning)

ggplot() + 
  geom_scatterpie(aes(Quarantine, Gloves, r = sqrt(N)/16), data=df_sum, 
                  cols=c("Handling", "Feeding", "Cleaning", "Handling & Cleaning",
                         "Handling, Feeding, & Cleaning", "Never")) +
  annotate(geom = "text", x=c(-0.04, 0, 0.04), y=c(1.12, 0.92, 1.12), label=c("1","6","1"), color="white") + 
  annotate(geom = "text", x=c(0.96, 0.92, 1.05, 1.08), y=c(1.1, 0.96, 0.92, 1.05), label=c("1","2","1", "2"), color="white") + 
  scale_fill_manual("Gloves\nused when", values = c("#8c96c6", "#b3cde3", "#8856a7", "#810f7c", "black")) + 
  annotate(geom = "text", x=1, y=0, label="1", color="white") + 
  scale_x_continuous("Quarantine new animals?", breaks = 0:1, labels = c("No", "Yes")) + 
  scale_y_continuous("Use gloves?", breaks = 0:1, labels = c("No", "Yes")) +
  coord_equal()+
  theme_bw()
  
ggsave("QuarantineGloves.pdf", height=3, width=5)
