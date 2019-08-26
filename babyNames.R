# Baby names
#library(dplyr) # data munging and manipulation
getwd()
library(ggplot2) # visualize data
library(tidyr) # tidy data
library(dplyr)
library(readr)
#library(RColorBrewer) # making a colour brewer
national <- read_csv("./Data/NationalNames.csv")
dim(national)
str(national)
head(national)

agdata <- aggregate(Count ~ Year + Gender, national, sum )
head(agdata)
# Using joins.
national3 <- left_join(national,agdata,by = c("Year", "Gender"))
head(national3)
national4 <- national3 %>% mutate(rel.number = 100*Count.x/Count.y)
head(national4)

babyname <- "Jack"
selected_name <- national4 %>%
  filter(Name == babyname)
ggplot(selected_name, aes(x = Year, y= rel.number, color = Gender, group = Gender)) +
  geom_line() +
  scale_x_continuous(breaks = round(seq(1880, 2014, by = 10),1)) +
  theme_minimal()  +
  labs(title = paste("Babies Named", babyname, "Over Time"))

babyname <- "Franklin"
selected_name <- national4 %>%
  filter(Name == babyname)
ggplot(selected_name, aes(x = Year, y= rel.number, color = Gender, group = Gender)) +
  geom_line() +
  scale_x_continuous(breaks = round(seq(1880, 2014, by = 10),1)) +
  theme_minimal()  +
  geom_vline(xintercept = 1932) +
   annotate("text", x = 1980, y = .4, label = "Franklin Roosevelt elected    president of U.S in 1932") +
   labs(title = paste("Babies Named", babyname, "Over Time"))





 
babyname <- "Alexa"
selected_name <- national4 %>%
  filter(Name == babyname)
ggplot(selected_name, aes(x = Year, y = rel.number, color = Gender, group = Gender)) +
  geom_line() +
  scale_x_continuous(limits = c(1950,2015),breaks = round(seq(1950, 2014,    by = 10),1)) +
  theme_minimal()  +
  geom_vline(xintercept = 1989) +
  annotate("text", x = 1935, y = .4, label = "Billy Joel song Alexa in     1989") +
  labs(title = paste("Babies Named", babyname, "Over Time"))


babyname <- "Sharona"
selected_name <- national4 %>%
  filter(Name == babyname)
ggplot(selected_name, aes(x = Year, y= rel.number, color = Gender, group = Gender)) +
  geom_line() +
  scale_x_continuous(limits = c(1950,2015),breaks = round(seq(1950, 2014, by = 10),1)) +
  theme_minimal()  +
  geom_vline(xintercept = 1979) +
      labs(title = paste("Babies Named", babyname, "Over Time"))

