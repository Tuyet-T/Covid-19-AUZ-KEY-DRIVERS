library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)


# This data is the excess mortality during Covid-19, not due to Covid-19
data <- read_excel("Excess mortality, Australia and by state, Jan 2013 - Aug 2023.xlsx", sheet = "Table 1")
data <- data[, -c(4,7,8,9,10,11,12)]
Auz_mort <- data[15:18, ]
names(Auz_mort) <- c("Year", "Expected_deaths", "Actual_deaths","Excess_deaths","Excess_Percentage")
Auz_mort$Year <- as.factor(Auz_mort$Year)
Auz_mort$Excess_deaths<- as.numeric(Auz_mort$Excess_deaths)
dat <- read_excel("Deaths due to COVID-19 by year and month of occurrence.xlsx",sheet = "Sheet2")
dat$Year <- as.factor(dat$Year)
dat$Total <- as.numeric(dat$Total)
Death<- Auz_mort %>%
  left_join(dat, by = "Year")
Death <- Death %>%
  pivot_longer(cols = c(Excess_deaths, Total),
               names_to = "category",
               values_to = "count")

ggplot(Death, aes(x = Year, y = count, fill = category)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  labs(x = 'Year', y = 'No.of Deaths', title = 'Contribution to Excess Mortality by Covid-19') +
  scale_fill_manual(values = c('red', 'darkgreen'),
                    labels = c('Excess death','Death due to Covid_19')) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(fill = "white"))



Age_Gender <- read_excel("COVID-19 deaths by age and sex, 2020-23.xlsx", sheet = "Sheet2")
Age_Excess <- read_excel("Number of excess deaths by age group by year, Australia, 2020-2023.xlsx", sheet = "Sheet2")

Age <- merge(Age_Gender, Age_Excess, by = "Age")
Age_Gender$Age<-as.factor(Age_Gender$Age)
Age_Excess$Age<-as.factor(Age_Excess$Age)

Age_long <- Age %>%
  pivot_longer(cols = c("Covid_death", "Excess_death"), names_to = "Death_Type", values_to = "Count")


ggplot(Age_long, aes(x = Age, y = Count, fill = Death_Type)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  labs(x = 'Age', y = 'No. of Deaths', title = 'Distribution of Covid and Excess Mortality by Age') +
  scale_fill_manual(values = c('orange', 'lightcoral'),
                    labels = c('Covid Death', 'Excess Death')) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(fill = "white"))

        
#Region
Region <- read_excel("Excess mortality, Australia and by state, Jan 2013 - Aug 2023.xlsx", sheet = "Sheet1")
Region <- melt(Region, id.vars = "Year", variable.name = "Region", value.name = "Value")

ggplot(Region, aes(x = Year, y = Region, fill = Value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(min(Region$Value), max(Region$Value))) +
  labs(title = "Excess Mortality Across Region", x = "Year", y = "Region") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# The major cities takes large proportion From 2020 to 2022
Area <- read_excel("AIHW-PHE-333-demonstrating-utility-covid19-register-data-tables.xlsx", sheet = "Table S5")
selected_columns <- Area[ , c(1, 11)]
Area <- selected_columns[5:9, ]
names(Area) <- c("Area","Percentage")
Area$Percentage <- as.numeric(Area$Percentage)

ggplot(Area, aes(x = "", y = Percentage, fill = Area)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  geom_text(aes(label = ifelse(Percentage %in% sort(Percentage, decreasing = TRUE)[1:2],
                               sprintf("%.2f%%", Percentage), "")),
             position = position_stack(vjust = 0.5), 
            color = "black", size = 4) +
  labs(title = "The distribution of Covid-19 cases by remoteness area ") +
  theme_void()


Socio <- read_excel("AIHW-PHE-333-demonstrating-utility-covid19-register-data-tables.xlsx", sheet = "Table S6")
selected_columns <- Socio[, c(1, 11)]
Socio <- selected_columns[5:9, ]
names(Socio) <- c("Socioeconomics_group", "Percentage")
Socio$Percentage <- as.numeric(Socio$Percentage)
Socio$Socioeconomics_group <- factor(Socio$Socioeconomics_group)

ggplot(Socio, aes(x = "", y = Percentage, fill = Socioeconomics_group)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  geom_text(aes(label = sprintf("%.2f%%", Percentage)),
            position = position_stack(vjust = 0.5), 
            color = "black", size = 4) +
  labs(title = "The distribution of Covid-19 cases by socioeconomic group") +
  theme_void()

Cause <- read_excel("Most common underlying cause in COVID-19 related deaths.xlsx", sheet = "Sheet1")
selected_columns <- Cause[ , c(1, 6)]
Cause<- selected_columns[2:9, ]
names(Cause) <- c("Cause", "No_of_Covid19_deaths_associated")
Cause$No_of_Covid19_deaths_associated <- as.numeric(Cause$No_of_Covid19_deaths_associated)
# View(Cause)
ggplot(Cause, aes(x = Cause, y = No_of_Covid19_deaths_associated, fill = Area)) +
  geom_bar(stat = "identity", fill = "orange", alpha = 0.7) +
  labs(title = "Common Underlying Cause in Covid 19 Related Deaths",
       x = "Cause",
       y = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        panel.background = element_rect(fill = "white"))


# I NEED THE GRAPH SHOWS THAT DIES VS COMORBIDITIES VS COVID19 DEATH
Comorbidities <- read_excel("Pre-existing chronic conditions certified with COVID-19 deaths.xlsx", sheet = "Sheet1")
Comorbidities <- Comorbidities[13:14, ]
Comorbidities <- Comorbidities[, -ncol(Comorbidities)]
names(Comorbidities) <- c("Category","2020","2021","2022","2023")
Comorbidities <- Comorbidities %>%
  mutate(across(`2020`:`2023`, as.numeric)) 
Comorbidities <- Comorbidities %>%
  pivot_longer(cols = `2020`:`2023`, names_to = "Year", values_to = "Deaths")

ggplot(Comorbidities, aes(x = Year, y = Deaths, fill = Category)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "COVID-19 Deaths with Pre-exsiting Chronic Conditions",
       x = "Year", y = "Number of Deaths") +
  theme_minimal() +
  theme(legend.position = "bottom")


