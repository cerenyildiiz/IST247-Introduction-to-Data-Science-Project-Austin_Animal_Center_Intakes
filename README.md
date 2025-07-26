# IST247-Introduction-to-Data-Science-Project-Austin_Animal_Center_Intakes
This project is the end-of-term final project of the course called Introduction to Data Science, also coded IST247.
# Graphs
```{r setup, include=TRUE}
options(repos = c(CRAN = "https://cran.rstudio.com/"))
# Gerekli kütüphaneleri yükleyin
install.packages("tinytex")
library(ggplot2)
library(dplyr)
library(ggplot2)
library(dplyr)
install.packages("tidyr")
library(tidyr)

# Veriyi yükleyin
Processed_Austin_Animal_Center_Intakes <- read.csv("C:/Users/pc/Desktop/Processed_Austin_Animal_Center_Intakes.csv")

 head(Processed_Austin_Animal_Center_Intakes)
 dim(Processed_Austin_Animal_Center_Intakes)
 colnames(Processed_Austin_Animal_Center_Intakes)
 summary(Processed_Austin_Animal_Center_Intakes)
 str(Processed_Austin_Animal_Center_Intakes)
 


# Animal Type Distribution
ggplot(Processed_Austin_Animal_Center_Intakes, aes(x = Animal.Type)) +
  geom_bar(fill = "blue", color = "black") +
  labs(title = "Animal Type Distribution", x = "Animal Type", y = "Frequency") +
  theme_minimal()







# Age Distribution in Days
ggplot(Processed_Austin_Animal_Center_Intakes, aes(x = Age.in.Days)) +
  geom_histogram(binwidth = 100, fill = "orange", color = "black") +
  labs(title = "Age Distribution (in Days)", x = "Age (Days)", y = "Frequency") +
  theme_minimal()





# Create age groups and add to dataframe
Processed_Austin_Animal_Center_Intakes$Age.Group <- cut(
  Processed_Austin_Animal_Center_Intakes$Age.in.Days,
  breaks = c(-Inf, 365, 1825, 3650, Inf),
  labels = c("Puppy/Kitten (0-1 year)", "Young (1-5 years)", "Adult (5-10 years)", "Senior (10+ years)")
)


# Plot the age group distribution
ggplot(Processed_Austin_Animal_Center_Intakes, aes(x = Age.Group)) +
  geom_bar(fill = "purple", color = "black") +
  labs(title = "Age Group Distribution", x = "Age Group", y = "Frequency") +
  theme_minimal() +
  coord_flip() # Flip for better readability




# Age Group vs Animal Type Distribution
ggplot(Processed_Austin_Animal_Center_Intakes, aes(x = Age.Group, fill = Animal.Type)) +
  geom_bar(position = "dodge") +
  labs(title = "Animal Type Distribution by Age Group", x = "Age Group", y = "Frequency") +
  theme_minimal()



# Age Group vs Sex Distribution
ggplot(Processed_Austin_Animal_Center_Intakes, aes(x = Age.Group, fill = Sex.upon.Intake)) +
  geom_bar(position = "dodge") +
  labs(title = "Sex Distribution by Age Group", x = "Age Group", y = "Frequency") +
  theme_minimal() +
  coord_flip() # Better readability




# Convert DateTime to Date
Processed_Austin_Animal_Center_Intakes$DateTime <- as.Date(Processed_Austin_Animal_Center_Intakes$DateTime)

# Time Series of Age Groups
ggplot(Processed_Austin_Animal_Center_Intakes, aes(x = DateTime, fill = Age.Group)) +
  geom_histogram(binwidth = 30, position = "fill") +
  labs(title = "Age Groups Over Time", x = "Date", y = "Proportion") +
  theme_minimal()





# Age Group vs Intake Type Distribution
ggplot(Processed_Austin_Animal_Center_Intakes, aes(x = Age.Group, fill = Intake.Type)) +
  geom_bar(position = "dodge") +
  labs(title = "Intake Type Distribution by Age Group", x = "Age Group", y = "Frequency") +
  theme_minimal() +
  coord_flip() # Better readability




# Age Group vs Intake Condition Distribution
ggplot(Processed_Austin_Animal_Center_Intakes, aes(x = Age.Group, fill = Intake.Condition)) +
  geom_bar(position = "dodge") +
  labs(title = "Intake Condition by Age Group", x = "Age Group", y = "Frequency") +
  theme_minimal() +
  coord_flip() # Better readability





# Time Series Analysis by Age Group
ggplot(Processed_Austin_Animal_Center_Intakes, aes(x = DateTime, fill = Age.Group)) +
  geom_histogram(binwidth = 30, position = "fill") +
  labs(title = "Age Group Trends Over Time", x = "Date", y = "Proportion") +
  theme_minimal()




# Age Group vs Color Distribution (Top Colors)
library(dplyr)

# Extract top 10 colors
top_colors <- Processed_Austin_Animal_Center_Intakes %>%
  count(Color, sort = TRUE) %>%
  top_n(10, n)

# Filter data by top colors
filtered_data <- Processed_Austin_Animal_Center_Intakes %>%
  filter(Color %in% top_colors$Color)




# Plot
ggplot(filtered_data, aes(x = Age.Group, fill = Color)) +
  geom_bar(position = "fill") +
  labs(title = "Color Distribution by Age Group", x = "Age Group", y = "Proportion") +
  theme_minimal() +
  coord_flip()



# Boxplot: Age by Animal Type
ggplot(Processed_Austin_Animal_Center_Intakes, aes(x = Animal.Type, y = Age.in.Days)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Age Distribution by Animal Type", x = "Animal Type", y = "Age (Days)") +
  theme_minimal()




# Boxplot: Age by Sex
ggplot(Processed_Austin_Animal_Center_Intakes, aes(x = Sex.upon.Intake, y = Age.in.Days)) +
  geom_boxplot(fill = "lightgreen", color = "black") +
  labs(title = "Age Distribution by Sex", x = "Sex upon Intake", y = "Age (Days)") +
  theme_minimal() +
  coord_flip() # Better readability




# Boxplot: Age by Intake Type
ggplot(Processed_Austin_Animal_Center_Intakes, aes(x = Intake.Type, y = Age.in.Days)) +
  geom_boxplot(fill = "orange", color = "black") +
  labs(title = "Age Distribution by Intake Type", x = "Intake Type", y = "Age (Days)") +
  theme_minimal()




# Boxplot: Age by Intake Condition
ggplot(Processed_Austin_Animal_Center_Intakes, aes(x = Intake.Condition, y = Age.in.Days)) +
  geom_boxplot(fill = "purple", color = "black") +
  labs(title = "Age Distribution by Intake Condition", x = "Intake Condition", y = "Age (Days)") +
  theme_minimal() +
  coord_flip() # Better readability





# Boxplot: Age Group by Animal Type
ggplot(Processed_Austin_Animal_Center_Intakes, aes(x = Animal.Type, y = Age.in.Days, fill = Age.Group)) +
  geom_boxplot() +
  labs(title = "Age Group Distribution by Animal Type", x = "Animal Type", y = "Age (Days)") +
  theme_minimal()








# Gerekli kütüphaneleri yükleyin
library(ggplot2)
library(dplyr)
install.packages("tidyr")
library(tidyr)
# Veriyi yükleyin
data <- read.csv("Processed_Austin_Animal_Center_Intakes.csv")

# Tarih ve yaş sütunlarını doğru formata dönüştürün
data$DateTime <- as.Date(data$DateTime, format = "%Y-%m-%d")
data$Age.in.Days <- as.numeric(data$Age.in.Days)

# Eksik veri temizleme
data <- data[!is.na(data$DateTime) & !is.na(data$Age.in.Days), ]

# Yaş Gruplarını Oluşturma
data$Age.Group <- cut(
  data$Age.in.Days,
  breaks = c(-Inf, 365, 1825, 3650, Inf),
  labels = c("Puppy/Kitten (0-1 year)", "Young (1-5 years)", "Adult (5-10 years)", "Senior (10+ years)")
)

# 1. Zaman Serisi Grafiği
monthly_data <- data %>%
  mutate(MonthYear = format(DateTime, "%Y-%m")) %>%
  group_by(MonthYear) %>%
  summarise(Count = n())

ggplot(monthly_data, aes(x = as.Date(paste0(MonthYear, "-01")), y = Count)) +
  geom_line(color = "blue") +
  labs(title = "Monthly Animal Intake Trends", x = "Month-Year", y = "Number of Intakes") +
  theme_minimal()

# 2. Korelasyon Analizi (Yaş ve Kabul Şartları)
ggplot(data, aes(x = Intake.Condition, y = Age.in.Days)) +
  geom_boxplot(fill = "orange", color = "black") +
  labs(title = "Age Distribution by Intake Condition", x = "Intake Condition", y = "Age (Days)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 3. Renk Dağılımı (Yaş Gruplarına Göre)
top_colors <- data %>%
  count(Color) %>%
  arrange(desc(n)) %>%
  slice_head(n = 10)

filtered_data <- data[data$Color %in% top_colors$Color, ]

ggplot(filtered_data, aes(x = Age.Group, fill = Color)) +
  geom_bar(position = "stack") +
  labs(title = "Top Color Distribution by Age Group", x = "Age Group", y = "Count") +
  theme_minimal() +
  coord_flip()

# 4. Yoğunluk Grafiği (Yaş Dağılımı)
ggplot(data, aes(x = Age.in.Days)) +
  geom_density(fill = "skyblue", alpha = 0.7) +
  labs(title = "Age Distribution Density", x = "Age in Days", y = "Density") +
  theme_minimal()

# 5. Zaman Serisi Yoğunluğu (Yaş Grupları Üzerinden)
data$YearMonth <- format(data$DateTime, "%Y-%m")

age_group_distribution <- data %>%
  group_by(YearMonth, Age.Group) %>%
  summarise(Count = n()) %>%
  pivot_wider(names_from = Age.Group, values_from = Count, values_fill = 0)

age_group_long <- pivot_longer(age_group_distribution, cols = -YearMonth, names_to = "Age.Group", values_to = "Count")

ggplot(age_group_long, aes(x = as.Date(paste0(YearMonth, "-01")), y = Count, fill = Age.Group)) +
  geom_area(alpha = 0.7) +
  labs(title = "Age Group Trends Over Time", x = "Month-Year", y = "Number of Intakes") +
  theme_minimal()




# Korelasyon Analizi: Yaş ve Kabul Koşulları
ggplot(data, aes(x = Intake.Condition, y = Age.in.Days)) +
  geom_boxplot(fill = "orange", color = "black") +
  labs(
    title = "Age Distribution by Intake Condition",
    x = "Intake Condition",
    y = "Age (Days)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



create_bar_plot <- function(data, x_var, fill_var, title, x_label, y_label) {
  ggplot(data, aes_string(x = x_var, fill = fill_var)) +
    geom_bar(position = "dodge") +
    labs(title = title, x = x_label, y = y_label) +
    theme_minimal()
}
view(create_bar_plot())

```
<img width="757" height="468" alt="image" src="https://github.com/user-attachments/assets/75dab26a-1540-44a3-bb91-4cbee0c20c32" />

