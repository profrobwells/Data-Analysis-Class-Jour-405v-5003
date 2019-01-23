Title: Machlis - Basic Data Visualization

#load software
pacman::p_load(ggplot2, dplyr, usethis, forcats)

#Basic demo
demo(topic="graphics")


#Tutorial
snowdata <- rio::import("data/BostonChicagoNYCSnowfalls.csv")
bostonsnow <- select(snowdata, Winter, Boston)
names(bostonsnow)[2] <- "TotalSnow"

bostonsnow <- select(snowdata, Winter, Boston) %>%
  rename(TotalSnow = Boston)

bostonsnow <- select(snowdata, Winter, TotalSnow = Boston) 

#Basic graphs
plot(bostonsnow$TotalSnow)

hist(bostonsnow$TotalSnow)
boxplot(bostonsnow$TotalSnow)
barplot(bostonsnow$TotalSnow)
barplot(sort(bostonsnow$TotalSnow, decreasing = TRUE))

#qplot
qplot(data=bostonsnow, y = TotalSnow)
qplot(y = bostonsnow$TotalSnow)

#bring in snowdata tidy
snowdata_tidy <- rio::import("data/snowdata_tidy.csv")

#ggplot
ggplot(snowdata_tidy, aes(x = Winter, y = TotalSnow, group = City, color = City)) +
  geom_line() +
  geom_point()

ggplot(data = snowdata_tidy21, aes(x = Winter, y = TotalSnow, group = City, color = City)) +
  geom_col() 

ggplot(data = snowdata_tidy21, aes(x = Winter, y = TotalSnow, group = City, fill = City)) +
  geom_col(position = "dodge") 

snippet myg_barplot_grouped
  ggplot(${1:mydataframe}, aes(${2:xcolname}, ${3:ycolname}, group = ${4:groupbycolname},
                               fill = ${4:groupbycolname})) +
  geom_col(position = "dodge")

ggplot(mydataframe, aes(xcolname, ycolname, group = groupbycolname, 
fill = groupbycolname)) +
geom_col(position = "dodge")

boston10 <- bostonsnow %>%
  top_n(10, TotalSnow) %>%
  arrange(desc(TotalSnow))

ggplot(data = boston10, aes(x = Winter, y = TotalSnow)) + 
  geom_col(fill = "rainbow") +
  theme_minimal() 

ggplot(data = boston10, aes(x = Winter, y = TotalSnow)) + 
geom_col(fill = "dodgerblue4") +
theme_minimal() +
labs(title = " XXXX Any Title Here",
subtitle = "XXXX Any subtitle", 
caption = "Source: XXXXX")

#With ordered bars
ggplot(boston10, aes(x=fct_reorder(Winter, TotalSnow), y=TotalSnow)) + 
  geom_col()

#month factor
month_factor <- factor(month.name, levels = month.name, ordered = TRUE) 
month_factor


#Adjust the axes
myplot + theme(axis.text.x = element_text(angle = 45, vjust = 0.5))


