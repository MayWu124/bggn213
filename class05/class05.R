#Class 05: Data visualization 
# use ggplot2 package 
library(ggplot2) # load the package
head(cars)
# all ggplots have at least 3 layers
#data + aes + geoms
ggplot(data = cars) + aes(x = speed, y = dist) +
  geom_point() +
  # geom_line() + 
   geom_smooth(method = "lm") +
  labs(title = "stopping dstance of old cars",
       x = "speed (MPH)",
       y = "stopping distance (ft)")
# ggplot is  nothe only graphic system
plot(cars$speed, cars$speed)
plot(cars)
url <- "https://bioboot.github.io/bimm143_S20/class-material/up_down_expression.txt"
genes <- read.delim(url)
head(genes)
nrow(genes)

# how many genes are up?
table(genes$State)

#what percentage of the genes are up?
# round() round up to whole number or certain digits
round(table(genes$State)/nrow(genes) *100, 3)

# make a figure
p <- ggplot(genes) + aes(x=Condition1, y=Condition2, col = State) +
  geom_point()
p

#change the color
p + scale_color_manual(values = c("blue", "grey", "red")) 

# bad color
p+ geom_point(col = "blue")

# nicer color
p + aes(color = State)

# explor the gapminder dataset
# install.packages("gapminder")
library(gapminder)
head(gapminder)

ggplot(gapminder, 
       aes(year, lifeExp, col = continent)) + 
  #geom_point(alpha = 0.4)+
  geom_jitter(width = 0.3, alpha = 0.4) +
  #geom_boxplot(alpha = 0.3, aes(group = year))
  geom_violin(aes( group = year),alpha = 0.2, draw_quantiles = 0.5)
              
# install the plotly
# install.packages("plotly")
# interactive plot
library(plotly)
#ggplotly()




