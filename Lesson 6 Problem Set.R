data("diamonds")

names(diamonds)
ggplot(aes(x,price), data = diamonds) + geom_point()
cor(diamonds$price, diamonds$volume)
ggplot(aes(depth, price), data = diamonds) + 
  geom_point(alpha = 1/100) +
  scale_x_continuous(breaks = seq(0, 80, 2))

ggplot(aes(carat, price),
      data = subset(diamonds, diamonds$price < quantile(diamonds$price, 0.99) &
                      diamonds$carat < quantile(diamonds$carat, 0.99))) + 
  geom_point()

diamonds$volume = diamonds$x * diamonds$y * diamonds$z

ggplot(aes(volume, price), data = diamonds) + 
  geom_point()
cor(diamonds$price, subset(diamonds, diamonds$volume > 0 &
                             diamonds$volume < 800))

diamonds.volume <- subset(diamonds, diamonds$volume > 0 &
                            diamonds$volume < 800)    
cor(diamonds.volume$price, diamonds.volume$volume)

ggplot(aes(volume, price), data = diamonds.volume) + 
  geom_point(alpha = 1/20) +
  geom_smooth()

clarity_group <- group_by(diamonds, clarity)
diamondsByClarity <- summarise(clarity_group,
                                        mean_price = mean(price),
                                        median_price = median(price),
                                        min_price = min(price),
                                        max_price = max(price),
                                        n = n())
diamonds.diamondsByClarity <- arrange(diamonds.diamondsByClarity, clarity_group)
head(diamonds.diamondsByClarity)
