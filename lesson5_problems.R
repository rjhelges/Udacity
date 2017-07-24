library(ggplot2)
data("diamonds")
str(diamonds)

ggplot(aes(x = log(price), fill = cut), data = diamonds) + geom_histogram() +
  facet_wrap(~ color)

str(diamonds)
  
ggplot(aes(table, price), data = diamonds) + geom_point(aes(color = cut)) +
  scale_x_continuous(breaks = seq(46, 80, 2), limits = c(46,80))

diamonds$volume <- with(diamonds,x*y*z)

ggplot(aes(volume, price), data = subset(diamonds, volume != 0)) + geom_point(aes(color = clarity)) +
  scale_y_log10() +
  xlim(0,quantile(diamonds$volume, 0.99))

str(pf)

pf$prop_initiated <- pf$friendships_initiated / pf$friend_count

summary(pf$prop_initiated)

pf_has_friends <- subset(pf, !is.nan(prop_initiated))
pf_has_friends$year_joined <- floor(2014 - (pf_has_friends$tenure/365))
pf_has_friends$year_joined.buckets <- cut(pf_has_friends$year_joined, c(2004, 2009, 2011, 2012, 2014))

ggplot(aes(10 * round(tenure / 10), prop_initiated), data = pf_has_friends) + 
  geom_line(aes(color = year_joined.buckets),
            stat = 'summary',
            fun.y = median)

group1 <- subset(pf_has_friends,
                 year_joined.buckets == '(2012,2014]')

summary(group1)

ggplot(aes(cut, price/carat), data = diamonds) +
  geom_jitter(aes(color = color), alpha = .5) +
  facet_wrap( ~ clarity)

