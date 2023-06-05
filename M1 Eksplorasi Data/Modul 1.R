library(tidyverse)
ggplot2::ggplot()
ggplot2::mpg
ggplot(data=mpg)+
  geom_point(mapping=aes(x=displ,y=hwy),color="green")
ggplot(data=mpg)+
  geom_line(mapping=aes(x=displ,y=hwy),color="green")
ggplot(data=mpg,mapping=aes(x=displ,y=hwy,color=drv)) +
  geom_point() +
  geom_smooth(se=FALSE)
ggplot(data=mpg,mapping=aes(x=displ,y=hwy,color=drv)) +
  geom_point() +
  geom_smooth()
str(mpg)
??mpg
?str(mpg)
?mpg
summary(select(diamonds, x,y,z))
ggplot(diamonds) + 
  geom_histogram(mapping = aes(x = x), binwidth = 0.01)
ggplot(diamonds) + 
  geom_histogram(mapping = aes(x = y), binwidth = 0.01)
ggplot(diamonds) + 
  geom_histogram(mapping = aes(x = z), binwidth = 0.01)
filter(diamonds, x == 0 | y == 0 | z == 0) 
diamonds %>%  +
  arrange(desc(y)) %>%
  +     head() 
diamonds %>%  +
  +     arrange(desc(y)) %>% 
  +     head() # A 
ggplot(diamonds, aes(x = x, y = y)) + 
  geom_point()
