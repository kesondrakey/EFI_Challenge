#Code to utilize dates with hourly steps
library(tidyverse)
library(lubridate)
t <- tibble(id = c("a", "b"),
                +             date = c(199901031400, 199901031402))
t
# A tibble: 2 x 2
#  id            date
#  <chr>        <dbl>
#    1 a     199901031400
#  2 b     199901031402
 
t2 <- t %>%
  +   mutate(date = as.character(date),
  +          date = as_datetime(date, format = "%Y%m%d%H%M"),
  +          year = year(date),
  +          month = month(date),
  +          day = day(date),
  +          hour = hour(date),
  +          minute = minute(date))
  
  t2
