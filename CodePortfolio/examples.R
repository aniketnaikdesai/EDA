# Quantile----
q75 = quantile(dep_delay, 0.75) #75th percentile


# Dplyr ------- 
# * Select ------- 
sleepData <- select(msleep, name, sleep_total) #df followed by var names
# ** select neg index -------
select(msleep, -name)
# ** select :index -------
select(msleep, name:order)
# **Selecting starting with --------
select(msleep, starts_with("sl"))

# **Select additional options -------
select(msleep,ends_with("ad"))
select(msleep,contains("ad"))
select(msleep,matches("one_string|or_the_other"))#regex
select(msleep,one_of(c("one","two","three"))) #from a group of names

## *Group_by ----
group_by(flights, date) #df and variable
#you can also use n = n() to find count of rows in each group

# *Arrange ----
flights %>%
  arrange(desc(arr_delay),.by_group = TRUE)
#default ascending; need desc(variable) otherwise
#by group is required else arrange will ignore grouping
# NA is sorted to the end for local data even with desc() and depends on backend for remote

# *Tally ----
flights %>% 
  group_by(carrier, flight, dest) %>% 
  tally(sort = TRUE) %>%
  filter(n == 365)
#Tally calls a n() or sum(n) it adds a column n() to the table. 
#Tally will collapse all the other columns

# *Ranking functions ----

min_rank(c(1, 1, 2, 3))
dense_rank(c(1, 1, 2, 3))
row_number(c(1, 1, 2, 3))

# *Mutate ----
per_hour <- flights %>%
  filter(cancelled == 0) %>%
  mutate(time = hour + minute / 60)
#calculates and adds a column with the result of the calculation

# *Lead Lag ----
lag(1:10, 1)
lead(1:10, 1)
#to find values in a vector after a certain lead or lag

# *Summarize ----
summarise(by_day, 
          dep = mean(dep_delay, na.rm = TRUE),
          arr = mean(arr_delay, na.rm = TRUE))#df and the aggregate
#combination of filter and summarize with pipe
daily_delay <- by_day %>% 
  filter(!is.na(dep_delay)) %>%
  summarise(
    mean = mean(dep_delay),
    median = median(dep_delay),
    q75 = quantile(dep_delay, 0.75),
    over_15 = mean(dep_delay > 15),
    over_30 = mean(dep_delay > 30),
    over_60 = mean(dep_delay > 60)
  )

# GGPlot2 ----
ggplot(filter(per_hour, n > 30), aes(time, arr_delay)) + 
  geom_vline(xintercept = 5:24, colour = "white", size = 2) +
  geom_point()
