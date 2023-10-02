# plot about fanilies over the years
make_plot <- function (data) { 
  ggplot(data, aes(x = factor(YEAR), group = YEAR)) +
    geom_bar(stat = "count", fill = "blue") +
    labs(x = "Year") +
    theme_classic()
}

# plot about payment amount over the years
make_plot_payment <- function (data) { 
  ggplot(data %>% group_by(YEAR) %>% summarize(mean_pay=mean(PAYMENT)),
         aes(x = factor(YEAR), y = mean_pay)) +
    geom_bar(stat='identity', fill = "blue") +
    labs(x = "Year", 
         y = "Average payment amount ($)") +
    theme_classic()
}

# percentage
perc <- function(curr_count, total_count) {
  curr_count / total_count * 100
}

# mean income
mean_income <- function(data) {
  mean(data$INCOME, na.rm = T)
}

# mean family size
mean_fam <- function(data) {
  mean(data$FAMILYSZ, na.rm = T)
}

# mean proportion of single parents (?) - ended up not using
mean_sing_par <- function(data) {
  count(data %>%  
    filter(SINGPAR == 1)) /
    count(data) * 100
}

# return unique states
return_state <- function(data) {
  unique(data$STATE)
}