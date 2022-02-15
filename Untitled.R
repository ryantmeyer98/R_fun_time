# to remove the row we don't want??
# bolean operator go in between
test.df <- test.df %>%
  filter(value > 0) 
  filter(value == 0)
  filter(treatment != control)
  
  test.df %>%
    filter(treatment != control) %>%
    ggplot(aes(x = time?, y = treatment)) +
    geom_boxplot() +
    geom_point()