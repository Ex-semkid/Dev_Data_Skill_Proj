
if(!require (pacman)) install.packages('pacman')

p_load('tidyverse', 'janitor', 'NHANES', 'visdat', 'inspectdf')

data("NHANES")

head(NHANES)
?NHANES

str(NHANES)

plot_cat<-inspect_cat(NHANES)
show_plot(plot_cat)

nt<-
  NHANES %>%
  select(Education, Work) %>%
  drop_na() %>% 
  tabyl(Work, Education) %>% 
  gt()

nt %>% chisq.test()


n<-NHANES %>%
  select(Education, Work) %>%
  drop_na()

n %>% 
  ggplot(aes(x=Work,fill=Education))+
  geom_bar(position="dodge")

