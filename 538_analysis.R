#'# A look at 538's accuracy.
#' I'm interested in how well Nate Silver's 538 model performed against the stupider approach of just averaging polls by state.
library(ggplot2)
##### Read in the data ####

silver <- read.delim("silver.txt")
npr <- read.delim("npr_results.txt")

##### Adjust for formatting differences ####
silver$State <- tolower(silver$State)
npr$State <- tolower(npr$State)

##### Merge ####
comp <- merge(silver, npr)

#' Only states with poll averages work for this comparison
comp <- subset(comp, !is.na(Obama.avg))

#'## Obama Comparison
#' Apologies to Hadley Wickham for using with()
obama_rmse_avg <- with(comp, sqrt(mean((Obama.avg-Obama)^2))) 
obama_rmse_538 <- with(comp, sqrt(mean((Obama.538-Obama)^2)))

obama_rmse_avg/obama_rmse_538

#'## Romney Comparison
romney_rmse_avg <- with(comp, sqrt(mean((Romney.avg-Romney)^2)))
romney_rmse_538 <- with(comp, sqrt(mean((Romney.538-Romney)^2)))

romney_rmse_avg/romney_rmse_538


#'## Weighted by Electoral Votes
obama_weighted_rmse_avg <- with(comp, sqrt(sum((((Obama-Obama.avg)^2)*EV))/sum(EV)))
obama_weighted_rmse_538 <- with(comp, sqrt(sum((((Obama-Obama.538)^2)*EV))/sum(EV)))

obama_weighted_rmse_avg/obama_weighted_rmse_538


romney_weighted_rmse_avg <- with(comp, sqrt(sum((((Romney-Romney.avg)^2)*EV))/sum(EV)))
romney_weighted_rmse_538 <- with(comp, sqrt(sum((((Romney-Romney.538)^2)*EV))/sum(EV)))

romney_weighted_rmse_avg/romney_weighted_rmse_538


#'## Some nice plots

#+ dev = "svg", out.width = "60%"
ggplot(comp, aes(Obama.avg-Obama, Obama.538-Obama))+
  geom_point(aes(size = EV))+
  geom_abline()+
  stat_smooth(method = lm)+
  scale_area()+
  coord_fixed()+
  theme_bw()

#' It looks like over-all, Silver outperformed averaging polls by being more generous to Obama.

#+ dev = "svg", out.width = "60%"
ggplot(comp, aes(Obama.avg, Obama))+
    geom_point(aes(size = EV))+
    geom_abline()+
    stat_smooth(method = lm)+
    theme_bw()

#' In fact, it looks like Obama out-performed the poll averages.