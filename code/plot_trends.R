library(tidyverse)
library(gganimate)
library(tweenr)

gss_trends_cross = read_rds("output/gss_trends_cross.RDS")
gss_trends = read_rds("output/gss_trends_full.RDS")

full_plot_trust =  gss_trends %>% 
  filter(var == "trust") %>% 
  mutate(year = as.numeric(as.character(year)),
         min_year = min(year),
         max_year = max(year))




# plot_data_tween<- tween_elements(full_plot_trust, time = "year",  
#                                  group="year", ease="ease", nframes = rows)
# df_tween_appear <- tween_appear(plot_data_tween, time='day', nframes = nrow(df.us)*5)

# add pause at end of animation
#df_tween_appear<- df_tween_appear %>% keep_state(20)


make_plot <- function(i) {
  plot_df = full_plot_trust %>% 
    filter(year == i)
  
  p <- plot_df %>% 
    ggplot(aes(year, est, group = punch)) +
    #geom_point(aes(color = punch)) +
    geom_line(aes(color = punch)) +
    geom_text(data = full_plot_trust %>% 
                filter(year == min_year | year == max_year),
              aes(label = round(est, 0))) +
    theme_bw()
  
}

opt<-ani.options(interval=1/20)
saveGIF({for (i in 1972:max(full_plot_trust$year)){
  g<-make_plot(i)
  print(g)
  print(paste(i,"out of",max(full_plot_trust$year)))
  ani.pause()
}
},movie.name="output/test.gif",ani.width = 700, ani.height = 540)

make_gif <- 
  print(g)
print(paste(i,"out of",max(df_tween_appear$.frame)))
ani.pause()


full_plot_trust %>% 
  ggplot(aes(year, est, group = punch)) +
  #geom_point(aes(color = punch)) +
  geom_line(aes(color = punch)) +
  geom_text(data = full_plot_trust %>% 
              filter(year == min_year | year == max_year),
            aes(label = round(est, 0))) +
  theme_bw()



race_trust = gss_trends_cross %>% filter(subgroup == "racethn" & var == "trust")
educ_trust = gss_trends_cross %>% filter(subgroup == "educ3" & var == "trust")

plot = race_trust %>% 
  filter(cat!="Other race") %>% 
  filter(punch=="CAN TRUST") %>% 
  ggplot(aes(year, est, group = cat)) +
  geom_point(aes(color = cat)) +
  geom_line(aes(color = cat)) +
  theme_hc()
  #facet_wrap(~punch)
plot

plot = educ_trust %>% 
  ggplot(aes(year, est)) +
  geom_point(aes(color = cat)) +
  geom_line(aes(color = cat)) +
  facet_wrap(~punch)
plot

gss_trends %>% 
  filter(var=="trust") %>% 
  ggplot(aes(year, est, group = punch)) +
  geom_point(aes(color = punch)) +
  geom_line(aes(color = punch))
