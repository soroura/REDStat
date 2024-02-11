map_emr <- map_data('world')[map_data('world')$region %in% c("Egypt"),]

ggplot() +                 # first layer
  geom_polygon(data = map_data("world"), 
               aes(x=long, y=lat, group = group), 
               color = 'blue', fill = 'lightblue')+ 
  
  geom_polygon(data = map_emr,
               aes(x=long, y=lat, group = group),
               color = 'red', fill = 'pink')
