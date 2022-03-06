#map 
armsMap <- function(nc, eur, b, p) {

  nc <- getNonCartogram()
  eur <- europeMap() %>% st_transform(crs = crsLAEA)
  e <-rmapshaper::ms_erase(eur, nc)
  b <- bbox()

  p <-
  ggplot(nc) +
  geom_sf(aes(fill = value), color = NA) +
  geom_sf(data=e, fill="grey40", color="white", size=0.2) +
  coord_sf(crs = crsLAEA, 
      xlim = c(b["xmin"], b["xmax"]), 
      ylim = c(b["ymin"], b["ymax"])) +
  scale_fill_gradientn(
    name = "quantity",
    colours = hcl.colors(n = 5, "SunsetDark", rev = T),
    limits = c(min(nc$value), max(nc$value)),
    n.breaks = 8) +
  guides(fill=guide_legend(
      direction = "horizontal",
      keyheight = unit(1.15, units = "mm"),
      keywidth = unit(15, units = "mm"),
      title.position = 'top',
      title.hjust = 0.5,
      label.hjust = .5,
      nrow =1,
      byrow = T,
      reverse = F,
      label.position = "bottom")
    ) +
  theme_minimal() +
    theme(text = element_text(family = "georg"),
      panel.background = element_blank(), 
      legend.background = element_blank(),
      legend.position = c(.45, .05),
      panel.border = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      plot.title = element_text(size=14, color="#60047a", hjust=0.5, vjust=1),
      plot.caption = element_text(size=8, color="grey60", hjust=.5, vjust=10),
      axis.title.x = element_text(size=10, color="grey20", hjust=0.5, vjust=0),
      legend.text = element_text(size=9, color="grey20"),
      legend.title = element_text(size=11, color="grey20"),
      strip.text = element_text(size=12),
      plot.margin = unit(c(t=1, r=-2, b=-1, l=-2),"lines"),
      axis.title.y = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank()) +
  labs(y="", 
      subtitle="",
      x = "",
      title="US Arms Sales (2016-2020)",
      caption="©2022 Milos Popovic https://milospopovic.net\nSource: SIPRI Arms Transfers Database, https://sipri.org/databases/armstransfers")
return(p)
}