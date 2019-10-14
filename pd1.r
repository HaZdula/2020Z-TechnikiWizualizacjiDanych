#install.packages('USAboundaries')
#install.packages('elevatr')
#install.packages('ggridges')
#install.packages('elevatr')
#install.packages("extrafont")

library(USAboundaries)
library(elevatr)
library(sf)
library(ggplot2)

s <- st_geometry(us_states(states = "California"))

cali_poly <- st_cast(s, "POLYGON")
plot(cali_poly[6])
cali_p <- cali_poly[6]


#get all points from 
#Calfironia min, max
#xmin: -124.4096 ymin: 32.53416 xmax: -114.1391 ymax: 42.00925
x <- data.frame( x = 1, y = 1)

for (i in seq(-124.4096, -114.1391, 0.01)){
  for (j in seq(32.53416, 42.00925, 0.1)){
    if (st_intersects(st_point(c(i,j)), cali_p, sparse = FALSE)){
      cat(i,j)
      x <- rbind(x, c(i,j))
      }
    }
}


start_here <- 0
end_here <- 0
x2 <- data.frame(x = 1, y = 1)
for (j in seq(32.53416, 41, 0.1)){
  for (i in seq(-124.4096, -114.1391, 0.01)){
    p <- 
    if (st_intersects(st_point(c(i,j)), cali_p, sparse = FALSE)){
      start_here <- i
      break
    }
  }
  for (i in seq(-114.1391,-124.4096, -0.01)){
    end_here <- start_here
    if (st_intersects(st_point(c(i,j)), cali_p, sparse = FALSE)){
      end_here <- i
      break
    }
  }
  cat("bind", start_here, end_here)
  x2 <- rbind(x2, data.frame(x = seq(start_here, end_here, 0.0025), y = j))
}



x <- x2[-1,]
plot(x)

elev <- get_elev_point(x[,1:2], "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
elev2 <- get_elev_point(x[,1:2], "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs", "aws")
x$elev <- elev2$elevation

x$elev <- ifelse(x$elev <0, 0, x$elev)

library(dplyr)
library(ggridges)

kolory <- x %>% group_by(y) %>% summarise(col = max(elev) - min(elev))

x <- x %>% left_join(kolory, by ="y", copy = FALSE)




Cali <- ggplot(x, aes(x = x, y = y, group = y, height = elev, color = col)) +
        geom_density_ridges(stat = "identity", scale = 4, fill="black", size = 0.5) +
        scale_color_gradient2(low = "blueviolet",
                            midpoint = 2500,
                            mid = "darkcyan",
                            high = "#fde300",
                            space="Lab",
                            breaks=c(1000,2000,3000,4000),
                            limits = c(900,4100),
                            labels=c("1K Meters\n(3.3K Feet)", "2K Meters\n(6.6K Feet)", "3K Meters\n(9.8K Feet)","4K Meters\n(13.1K Feet)"),
                            name = "Elevation Difference \n @ Latitude",
                        
                            ) + 
        theme(axis.line = element_blank(),
                            axis.text.x=element_blank(),
                            axis.ticks.x=element_blank(),
                            axis.title.y=element_blank(),
                            axis.text.y=element_blank(),
                            axis.ticks.y=element_blank(),
                                
                            panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(),
                                 
                            panel.background = element_rect(fill = "black"),
                            plot.background = element_rect(fill = "black"),
                                
                            aspect.ratio = 1334/1220,
              
                            legend.key.size = unit(1.2, "cm"),
                            legend.key.width = unit(0.2,"cm") ,
                            legend.position = c(0.8,0.8),
                            legend.title = element_text(color = "white", size = 15, family = "Courier", face = "bold"),
                            legend.text = element_text(color = "white", size = 10, family = "Courier", face = "bold"),
                            legend.background = element_rect(fill = "black"),
              
              
                            
                            ) +
         xlim(-124.7,-112)
Cali

#,family = "mono"
family = "HoboStd"
          #low = "#3b0248", high = "#fde724", mid = "#fde724"
scale_color_gradient2(low = "#3f014f",
                      midpoint = 2500,
                      mid = "#2a8c8a",
                      high = "#fde300",
                      space="Lab")
