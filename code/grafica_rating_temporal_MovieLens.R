library(tidyverse)

theme_set(theme_bw(base_size = 20))

dataset <- "MovieLens"

archivo_calis <- paste0("../data/", dataset, "/ratings.csv")

folder <- paste0("../out/", dataset)

calis <- readr::read_csv(archivo_calis) %>% 
  mutate(fecha = as.Date(as.POSIXct(timestamp, origin="1970-01-01")))

(calis %>% 
    group_by(week = cut(fecha, "week")) %>% 
    summarise(cal_prom = mean(rating)) %>%
    mutate(week = as.Date(week)) %>% 
    ggplot(aes(week, cal_prom)) + 
    geom_point(size = 0.5, alpha = 0.8) + 
    #geom_smooth(method = "loess", colour="black") +
    xlab("Tiempo") +
    ylab("Calificación promedio")
) %>% 
  ggsave(., 
         file = paste0(folder, "/plots/calificaciones_promedio_semana_", dataset, ".pdf"),
         device = "pdf")