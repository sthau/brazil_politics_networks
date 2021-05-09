setwd("~/Desktop/Gov 94JW/scripts/brazil_politics_networks")

library(tidyverse)
library(igraph)
library(janitor)
library(utils)


## clean 55th congress
graph_2015 <- read_graph("../data/networks/graph_2015.gml", format = "gml")
graph_2016 <- read_graph("../data/networks/graph_2016.gml", format = "gml")
graph_2017 <- read_graph("../data/networks/graph_2017.gml", format = "gml")
graph_2018 <- read_graph("../data/networks/graph_2018.gml", format = "gml")

graph_2015$name <- graph_2015$label
graph_2016$name <- graph_2016$label
graph_2017$name <- graph_2017$label
graph_2018$name <- graph_2018$label

graph_df_1 <- as_data_frame(graph_2015, "vertices") %>% mutate(year = 2015)
graph_df_2 <- as_data_frame(graph_2016, "vertices") %>% mutate(year = 2016)
graph_df_3 <- as_data_frame(graph_2017, "vertices") %>% mutate(year = 2017)
graph_df_4 <- as_data_frame(graph_2018, "vertices") %>% mutate(year = 2018)


attrs <- rbind(graph_df_1, graph_df_2, graph_df_3, graph_df_4) %>%
  dplyr::select(-id) %>%
  pivot_wider(names_from = year, 
              values_from = siglaPartidoAutor, 
              names_prefix = "party")  %>%
  filter(tipoAutor == "Deputado")

attrs %>% group_by(name) %>% filter(n()>1)
attrs[attrs$name == 4931 & attrs$nomeAutor == "izalci lucas", "party2015"] <- "PSDB"
attrs <- attrs[attrs$nomeAutor != "izalci", ]


attrs[attrs$name == 141411 & attrs$nomeAutor == "dagoberto nogueira", "party2015"] <- "PDT"
attrs[attrs$name == 141411 & attrs$nomeAutor == "dagoberto nogueira", "party2016"] <- "PDT"
attrs <- attrs[attrs$nomeAutor != "dagoberto", ]

attrs[attrs$name == 160646 & attrs$nomeAutor == "delegado francischini", "party2015"] <- "SD"
attrs[attrs$name == 160646 & attrs$nomeAutor == "delegado francischini", "party2016"] <- "SD"
attrs <- attrs[attrs$nomeAutor != "fernando francischini", ]

attrs[attrs$name == 178871 & attrs$nomeAutor == "evair vieira de melo", "party2015"] <- "PV"
attrs <- attrs[attrs$nomeAutor != "evair de melo", ]



el <- rbind(as_data_frame(graph_2015), 
            as_data_frame(graph_2016),
            as_data_frame(graph_2017), 
            as_data_frame(graph_2018)) 

graph_55 <- graph_from_data_frame(el, directed = FALSE, vertices = attrs)
write.graph(graph_55, "../data/clean_networks/graph_55.gml", format = "gml")

## clean 54th congress
graph_2011 <- read_graph("../data/networks/graph_2011.gml", format = "gml")
graph_2012 <- read_graph("../data/networks/graph_2012.gml", format = "gml")
graph_2013 <- read_graph("../data/networks/graph_2013.gml", format = "gml")
graph_2014 <- read_graph("../data/networks/graph_2014.gml", format = "gml")

graph_2011$name <- graph_2011$label
graph_2012$name <- graph_2012$label
graph_2013$name <- graph_2013$label
graph_2014$name <- graph_2014$label

el <- rbind(as_data_frame(graph_2011), 
            as_data_frame(graph_2012),
            as_data_frame(graph_2013), 
            as_data_frame(graph_2014)) 

graph_df_1 <- as_data_frame(graph_2011, "vertices") %>% mutate(year = 2011)
graph_df_2 <- as_data_frame(graph_2012, "vertices") %>% mutate(year = 2012)
graph_df_3 <- as_data_frame(graph_2013, "vertices") %>% mutate(year = 2013)
graph_df_4 <- as_data_frame(graph_2014, "vertices") %>% mutate(year = 2014)


attrs <- rbind(graph_df_1, graph_df_2, graph_df_3, graph_df_4) %>%
  dplyr::select(-id) %>%
  pivot_wider(names_from = year, 
              values_from = siglaPartidoAutor, 
              names_prefix = "party")  %>%
  filter(tipoAutor == "Deputado")

attrs %>% group_by(name) %>% filter(n()>1)


graph_54 <- graph_from_data_frame(el, directed = FALSE, vertices = attrs)
write.graph(graph_54, "../data/clean_networks/graph_54.gml", format = "gml")


## clean 53rd congress 
graph_2007 <- read_graph("../data/networks/graph_2007.gml", format = "gml")
graph_2008 <- read_graph("../data/networks/graph_2008.gml", format = "gml")
graph_2009 <- read_graph("../data/networks/graph_2009.gml", format = "gml")
graph_2010 <- read_graph("../data/networks/graph_2010.gml", format = "gml")

graph_2007$name <- graph_2007$label
graph_2008$name <- graph_2008$label
graph_2009$name <- graph_2009$label
graph_2010$name <- graph_2010$label

el <- rbind(as_data_frame(graph_2007), 
            as_data_frame(graph_2008),
            as_data_frame(graph_2009), 
            as_data_frame(graph_2010)) 

graph_df_1 <- as_data_frame(graph_2007, "vertices") %>% mutate(year = 2007)
graph_df_2 <- as_data_frame(graph_2008, "vertices") %>% mutate(year = 2008)
graph_df_3 <- as_data_frame(graph_2009, "vertices") %>% mutate(year = 2009)
graph_df_4 <- as_data_frame(graph_2010, "vertices") %>% mutate(year = 2010)


attrs <- rbind(graph_df_1, graph_df_2, graph_df_3, graph_df_4) %>%
  dplyr::select(-id) %>%
  pivot_wider(names_from = year, 
              values_from = siglaPartidoAutor, 
              names_prefix = "party") %>%
  filter(tipoAutor == "Deputado")

attrs %>% group_by(name) %>% filter(n()>1)
attrs[attrs$name == 141521 & attrs$nomeAutor == "paulo roberto pereira", "party2007"] <- "PTB"
attrs[attrs$name == 141521 & attrs$nomeAutor == "paulo roberto pereira", "party2008"] <- "PTB"
attrs <- attrs[attrs$nomeAutor != "paulo roberto", ]

attrs[attrs$name == 141376 & attrs$nomeAutor == "bel mesquita", "party2010"] <- "PMDB"
attrs <- attrs[attrs$nomeAutor != "bel mesquita e outros", ]

attrs[attrs$name == 141500 & attrs$nomeAutor == "marcos montes", "party2007"] <- "DEM"
attrs <- attrs[attrs$nomeAutor != "marcos montes - pres. da comissao de agricultura, pecuaria, abastecimento e desenvolvimento rural", ]

attrs[attrs$name == 141509 & attrs$nomeAutor == "miguel correa", "party2007"] <- "PT"
attrs <- attrs[attrs$nomeAutor != "miguel correa jr.", ]

attrs[attrs$name == 141438 & attrs$nomeAutor == "francisco praciano", "party2007"] <- "PT"
attrs <- attrs[attrs$nomeAutor != "praciano", ]

attrs[attrs$name == 141532 & attrs$nomeAutor == "rodrigo rocha loures", "party2007"] <- "PMDB"
attrs <- attrs[attrs$nomeAutor != "rocha loures", ]

in_list <- unique(c(el$to, el$from))
attr_list <- unique(attrs$name)

in_list[!(in_list %in% attr_list)]
el[el$to == 130660,]
el[el$from == 130660,]

el <- el[el$to != 130660,]


graph_53 <- graph_from_data_frame(el, directed = FALSE, vertices = attrs)
write.graph(graph_53, "../data/clean_networks/graph_53.gml", format = "gml")




## 52nd congress
graph_2003 <- read_graph("../data/networks/graph_2003.gml", format = "gml")
graph_2004 <- read_graph("../data/networks/graph_2004.gml", format = "gml")
graph_2005 <- read_graph("../data/networks/graph_2005.gml", format = "gml")
graph_2006 <- read_graph("../data/networks/graph_2006.gml", format = "gml")

graph_2003$name <- graph_2003$label
graph_2004$name <- graph_2004$label
graph_2005$name <- graph_2005$label
graph_2006$name <- graph_2006$label

el <- rbind(as_data_frame(graph_2003), 
            as_data_frame(graph_2004),
            as_data_frame(graph_2005), 
            as_data_frame(graph_2006)) 

graph_df_1 <- as_data_frame(graph_2003, "vertices") %>% mutate(year = 2003)
graph_df_2 <- as_data_frame(graph_2004, "vertices") %>% mutate(year = 2004)
graph_df_3 <- as_data_frame(graph_2005, "vertices") %>% mutate(year = 2005)
graph_df_4 <- as_data_frame(graph_2006, "vertices") %>% mutate(year = 2006)


attrs <- rbind(graph_df_1, graph_df_2, graph_df_3, graph_df_4) %>%
  dplyr::select(-id) %>%
  pivot_wider(names_from = year, 
              values_from = siglaPartidoAutor, 
              names_prefix = "party") %>%
  filter(tipoAutor == "Deputado")

attrs %>% group_by(name) %>% filter(n()>1)

attrs[attrs$name == 74700 & attrs$nomeAutor == "luiz antonio fleury", "party2003"] <- "PTB"
attrs[attrs$name == 74700 & attrs$nomeAutor == "luiz antonio fleury", "party2006"] <- "PTB"
attrs <- attrs[attrs$nomeAutor != "fleury", ]

attrs[attrs$name == 73825 & attrs$nomeAutor == "carlos nader", "party2003"] <- "PFL"
attrs <- attrs[attrs$nomeAutor != "carlos frederico theodoro nader", ]

attrs[attrs$name == 74676 & attrs$nomeAutor == "carlos rodrigues", "party2003"] <- "PL"
attrs <- attrs[attrs$nomeAutor != "bispo rodrigues", ]

attrs[attrs$name == 74544 & attrs$nomeAutor == "geddel vieira lima", "party2005"] <- "PMDB"
attrs <- attrs[attrs$nomeAutor != "geddel vieira lima - presidente da cft", ]

attrs[attrs$name == 74591 & attrs$nomeAutor == "romeu queiroz", "party2005"] <- "PTB"
attrs <- attrs[attrs$nomeAutor != "romeu queiroz - presidente da cdeic", ]

attrs[attrs$name == 73653 & attrs$nomeAutor == "wellington fagundes", "party2003"] <- "PL"
attrs[attrs$name == 73653 & attrs$nomeAutor == "wellington fagundes", "party2004"] <- "PL"
attrs <- attrs[attrs$nomeAutor != "welinton fagundes", ]

attrs[attrs$name == 74159 & attrs$nomeAutor == "odair cunha", "party2003"] <- "PT"
attrs <- attrs[attrs$nomeAutor != "odair", ]

attrs[attrs$name == 74260 & attrs$nomeAutor == "amauri gasques", "party2003"] <- "PL"
attrs <- attrs[attrs$nomeAutor != "amauri robledo gasques", ]

attrs[attrs$name == 74702 & attrs$nomeAutor == "wanderval santos", "party2003"] <- "PL"
attrs <- attrs[attrs$nomeAutor != "bispo wanderval", ]

graph_52 <- graph_from_data_frame(el, directed = FALSE, vertices = attrs)
write.graph(graph_52, "../data/clean_networks/graph_52.gml", format = "gml")






