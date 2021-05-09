setwd("~/Desktop/Gov 94JW/scripts/brazil_politics_networks")

library(tidyverse)
library(igraph)
library(janitor)
library(utils)
library(data.table)
library(MASS)
library(stringi)


drop_large <- function(list){
  if(length(list) > 14000){
    return(NULL)
  }else{
    return(list)
  }
}


make_graph <- function(year){
  # make in and out paths
  in_path <- paste("http://dadosabertos.camara.leg.br/arquivos/proposicoesAutores/csv/proposicoesAutores-", year, ".csv", sep = "")
  content_path <- paste("http://dadosabertos.camara.leg.br/arquivos/proposicoes/csv/proposicoes-", year, ".csv", sep = "")
  out_adj_path <- paste("../data/networks/adj_", year,".csv", sep = "")
  out_graph_path <- paste("../data/networks/graph_", year, ".gml", sep = "")
  
  #read in data
  full_df <- read.csv(in_path, sep = ";")
  content_df <- read.csv(content_path, sep = ";")
  print(paste("read data for ", year, sep = ""))

  #clean content of bills
  content_df$ementa <- stri_trans_general(content_df$ementa,"Latin-ASCII")
  content_df$ementa <- str_to_lower(content_df$ementa)
  
  #get relevant data for nodes/edges
  df <- full_df %>% 
    left_join(content_df, by = c("idProposicao" = "id")) %>%
    filter(!grepl("constitucion|constituicao", ementa)) %>%
    filter(tipoAutor == "Deputado") %>%
    dplyr::select(idProposicao, idDeputadoAutor) %>%
    drop_na()

  #get edges
  find_pairs <- function(prop){
    auths <- df[df$idProposicao == prop,]$idDeputadoAutor
    if(length(auths) > 1){
      pairs <- combn(auths, 2, simplify = FALSE)
      return(pairs)
    }else{
      return(list())
    }
  }
  
  edges_temp <- lapply(unique(df$idProposicao), find_pairs)
  print(paste("gotten dyads for ", year, sep = ""))

  #drop massive bills
  edges_temp <- lapply(edges_temp, drop_large)

  # drop empty bills
  edges_temp <- Filter(length, edges_temp)
  print(mean(lengths(edges_temp)))
  
  #make into df
  edges_temp <- unlist(edges_temp, recursive = F)
  edges_df <- do.call(rbind.data.frame, edges_temp)

  # format for igraph
  colnames(edges_df) <- c("from", "to")
  edge_list <- as.data.frame(edges_df) %>% 
    group_by(from, to) %>%
    mutate(weight = n()) %>%
    distinct(from, to, .keep_all = TRUE)

  # pull meta data from file
  meta_data <- full_df[full_df$idDeputadoAutor %in% unique(df$idDeputadoAutor),] 
  
  #join meta data from reps df
  meta_data <- full_df %>%
    dplyr::select(idDeputadoAutor, tipoAutor, nomeAutor, siglaPartidoAutor) %>%
    filter(idDeputadoAutor %in% edge_list$to | idDeputadoAutor %in% edge_list$from)
  meta_data$nomeAutor <- stri_trans_general(meta_data$nomeAutor,"Latin-ASCII")
  meta_data$nomeAutor <- str_to_lower(meta_data$nomeAutor)
  meta_data <- meta_data %>%
    distinct(idDeputadoAutor, .keep_all = TRUE)



  print(paste("made metadata for ", year, sep = ""))
  

  # make out graph
  out_graph <- graph.data.frame(edge_list, directed = F, vertices = meta_data)
  out_adj <- as_adjacency_matrix(out_graph,attr = "weight",sparse = F)
  print(paste("made graph for ", year, sep = ""))
  
  
  # write adjacency matrix + graph object
  write.matrix(out_adj, out_adj_path, sep = ",")
  write.graph(out_graph, out_graph_path, format = "gml")
  
  print(paste("wrote files for ", year, sep = ""))
  
}

years <- seq(from = 2003, to = 2018, by = 1)

lapply(years, make_graph)
