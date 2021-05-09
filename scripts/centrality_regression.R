setwd("~/Desktop/Gov 94JW/scripts/brazil_politics_networks")

library(tidyverse)
library(igraph)
library(janitor)
library(utils)
library(data.table)
library(kableExtra)
library(stargazer)
library(estimatr)

get_bills <- function(congress){
  if(congress == 52){
    years <- seq(from = 2003, to = 2006, by = 1)
  }else if(congress == 53){
    years <- seq(from = 2007, to = 2010, by = 1)
  } else if(congress == 54){
    years <- seq(from = 2011, to = 2014, by = 1)
  } else if(congress == 55){
    years <- seq(from = 2015, to = 2018, by = 1)
  }
  
  bill_path_1 <- paste("http://dadosabertos.camara.leg.br/arquivos/proposicoesAutores/csv/proposicoesAutores-", years[1], ".csv", sep = "")
  bill_path_2 <- paste("http://dadosabertos.camara.leg.br/arquivos/proposicoesAutores/csv/proposicoesAutores-", years[2], ".csv", sep = "")
  bill_path_3 <- paste("http://dadosabertos.camara.leg.br/arquivos/proposicoesAutores/csv/proposicoesAutores-", years[3], ".csv", sep = "")
  bill_path_4 <- paste("http://dadosabertos.camara.leg.br/arquivos/proposicoesAutores/csv/proposicoesAutores-", years[4], ".csv", sep = "")

  
  content_path_1 <- paste("http://dadosabertos.camara.leg.br/arquivos/proposicoes/csv/proposicoes-", years[1], ".csv", sep = "")
  content_path_2 <- paste("http://dadosabertos.camara.leg.br/arquivos/proposicoes/csv/proposicoes-", years[2], ".csv", sep = "")
  content_path_3 <- paste("http://dadosabertos.camara.leg.br/arquivos/proposicoes/csv/proposicoes-", years[3], ".csv", sep = "")
  content_path_4 <- paste("http://dadosabertos.camara.leg.br/arquivos/proposicoes/csv/proposicoes-", years[4], ".csv", sep = "")
  
  bills1 <- read.csv(bill_path_1, sep = ";")
  bills2 <- read.csv(bill_path_2, sep = ";")
  bills3 <- read.csv(bill_path_3, sep = ";")
  bills4 <- read.csv(bill_path_4, sep = ";")
  
  
  
  content1 <- read.csv(content_path_1, sep = ";")
  content2 <- read.csv(content_path_2, sep = ";")
  content3 <- read.csv(content_path_3, sep = ";")
  content4 <- read.csv(content_path_4, sep = ";")
  
  df1 <- bills1 %>% 
    left_join(content1, by = c("idProposicao" = "id")) %>%
    filter(!grepl("constitucion|constituicao", ementa)) %>%
    filter(tipoAutor == "Deputado") %>% 
    group_by(idProposicao) %>% 
    filter(n()<171) %>%
    filter(n()>1)
  
  df2 <- bills2 %>% 
    left_join(content2, by = c("idProposicao" = "id")) %>%
    filter(!grepl("constitucion|constituicao", ementa)) %>%
    filter(tipoAutor == "Deputado") %>% 
    group_by(idProposicao) %>% 
    filter(n()<171) %>%
    filter(n()>1)
  
  df3 <- bills3 %>% 
    left_join(content3, by = c("idProposicao" = "id")) %>%
    filter(!grepl("constitucion|constituicao", ementa)) %>%
    filter(tipoAutor == "Deputado") %>% 
    group_by(idProposicao) %>% 
    filter(n()<171) %>%
    filter(n()>1)
  
  df4 <- bills4 %>% 
    left_join(content4, by = c("idProposicao" = "id")) %>%
    filter(!grepl("constitucion|constituicao", ementa)) %>%
    filter(tipoAutor == "Deputado") %>% 
    group_by(idProposicao) %>% 
    filter(n()<171) %>%
    filter(n()>1)
  
  full_df <- rbind(df1, df2, df3, df4)
  
  return(full_df)
  
}



## load graphs
graph_55 <- simplify(read_graph("../data/clean_networks/graph_55.gml", format = "gml"),  
                     edge.attr.comb=list(weight="sum"))
graph_54 <- simplify(read_graph("../data/clean_networks/graph_54.gml", format = "gml"),  
                     edge.attr.comb=list(weight="sum")) 
graph_53 <- simplify(read_graph("../data/clean_networks/graph_53.gml", format = "gml"),  
                     edge.attr.comb=list(weight="sum")) 
graph_52 <- simplify(read_graph("../data/clean_networks/graph_52.gml", format = "gml"),  
                     edge.attr.comb=list(weight="sum")) 

## make centrality dataframes
centr55 <- cbind.data.frame(V(graph_55)$name, 
                            degree(graph_55)/max(degree(graph_55)), 
                            strength(graph_55)/max(strength(graph_55)), 
                            eigen_centrality(graph_55)$vector) %>% 
  mutate(congress = 55)
colnames(centr55) <- c("id", "degree", "collab", "eigen", "congress")


centr54 <- cbind.data.frame(V(graph_54)$name, 
                            degree(graph_54)/max(degree(graph_54)), 
                            strength(graph_54)/max(strength(graph_54)), 
                            eigen_centrality(graph_54)$vector) %>% 
  mutate(congress = 54)
colnames(centr54) <- c("id", "degree", "collab", "eigen", "congress")


centr53 <- cbind.data.frame(V(graph_53)$name, 
                            degree(graph_53)/max(degree(graph_53)), 
                            strength(graph_53)/max(strength(graph_53)), 
                            eigen_centrality(graph_53)$vector) %>% 
  mutate(congress = 53)
colnames(centr53) <- c("id", "degree", "collab", "eigen", "congress")


centr52 <- cbind.data.frame(V(graph_52)$name, 
                            degree(graph_52)/max(degree(graph_52)), 
                            strength(graph_52)/max(strength(graph_52)), 
                            eigen_centrality(graph_52)$vector) %>% 
  mutate(congress = 52)
colnames(centr52) <- c("id", "degree", "collab", "eigen", "congress")

centrality_df <- rbind.data.frame(centr52, centr53, centr54, centr55)
centrality_df$degree <- as.numeric(centrality_df$degree)
centrality_df$collab <- as.numeric(centrality_df$collab)
centrality_df$eigen <- as.numeric(centrality_df$eigen)


## get bills for each congress
bills55 <- get_bills(55) 
bills54 <- get_bills(54)
bills53 <- get_bills(53)
bills52 <- get_bills(52)


stacked_bills <- rbind.data.frame(bills55 %>% mutate(congress = 55), 
                                  bills54 %>% mutate(congress = 54), 
                                  bills53 %>% mutate(congress = 53), 
                                  bills52 %>% mutate(congress = 52)) 

stacked_bills$idDeputadoAutor <- as.character(stacked_bills$idDeputadoAutor)
stacked_bills$siglaPartidoAutor <- as.factor(stacked_bills$siglaPartidoAutor)


stacked_bills <- stacked_bills %>% 
  filter(grepl("Lei", descricaoTipo)) %>%
  left_join(centrality_df, by = c("congress" = "congress", "idDeputadoAutor" = "id")) %>%
  mutate(outcome = case_when(
    ultimoStatus_descricaoSituacao ==  "Aguardando Apreciação pelo Senado Federal" ~ 1,
    ultimoStatus_descricaoSituacao == "Aguardando Apreciação do Veto" ~ 1,
    ultimoStatus_descricaoSituacao == "Transformado em Norma Jurídica" ~ 1,
    ultimoStatus_descricaoSituacao == "Aguardando Encaminhamento" ~ 1,
    ultimoStatus_descricaoSituacao == "Aguardando Parecer" ~ 1,
  )) %>%
  filter(ultimoStatus_descricaoSituacao != "Tramitando em Conjunto")

stacked_bills[["outcome"]][is.na(stacked_bills[["outcome"]])] <- 0
stacked_bills <- stacked_bills %>% 
  group_by(idProposicao) %>%
  mutate(coauthors = n())

mean(stacked_bills$outcome)

outcome_list <- stacked_bills %>% 
  group_by(ultimoStatus_descricaoSituacao) %>%
  count()


first_df <- stacked_bills %>%
  group_by(idProposicao) %>%
  filter(ordemAssinatura == 1)

deg_max_df <- stacked_bills %>%
  group_by(idProposicao) %>%
  filter(degree == max(degree))

deg_min_df <- stacked_bills %>%
  group_by(idProposicao) %>%
  filter(degree == min(degree))

collab_max_df <- stacked_bills %>%
  group_by(idProposicao) %>%
  filter(collab == max(collab))

collab_min_df <- stacked_bills %>%
  group_by(idProposicao) %>%
  filter(collab == min(collab))

eig_max_df <- stacked_bills %>%
  group_by(idProposicao) %>%
  filter(eigen == max(eigen))

eig_min_df <- stacked_bills %>%
  group_by(idProposicao) %>%
  filter(eigen == min(eigen))


deg1 <- lm(outcome ~ degree + coauthors + siglaPartidoAutor, data = first_df)
deg2 <- glm(outcome ~ degree + coauthors + siglaPartidoAutor, data = first_df, family=binomial(link='logit'))


deg3 <- lm(outcome ~ coauthors + degree*siglaPartidoAutor, data = deg_max_df)
deg4 <- glm(outcome ~ degree + coauthors + siglaPartidoAutor, data = deg_max_df, family=binomial(link='logit'))
summary(deg3)

collab1 <- lm(outcome ~ collab + coauthors + siglaPartidoAutor, data = first_df)
collab2 <- glm(outcome ~ collab + coauthors + siglaPartidoAutor, data = first_df, family=binomial(link='logit'))

collab3 <- lm(outcome ~ collab + coauthors + siglaPartidoAutor, data = collab_max_df)
collab4 <- glm(outcome ~ collab + coauthors + siglaPartidoAutor, data = collab_max_df, family=binomial(link='logit'))


eig1 <- lm(outcome ~ eigen + coauthors + siglaPartidoAutor, data = first_df)
eig2 <- glm(outcome ~ eigen + coauthors + siglaPartidoAutor, data = first_df, family=binomial(link='logit'))

eig3 <- lm(outcome ~ eigen + coauthors + siglaPartidoAutor, data = eig_max_df)
eig4 <- glm(outcome ~ eigen + coauthors + siglaPartidoAutor, data = eig_max_df, family=binomial(link='logit'))

stargazer(deg1, deg2, deg3, deg4, 
          omit = 'siglaPartidoAutor', omit.labels = 'Party Fixed Effect')

stargazer(collab1, collab2, collab3, collab4, 
          omit = 'siglaPartidoAutor', omit.labels = 'Party Fixed Effect')

stargazer(eig1, eig2, eig3, eig4, 
          omit = 'siglaPartidoAutor', omit.labels = 'Party Fixed Effect')

stargazer(deg2, deg4, collab2, collab4, eig2, eig4, 
          omit = 'siglaPartidoAutor')

stargazer(deg1, deg3, collab1, collab3, eig1, eig3, 
          omit = 'siglaPartidoAutor')

