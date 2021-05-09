setwd("~/Desktop/Gov 94JW/scripts/brazil_politics_networks")

library(tidyverse)
library(igraph)
library(janitor)
library(utils)
library(data.table)
library(xlsx)
library(kableExtra)

check_bills <- function(year){
  # make in and out paths
  in_path <- paste("http://dadosabertos.camara.leg.br/arquivos/proposicoesAutores/csv/proposicoesAutores-", year, ".csv", sep = "")
  
  #read in data
  full_df <- read.csv(in_path, sep = ";") %>% filter(siglaPartidoAutor == "PL")
  
  View(full_df)
}

years <- seq(from = 2003, to = 2003, by = 1)

lapply(years, check_bills)


test <- read.csv("http://dadosabertos.camara.leg.br/arquivos/proposicoesAutores/csv/proposicoesAutores-2007.csv", sep = ";")
test <- test %>% filter(tipoAutor == "Deputado")

content <- read.csv("http://dadosabertos.camara.leg.br/arquivos/proposicoes/csv/proposicoes-2007.csv", sep = ";")

content$ementa <- stri_trans_general(content$ementa,"Latin-ASCII")
content$ementa <- str_to_lower(content$ementa)


test <- test %>% 
  left_join(content, by = c("idProposicao" = "id"))
check <- test %>% 
  group_by(idProposicao) %>% 
  filter(n()<171) %>% 
  filter(n() > 1) %>%
  # filter(!grepl("constitucion|constituicao", ementa)) %>%
  mutate(count = n()) %>%
  distinct(idProposicao, .keep_all = T)

check <- as.data.frame(check)
View(check)

write.xlsx(check, "check.xlsx")

unique(check %>% pull(idProposicao))



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
    filter(n()<171)
  
  df2 <- bills2 %>% 
    left_join(content2, by = c("idProposicao" = "id")) %>%
    filter(!grepl("constitucion|constituicao", ementa)) %>%
    filter(tipoAutor == "Deputado") %>% 
    group_by(idProposicao) %>% 
    filter(n()<171)
  
  df3 <- bills3 %>% 
    left_join(content3, by = c("idProposicao" = "id")) %>%
    filter(!grepl("constitucion|constituicao", ementa)) %>%
    filter(tipoAutor == "Deputado") %>% 
    group_by(idProposicao) %>% 
    filter(n()<171)
  
  df4 <- bills4 %>% 
    left_join(content4, by = c("idProposicao" = "id")) %>%
    filter(!grepl("constitucion|constituicao", ementa)) %>%
    filter(tipoAutor == "Deputado") %>% 
    group_by(idProposicao) %>% 
    filter(n()<171)
  
  full_df <- rbind(df1, df2, df3, df4)
  
  num_bills <- length(unique(full_df$idProposicao))
  
  full_df <- full_df %>%
    group_by(idProposicao) %>%
    summarize(count = n())
  
  mean_authors <- mean(full_df$count)
  cond_mean_authors <- mean(full_df[full_df$count > 1,]$count)
  
  return(c(num_bills, mean_authors, cond_mean_authors))
  
  
}


graph_55 <- simplify(read_graph("../data/clean_networks/graph_55.gml", format = "gml"),  
                     edge.attr.comb=list(weight="sum"))
graph_54 <- simplify(read_graph("../data/clean_networks/graph_54.gml", format = "gml"),  
                     edge.attr.comb=list(weight="sum")) 
graph_53 <- simplify(read_graph("../data/clean_networks/graph_53.gml", format = "gml"),  
                     edge.attr.comb=list(weight="sum")) 
graph_52 <- simplify(read_graph("../data/clean_networks/graph_52.gml", format = "gml"),  
                     edge.attr.comb=list(weight="sum")) 




# get degree distributions
deg55 <- as.data.frame(degree(graph_55, normalized = FALSE))
colnames(deg55) <- "degree"
deg54 <- as.data.frame(degree(graph_54, normalized = FALSE))
colnames(deg54) <- "degree"
deg53 <- as.data.frame(degree(graph_53, normalized = FALSE))
colnames(deg53) <- "degree"
deg52 <- as.data.frame(degree(graph_52, normalized = FALSE))
colnames(deg52) <- "degree"




empty_as_na <- function(x){
  if("factor" %in% class(x)) x <- as.character(x)
  ifelse(as.character(x)!="", x, NA)
}

clean_info <- function(year1, year2, year3, year4, G){
  
  # turn NAs into actual NAs
  info_df <- as_data_frame(G, "vertices") %>% select(-id)
  info_df[[year1]][info_df[[year1]] == "NA"] <- NA 
  info_df[[year2]][info_df[[year2]] == "NA"] <- NA 
  info_df[[year3]][info_df[[year3]] == "NA"] <- NA 
  info_df[[year4]][info_df[[year4]] == "NA"] <- NA 
  
  # make blanks NAs
  info_df[[year1]][info_df[[year1]] == ""] <- NA 
  info_df[[year2]][info_df[[year2]] == ""] <- NA 
  info_df[[year3]][info_df[[year3]] == ""] <- NA 
  info_df[[year4]][info_df[[year4]] == ""] <- NA 
  
  return(info_df)
}


info55 <- clean_info("party2015", "party2016", "party2017", "party2018", graph_55)
info54 <- clean_info("party2011", "party2012", "party2013", "party2014", graph_54)
info53 <- clean_info("party2007", "party2008", "party2009", "party2010", graph_53)
info52 <- clean_info("party2003", "party2004", "party2005", "party2006", graph_52)


info55 <- info55 %>% 
  mutate(party = case_when(
    !is.na(party2018) ~ party2018,
    is.na(party2018) & !is.na(party2017) ~ party2017,
    is.na(party2018) & is.na(party2017) & !is.na(party2016) ~ party2016,
    is.na(party2018) & is.na(party2017) & is.na(party2016) & !is.na(party2015) ~ party2015
  ))

info54 <- info54 %>% 
  mutate(party = case_when(
    !is.na(party2014) ~ party2014,
    is.na(party2014) & !is.na(party2013) ~ party2013,
    is.na(party2014) & is.na(party2013) & !is.na(party2012) ~ party2012,
    is.na(party2014) & is.na(party2013) & is.na(party2012) & !is.na(party2011) ~ party2011
  ))

info53 <- info53 %>% 
  mutate(party = case_when(
    !is.na(party2010) ~ party2010,
    is.na(party2010) & !is.na(party2009) ~ party2009,
    is.na(party2010) & is.na(party2009) & !is.na(party2008) ~ party2008,
    is.na(party2010) & is.na(party2009) & is.na(party2008) & !is.na(party2007) ~ party2007
  ))

info52 <- info52 %>% 
  mutate(party = case_when(
    !is.na(party2006) ~ party2006,
    is.na(party2006) & !is.na(party2005) ~ party2005,
    is.na(party2006) & is.na(party2005) & !is.na(party2004) ~ party2004,
    is.na(party2006) & is.na(party2005) & is.na(party2004) & !is.na(party2003) ~ party2003
  ))

billsum55 <- get_bills(55)
billsum54 <- get_bills(54)
billsum53 <- get_bills(53)
billsum52 <- get_bills(52)


labels <- c("Congress", "Parties Represented","Legislators", "Edges", 
            "Mean Degree", "Mean Collaboration", 
            "Bills", "Mean Co-Authors", "Mean Co-Authors (>1 Author)")

sum55 <- c(55, length(unique(info55$party)), length(unique(info55$name)), gsize(graph_55),
           mean(deg55$degree), mean(strength(graph_55)), 
           billsum55[1], billsum55[2], billsum55[3])

sum54 <- c(54, length(unique(info54$party)), length(unique(info54$name)), gsize(graph_54),
           mean(deg54$degree), mean(strength(graph_54)), 
           billsum54[1], billsum54[2], billsum54[3])

sum53 <- c(53, length(unique(info53$party)), length(unique(info53$name)), gsize(graph_53),
           mean(deg53$degree), mean(strength(graph_53)), 
           billsum53[1], billsum53[2], billsum53[3])

sum52 <- c(52, length(unique(info52$party)), length(unique(info52$name)), gsize(graph_52),
           mean(deg52$degree), mean(strength(graph_52)), 
           billsum52[1], billsum52[2], billsum52[3])

summary_table <- rbind.data.frame(labels, sum52, sum53, sum54, sum55) %>% 
  row_to_names(row_number = 1)


summary_table %>% kable(format = "latex", digits = 3)


