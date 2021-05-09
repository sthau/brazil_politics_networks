setwd("~/Desktop/Gov 94JW/scripts/brazil_politics_networks")

library(tidyverse)
library(igraph)
library(janitor)
library(utils)
library(data.table)

year <- 2019
author_path <- paste("http://dadosabertos.camara.leg.br/arquivos/proposicoesAutores/csv/proposicoesAutores-", year, ".csv", sep = "")
bill_path <- paste("http://dadosabertos.camara.leg.br/arquivos/proposicoes/csv/proposicoes-", year, ".csv", sep = "")

author_df <- read.csv(author_path, sep = ";")
bill_df <- read.csv(bill_path, sep = ";")

