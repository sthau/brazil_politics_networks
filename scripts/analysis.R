setwd("~/Desktop/Gov 94JW/brazil_politics_networks/scripts")

library(tidyverse)
library(igraph)
library(janitor)
library(utils)

## load graphs
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


deg55 %>% ggplot(aes(x = degree))+ 
  geom_density(color = "deepskyblue", fill = "deepskyblue", alpha = 0.5) + 
  theme_minimal() + 
  labs(x = "Degree", 
       y = "Density", 
       title = "Degree Distribution of the 55th Congress") + 
  xlim(0,560) + 
  ylim(0,0.007) + 
  geom_vline(xintercept=mean(deg55$degree), size=1, alpha = 1,  color="firebrick2", linetype = "dashed") +
  theme(text = element_text(size=20))
ggsave("../figures/deg55.pdf")

deg54 %>% ggplot(aes(x = degree)) + 
  geom_density(color = "deepskyblue", fill = "deepskyblue", alpha = 0.5) + 
  theme_minimal() + 
  labs(x = "Degree", 
       y = "Density", 
       title = "Degree Distribution of the 54th Congress") + 
  xlim(0,560) + 
  ylim(0,0.007) + 
  geom_vline(xintercept=mean(deg54$degree), size=1, alpha = 1,  color="firebrick2", linetype = "dashed") +
  theme(text = element_text(size=20))
ggsave("../figures/deg54.pdf")


deg53 %>% ggplot(aes(x = degree))+ 
  geom_density(color = "deepskyblue", fill = "deepskyblue", alpha = 0.5) + 
  theme_minimal() + 
  labs(x = "Degree", 
       y = "Density", 
       title = "Degree Distribution of the 53rd Congress") + 
  xlim(0,560) + 
  ylim(0,0.007) + 
  geom_vline(xintercept=mean(deg53$degree), size=1, alpha = 1,  color="firebrick2", linetype = "dashed") +
  theme(text = element_text(size=20))
ggsave("../figures/deg53.pdf")


deg52 %>% ggplot(aes(x = degree))+ 
  geom_density(color = "deepskyblue", fill = "deepskyblue", alpha = 0.5) + 
  theme_minimal() + 
  labs(x = "Degree", 
       y = "Density", 
       title = "Degree Distribution of the 52nd Congress")+ 
  xlim(0,560) + 
  ylim(0,0.007) + 
  geom_vline(xintercept=mean(deg52$degree), size=1, alpha = 1,  color="firebrick2", linetype = "dashed") +
  theme(text = element_text(size=20))
ggsave("../figures/deg52.pdf")

  

# generate adjacency matricies
adj55 <- as_adjacency_matrix(graph_55,attr = "weight",sparse = F)
adj54 <- as_adjacency_matrix(graph_54,attr = "weight",sparse = F)
adj53 <- as_adjacency_matrix(graph_53,attr = "weight",sparse = F)
adj52 <- as_adjacency_matrix(graph_52,attr = "weight",sparse = F)


# clean information for each congress
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


# add party to each graph
V(graph_55)$party <- info55$party
V(graph_54)$party <- info54$party
V(graph_53)$party <- info53$party
V(graph_52)$party <- info52$party



# check group coherence
check_coherence <- function(adj, info_df){
  party_list <- unique(info_df$party)
  avg_cluster <- rep(NA, length(party_list))
  total_cluster <- rep(NA, length(party_list))

  for(i in seq(from = 1, to = length(party_list), by = 1)){
    pt <- party_list[i]
    ids <- info_df %>% 
      filter(party == pt) %>% 
      pull(name)
    
    total_sum_cols <- colSums(adj[,ids, drop = F])
    in_sum_cols <- colSums(adj[ids, ids, drop = F])
    
    avg_cluster[i] <- mean(in_sum_cols/total_sum_cols)
    total_cluster[i] <- sum(in_sum_cols)/sum(total_sum_cols)
    
  }
  
  out <- cbind.data.frame(party_list, avg_cluster, total_cluster)
  colnames(out) <- c("Party", "Avg Coherence", "Total Coherence")
  
  return(out)
  
}

coherence55 <- check_coherence(adj55, info55)
coherence54 <- check_coherence(adj54, info54)
coherence53 <- check_coherence(adj53, info53)
coherence52 <- check_coherence(adj52, info52)

## all party connection rates
connection_rates <- function(adj, info_df){
  # make party list
  party_list <- unique(info_df$party)
  party_list <- na.omit(party_list)
  
  # make empty matricies
  connection_mat_group <- matrix(NA, 
                           nrow = length(party_list), 
                           ncol = length(party_list), 
                           dimnames = list(party_list, party_list))
  
  connection_mat_avg <- matrix(NA, 
                                 nrow = length(party_list), 
                                 ncol = length(party_list), 
                                 dimnames = list(party_list, party_list))
  
  for(ii in seq(from = 1, to = length(party_list), by = 1)){
    col_party <- party_list[ii]
    for(jj in seq(from = 1, to = ii, by = 1)){
      row_party <- party_list[jj]
      
      ids_row <- info_df %>% 
        filter(party == row_party) %>% 
        pull(name)
      
      ids_col <- info_df %>% 
        filter(party == col_party) %>% 
        pull(name)
      
      
      sum_col <- colSums(adj[, ids_col, drop = F])
      sum_row <- rowSums(adj[ids_row, , drop = F])
      
      cross_mat <- adj[ids_row, ids_col, drop = F]
      
      row_group_rate <- sum(rowSums(cross_mat))/sum(sum_row)
      col_group_rate <- sum(colSums(cross_mat))/sum(sum_col)
      
      row_avg_rate <- mean(rowSums(cross_mat)/sum_row)
      col_avg_rate <- mean(colSums(cross_mat)/sum_col)
      
      connection_mat_group[jj,ii] <- col_group_rate
      connection_mat_group[ii,jj] <- row_group_rate
      
      connection_mat_avg[jj,ii] <- col_avg_rate
      connection_mat_avg[ii,jj] <- row_avg_rate
    }
  }
  
  return(list(connection_mat_avg, connection_mat_group)) 
}

connections55 <- connection_rates(adj55, info55)
connections54 <- connection_rates(adj54, info54)
connections53 <- connection_rates(adj53, info53)
connections52 <- connection_rates(adj52, info52)



heatmap_plot <- function(mat, title, subtitle, info_df){
  df <- as.data.frame(mat)
  df <- cbind(from = rownames(df), df) %>%
    pivot_longer(!from , names_to = "to", values_to = "rate") 
  df$rate[df$rate == "NaN"] <- NA
  df$rate <- as.numeric(df$rate) 
  df$rate_bin <- cut(df$rate,
                         breaks = c(0, 0.01, 0.03, 0.05, 0.1, 0.25, 0.5, 0.75, 1),
                         labels = c("0-1%", "1%-3%", "3%-5%", "5%-10%", "10%-25%", "25%-50%", "50%-75%", "75%-100%"), 
                         include.lowest = T)
  
  order <- info_df %>% 
    group_by(party) %>%
    count() %>%
    arrange(-n)
  
  df$from <- factor(df$from,levels=order$party)
  df$to <- factor(df$to,levels=order$party)
  
  
  df %>% ggplot(aes(x = rate)) + 
    geom_histogram()
  
  df %>% 
    drop_na() %>% 
    ggplot(aes(x = from, y = to, fill = rate_bin)) + 
    geom_tile() + 
    scale_fill_manual(values = rev(RColorBrewer::brewer.pal(8, "Spectral")), drop = FALSE) +
    theme_minimal() + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
    labs(x = "", 
         y = "", 
         fill = "Fraction of Co-Authorship", 
         title = title,
         subtitle = subtitle) + 
    theme(text = element_text(size=20),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14)
    )
}

## make party connection heat maps
heatmap_plot(connections55[1], "Average Member Co-Authorship Rates", "55th Congress", info55)
ggsave("../figures/heat_avg_55.pdf")
heatmap_plot(connections54[1], "Average Member Co-Authorship Rates", "54th Congress", info54)
ggsave("../figures/heat_avg_54.pdf")
heatmap_plot(connections53[1], "Average Member Co-Authorship Rates", "53rd Congress", info53)
ggsave("../figures/heat_avg_53.pdf")
heatmap_plot(connections52[1], "Average Member Co-Authorship Rates", "52nd Congress", info52)
ggsave("../figures/heat_avg_52.pdf")

heatmap_plot(connections55[2], "Group Co-Authorship Rates", "55th Congress", info55)
ggsave("../figures/heat_group_55.pdf")
heatmap_plot(connections54[2], "Group Co-Authorship Rates", "54th Congress", info54)
ggsave("../figures/heat_group_54.pdf")
heatmap_plot(connections53[2], "Group Co-Authorship Rates", "53rd Congress", info53)
ggsave("../figures/heat_group_53.pdf")
heatmap_plot(connections52[2], "Group Co-Authorship Rates", "52nd Congress", info52)
ggsave("../figures/heat_group_52.pdf")


## check large parties
size_limit <- 25

size55 <- info55 %>% 
  group_by(party) %>%
  count() %>%
  arrange(-n) %>%
  mutate(congress = 55) %>% 
  filter(n >= size_limit)

size54 <- info54 %>% 
  group_by(party) %>%
  count() %>%
  arrange(-n) %>%
  mutate(congress = 54) %>% 
  filter(n >= size_limit)

size53 <- info53 %>% 
  group_by(party) %>%
  count() %>%
  arrange(-n) %>%
  mutate(congress = 53) %>% 
  filter(n >= size_limit)

size52 <- info52 %>% 
  group_by(party) %>%
  count() %>%
  arrange(-n) %>%
  mutate(congress = 52) %>% 
  filter(n >= size_limit)

party_size <- rbind(size55, size54, size53, size52)
party_size <- party_size %>%
  pivot_wider(names_from = congress, values_from = n) %>%
  drop_na()

large_parties <- unique(party_size$party)
large_parties

## get density of graphs
density55 <- edge_density(graph_55)
density54 <- edge_density(graph_54)
density53 <- edge_density(graph_53)
density52 <- edge_density(graph_52)

## internal densities
group_density <- function(graph){
  # make splitting idx
  idx <- split(V(graph), V(graph)$party)
  
  # subgraph extraction
  lst <- lapply(idx, function(v) induced_subgraph(graph, v))
  
  # get group densities
  return(do.call(rbind, lapply(lst, function(ig)
    data.frame(
      party = unique(V(ig)$party),
      density = edge_density(ig)))))
}

group_density_55 <- group_density(graph_55) %>% mutate(density55 = density) %>% dplyr::select(-density)
group_density_54 <- group_density(graph_54) %>% mutate(density54 = density) %>% dplyr::select(-density)
group_density_53 <- group_density(graph_53) %>% mutate(density53 = density) %>% dplyr::select(-density)
group_density_52 <- group_density(graph_52) %>% mutate(density52 = density) %>% dplyr::select(-density)

order <- info55 %>% 
  group_by(party) %>%
  count()

density_table <- group_density_55 %>%
  full_join(group_density_54, by = c("party" = "party")) %>%
  full_join(group_density_53, by = c("party" = "party")) %>%
  full_join(group_density_52, by = c("party" = "party")) %>%
  left_join(order, by = c("party" = "party")) %>%
  arrange(desc(n)) %>%
  dplyr::select(-n) %>%
  relocate(party, density52, density53, density54, density55)

density_table[sapply(density_table, is.nan)] <- NA



density_table %>% kableExtra::kable(digits = 3, format = "latex")
  

  
  
  
  
  
  
