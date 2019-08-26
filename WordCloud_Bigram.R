
rm(list=ls())

library('tidyverse') 
library('KoNLP') 
useNIADic() 
library('reshape2') 

m_df <- read_lines(file.choose()) %>% SimplePos09 %>% melt %>% as_tibble %>% select(3, 1)


# 한글 의미소 분석

m_df %>% mutate(noun=str_match(value, '([가-힣]+)/N')[,2]) %>% tail()

m_df %>% mutate(noun=str_match(value, '([가-힣]+)/N')[,2]) %>% na.omit %>% tail

m_df %>% mutate(noun=str_match(value, '([가-힣]+)/N')[,2]) %>% na.omit %>% count(noun, sort=TRUE)

m_df %>% mutate(noun=str_match(value, '([가-힣]+)/N')[,2]) %>% na.omit %>% filter(str_length(noun)>=2) %>% count(noun, sort=TRUE)


# 워드 클라우드 분석

library('wordcloud2')

m_df %>% mutate(noun=str_match(value, '([가-힣]+)/N')[,2]) %>% na.omit %>% filter(str_length(noun)>=2) %>% count(noun, sort=TRUE) %>% wordcloud2()

m_df %>% mutate(noun=str_match(value, '([가-힣]+)/N')[,2]) %>% na.omit %>% filter(str_length(noun)>=2) %>% count(noun, sort=TRUE) %>% filter(n>=2) %>% wordcloud2(fontFamily='Noto Sans CJK KR Bold', color='skyblue', minRotation=0, maxRotation=0)


# 의미연결망 만들기

m_count <- m_df %>% mutate(noun=str_match(value, '([가-힣]+)/N')[,2]) %>% na.omit %>% filter(str_length(noun)>=2) %>% count(noun, sort=TRUE) %>% head(15)

m_df2 <- m_df %>% mutate(noun=str_match(value, '([가-힣]+)/N')[,2]) %>% na.omit %>% filter(str_length(noun)>=2) %>% select(3, 1)

m_df2

m_df3 <- m_df2 %>% filter(noun %in% m_count$noun)


# install.packages('igraph') 
library('igraph')

mg <- graph_from_data_frame(m_df3)

mg

V(mg)$type <- bipartite_mapping(mg)$type 
mm <- as_incidence_matrix(mg) %*% t(as_incidence_matrix(mg)) 
diag(mm) <- 0 
mg <- graph_from_adjacency_matrix(mm)

plot(mg)

# 깔끔하게 의미연결망을 그려보자

# install.packages('tidygraph') 
# install.packages('ggraph') 
library('tidygraph') 
library('ggraph')


mg %>% as_tbl_graph() %>% 
  ggraph() + 
  geom_edge_link(aes(start_cap = label_rect(node1.name), end_cap = label_rect(node2.name))) + 
  geom_node_text(aes(label=name))



# 바이그램

m_df2 %>% select(noun) %>% mutate(lead=lead(noun))

m_df2 %>% na.omit() %>% select(noun) %>% mutate(lead=lead(noun)) %>% unite(bigram, c(noun, lead), sep=' ') 

m_df2 %>% na.omit() %>% select(noun) %>% mutate(lead=lead(noun)) %>% unite(bigram, c(noun, lead), sep=' ') %>% count(bigram, sort=TRUE)

bigram_df <- m_df2 %>% na.omit() %>% select(noun) %>% mutate(lead=lead(noun)) %>% unite(bigram, c(noun, lead), sep=" ") %>% count(bigram, sort=TRUE) %>% head(19) %>% separate(bigram, c('word1', 'word2'))

bigram_df

bigram_df <- bigram_df[-15,]

bigram_df %>% as_tbl_graph %>% ggraph() + geom_edge_link(aes(start_cap = label_rect(node1.name), end_cap = label_rect(node2.name))) + geom_node_text(aes(label=name))

