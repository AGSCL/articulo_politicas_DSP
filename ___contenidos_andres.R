library(readxl)
library(tidyverse)
NG_Def <- read_excel("_doc/Codificaciones.xlsx", 
                             sheet = "NG_Def")
NG_Ideo <- read_excel("_doc/Codificaciones.xlsx", 
                             sheet = "NG_Ideo")
P_Moral <- read_excel("_doc/Codificaciones.xlsx", 
                      sheet = "P_Moral")
P_medico <- read_excel("_doc/Codificaciones.xlsx", 
                      sheet = "P_medico")
SI_Contendores <-read_excel("_doc/Codificaciones.xlsx", 
                            sheet = "SI_Contendores")

to_remove <- c(tm::stopwords(kind="spanish"), c("usted", "ahí", "aquí", "acá", "además", "bajo", "muchas",
                                            "pues", "tal", "tan", "así", "dijo", "dicen","xq","status","wnes","jaja","jajaj","tb","t","mensaje",
                                            "cómo", "sino", "entonces", "aunque", "don", "doña","nomas","si","pa","jajajaja","jjaja",
                                            "alla", "creo", "cada", "alla", "le", "el", "la", "en","weon","wea","q","jajaja","ajaja",
                                            "igual", "pasa", "pasan", "hace","manda","cacha", "multimedia","wn","po","cn","pal","ta","ajajajja", "ajajajjaja", "seee", "ajajajajja", "wajajajajajjaja", "wajajaja", "10", "1", "pq", "jajajajajajaja", "k",
                                            "toda", "ahora", "tema", "omitido", "que","Porqué","siempre","https","toy","mas","2","3", "ser",
                                            "hacen", "hacer","ajajajajaja", "jajajajajaja", "jajajajaja", "jajajajaja"))

library(stringi)

NG_Def$contenido <- stri_replace_all_regex(NG_Def$contenido,
                                     pattern=paste0(" ",to_remove," "),
                                     replacement=rep(" ",length(to_remove)),
                                     vectorize=FALSE)
NG_Ideo$contenido <- stri_replace_all_regex(NG_Ideo$contenido,
                                           pattern=paste0(" ",to_remove," "),
                                           replacement=rep(" ",length(to_remove)),
                                           vectorize=FALSE)


library("tidytext")

NG_Ideo %>% #coalicion=="GOB" #, select="contenido"
  unnest_tokens(input = contenido,
                output = word) %>%
  filter(!word %in% to_remove)%>%
  count(coalicion, word, sort = TRUE) %>%
  group_by(coalicion) %>%
  top_n(n = 6, n) %>%
  ggplot(aes(x = reorder_within(word, n, coalicion), y = n, fill = factor(coalicion))) +
  geom_col(show.legend = FALSE) +
  ylab("") +
  xlab("") +
  coord_flip() +
  facet_wrap(~coalicion, ncol = 2, scales = "free_y") +
  scale_x_reordered() +
 # ggtitle("Palabras más usadas: Debate ideológico")+
  ylab("Frecuencia de aparición")+
  scale_fill_manual(values= c("lightgray", "darkgray"))+
  theme_classic()

ggsave("./_fig/ng_ideo.png", dpi=300)

o_words <- NG_Ideo %>%
  unnest_tokens(input = contenido,
                output = word) %>%
  filter(coalicion != "GOB") %>% 
  #filter(!word %in% to_remove) %>%
  count(word, sort = TRUE) 
NG_Ideo %>%
  unnest_tokens(input = contenido,
                output = word) %>%
  filter(coalicion == "GOB") %>% 
  filter(!word %in% to_remove) %>%
  count(word, sort = TRUE) %>% 
  filter(!word %in% o_words$word) %>% # only select words nobody else uses
  top_n(n = 6, n) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col(show.legend = FALSE) +
  ylab("") + xlab("") +
  coord_flip() +
  ggtitle("Palabras únicas de Gobierno en debate ideológico")


o_words <- NG_Ideo %>%
  unnest_tokens(input = contenido,
                output = word) %>%
  filter(coalicion != "OPOS") %>% 
  #filter(!word %in% to_remove) %>%
  count(word, sort = TRUE) 
NG_Ideo %>%
  unnest_tokens(input = contenido,
                output = word) %>%
  filter(coalicion == "OPOS") %>% 
  filter(!word %in% to_remove) %>%
  count(word, sort = TRUE) %>% 
  filter(!word %in% o_words$word) %>% # only select words nobody else uses
  top_n(n = 4, n) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col(show.legend = FALSE) +
  ylab("") + xlab("") +
  coord_flip() +
  ggtitle("Palabras únicas de Oposición en debate ideológico")

NG_Def %>% #coalicion=="GOB" #, select="contenido"
  unnest_tokens(input = contenido,
                output = word) %>%
  filter(!word %in% to_remove)%>%
  count(coalicion, word, sort = TRUE) %>%
  group_by(coalicion) %>%
  top_n(n = 6, n) %>%
  ggplot(aes(x = reorder_within(word, n, coalicion), y = n, fill = coalicion)) +
  geom_col(show.legend = FALSE) +
  ylab("") +
  xlab("") +
  coord_flip() +
  facet_wrap(~coalicion, ncol = 2, scales = "free_y") +
  scale_x_reordered() +
  ggtitle("Palabras más usadas: Debate definicional")



P_Moral %>% #coalicion=="GOB" #, select="contenido"
  unnest_tokens(input = contenido,
                output = word) %>%
  filter(!word %in% to_remove)%>%
  count(coalicion, word, sort = TRUE) %>%
  group_by(coalicion) %>%
  top_n(n = 3, n) %>%
  ggplot(aes(x = reorder_within(word, n, coalicion), y = n, fill = coalicion)) +
  geom_col(show.legend = FALSE) +
  ylab("") +
  xlab("") +
  coord_flip() +
  facet_wrap(~coalicion, ncol = 2, scales = "free_y") +
  scale_x_reordered() +
  ggtitle("Palabras más usadas: Marco moral")



o_words <- P_Moral %>%
  unnest_tokens(input = contenido,
                output = word) %>%
  filter(coalicion != "GOB") %>% 
  #filter(!word %in% to_remove) %>%
  count(word, sort = TRUE) 
P_Moral %>%
  unnest_tokens(input = contenido,
                output = word) %>%
  filter(coalicion == "GOB") %>% 
  filter(!word %in% to_remove) %>%
  count(word, sort = TRUE) %>% 
  filter(!word %in% o_words$word) %>% # only select words nobody else uses
  top_n(n = 6, n) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col(show.legend = FALSE) +
  ylab("") + xlab("") +
  coord_flip() +
  ggtitle("Palabras únicas de Gobierno en marco moral")


o_words <- P_medico %>%
  unnest_tokens(input = contenido,
                output = word) %>%
  filter(coalicion != "OPOS") %>% 
  #filter(!word %in% to_remove) %>%
  count(word, sort = TRUE) 
P_medico %>%
  unnest_tokens(input = contenido,
                output = word) %>%
  filter(coalicion == "OPOS") %>% 
  filter(!word %in% to_remove) %>%
  count(word, sort = TRUE) %>% 
  filter(!word %in% o_words$word) %>% # only select words nobody else uses
  top_n(n = 4, n) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col(show.legend = FALSE) +
  ylab("") + xlab("") +
  coord_flip() +
  ggtitle("Palabras únicas de Oposición en marco moral")


P_medico %>% #coalicion=="GOB" #, select="contenido"
  unnest_tokens(input = contenido,
                output = word) %>%
  filter(!word %in% to_remove)%>%
  count(coalicion, word, sort = TRUE) %>%
  group_by(coalicion) %>%
  top_n(n = 3, n) %>%
  ggplot(aes(x = reorder_within(word, n, coalicion), y = n, fill = coalicion)) +
  geom_col(show.legend = FALSE) +
  ylab("") +
  xlab("") +
  coord_flip() +
  facet_wrap(~coalicion, ncol = 2, scales = "free_y") +
  scale_x_reordered() +
  ggtitle("Palabras más usadas: Marco médico")



SI_Contendores %>% #coalicion=="GOB" #, select="contenido"
  unnest_tokens(input = contenido,
                output = word) %>%
  filter(!word %in% to_remove)%>%
  count(coalicion, word, sort = TRUE) %>%
  group_by(coalicion) %>%
  top_n(n = 3, n) %>%
  ggplot(aes(x = reorder_within(word, n, coalicion), y = n, fill = coalicion)) +
  geom_col(show.legend = FALSE) +
  ylab("") +
  xlab("") +
  coord_flip() +
  facet_wrap(~coalicion, ncol = 2, scales = "free_y") +
  scale_x_reordered() +
  #ggtitle("Palabras más usadas: actor contendor")+
  scale_fill_manual(values= c("lightgray", "darkgray"))+
  theme_classic()

ggsave("./_fig/ng_ideo.png", dpi=300)

o_words <- SI_Contendores %>%
  unnest_tokens(input = contenido,
                output = word) %>%
  filter(coalicion != "GOB") %>% 
  #filter(!word %in% to_remove) %>%
  count(word, sort = TRUE) 
SI_Contendores %>%
  unnest_tokens(input = contenido,
                output = word) %>%
  filter(coalicion == "GOB") %>% 
  filter(!word %in% to_remove) %>%
  count(word, sort = TRUE) %>% 
  filter(!word %in% o_words$word) %>% # only select words nobody else uses
  top_n(n = 6, n) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col(show.legend = FALSE) +
  ylab("Frecuencia de aparación")  + xlab("") +
  coord_flip() +
  scale_fill_manual(values= c("lightgray", "darkgray"))+
  theme_classic() -> fig_gob_si_conten
  #ggtitle("Palabras únicas de Gobierno para actor contendor")


o_words <- SI_Contendores %>%
  unnest_tokens(input = contenido,
                output = word) %>%
  filter(coalicion != "OPOS") %>% 
  #filter(!word %in% to_remove) %>%
  count(word, sort = TRUE) 
SI_Contendores %>%
  unnest_tokens(input = contenido,
                output = word) %>%
  filter(coalicion == "OPOS") %>% 
  filter(!word %in% to_remove) %>%
  count(word, sort = TRUE) %>% 
  filter(!word %in% o_words$word) %>% # only select words nobody else uses
  top_n(n = 4, n) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col(show.legend = FALSE) +
  ylab("") + xlab("") +
  coord_flip() +
  scale_fill_manual(values= c("lightgray", "darkgray"))+
  theme_classic() -> fig_opos_si_conten
  #ggtitle("Palabras únicas de Oposición para actor contendor")


cowplot::plot_grid(fig_gob_si_conten, fig_opos_si_conten, labels = "AUTO") 
ggsave("./_fig/si_conten.png", dpi=300)
