if(isTRUE(getOption('knitr.in.progress'))==T){
} else {
  #path<-ifelse(!grepl("$\\/",getwd()),paste0(getwd(),"/"),getwd())
  path<- getwd()
}

#https://docs.google.com/spreadsheets/d/1Ij8zDdusH-NsajNr0hSZMKtRk-PE4NWS/export?format=tsv&id=1Ij8zDdusH-NsajNr0hSZMKtRk-PE4NWS&gid=1306020744

library(tidyverse)
bd_noticias <- read_delim("https://docs.google.com/spreadsheets/d/1Ij8zDdusH-NsajNr0hSZMKtRk-PE4NWS/export?format=csv&id=1Ij8zDdusH-NsajNr0hSZMKtRk-PE4NWS&gid=1306020744", 
                                                                                     delim = ",", escape_double = FALSE, 
                                                                                      locale = ,
                                                                                     trim_ws = TRUE)

bd_noticias_alt1 <- read_delim("https://docs.google.com/spreadsheets/d/1Ij8zDdusH-NsajNr0hSZMKtRk-PE4NWS/export?format=csv&id=1Ij8zDdusH-NsajNr0hSZMKtRk-PE4NWS&gid=1702279652", 
                          delim = ",", escape_double = FALSE, 
                          locale = ,
                          skip=1,
                          trim_ws = TRUE) %>% janitor::clean_names()


bd_noticias_alt2 <- read_delim("https://docs.google.com/spreadsheets/d/1Ij8zDdusH-NsajNr0hSZMKtRk-PE4NWS/export?format=csv&id=1Ij8zDdusH-NsajNr0hSZMKtRk-PE4NWS&gid=491760237", 
                               delim = ",", escape_double = FALSE, 
                               locale = ,
                               skip=1,
                               trim_ws = TRUE) %>% janitor::clean_names()


bd_noticias_alt3 <- read_delim("https://docs.google.com/spreadsheets/d/1Ij8zDdusH-NsajNr0hSZMKtRk-PE4NWS/export?format=csv&id=1Ij8zDdusH-NsajNr0hSZMKtRk-PE4NWS&gid=1300125576", 
                               delim = ",", escape_double = FALSE, 
                               locale = ,
                               skip=1,
                               trim_ws = TRUE) %>% janitor::clean_names()


invisible("Suma de total de noticias")
sum(
sum(!is.na(bd_noticias_alt1$vinculos_7)),sum(!is.na(bd_noticias_alt2$x8)),
sum(!is.na(bd_noticias_alt3$https_news_google_com_search_q_test_percent_20de_percent_20drogas_percent_20parlamentarios_hl_es_419_gl_cl_ceid_cl_percent_3aes_419_8))
)
bd_noticias2<-
bd_noticias %>% 
  #get incomplete rows
  dplyr::filter(!is.na(tipo)) %>% 
  #count duplicateds
  dplyr::group_by(fuente) %>% 
  dplyr::mutate(rn=row_number(), tot_dup=max(rn,na.rm=T)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(rn2=paste0(dplyr::case_when(tipo==1~"a",tipo==2~"b",T~"c"),sprintf("%02.0f",row_number())))


#explore duplicated despite efforts of unduplicate
#bd_noticias2 %>% dplyr::filter(tot_dup>1) %>% View()
bd_noticias3<-
  bd_noticias2 %>% 
  #get incomplete rows
  dplyr::filter(rn<2)

invisible("Base de datos")
sum(
  nrow(bd_noticias3[bd_noticias3$tipo==1,]),
  nrow(bd_noticias3[bd_noticias3$tipo==2,]),
  nrow(bd_noticias3[bd_noticias3$tipo==3,]))


bd_noticias3 %>% 
  nrow()

#  dplyr::filter(motivodeegreso_mod_imp!="En curso")%>% #Sacar los tratamientos que estén en curso 
tab1_lab<- paste0('Noticias identificadas en motor de búsqueda \n(n = ', formatC(sum(
  sum(!is.na(bd_noticias_alt1$vinculos_7)),sum(!is.na(bd_noticias_alt2$x8)),
  sum(!is.na(bd_noticias_alt3$https_news_google_com_search_q_test_percent_20de_percent_20drogas_percent_20parlamentarios_hl_es_419_gl_cl_ceid_cl_percent_3aes_419_8))
), format='f', big.mark=',', digits=0),')')

tab2_lab<- paste0('Artículos después de\neliminar duplicados\n(n = ', 
                  formatC(sum(nrow(bd_noticias3[bd_noticias3$tipo==1,]),
                              nrow(bd_noticias3[bd_noticias3$tipo==2,]),
                              nrow(bd_noticias3[bd_noticias3$tipo==3,])), format='f', big.mark=',', digits=0),')')


#bd_noticias3 %>% janitor::tabyl(OBS)

  

tab2_5_lab<- paste0('&#8226; No relacionado (n= ', sum(grepl("NO RELACIONADO",bd_noticias3$OBS)),
                    ')\\\\l &#8226; Medio audiovisual (n= ', sum(grepl("AUDIOVISUAL",bd_noticias3$OBS)),
                    ')\\\\l &#8226; Acceso restringido (ej., early-access) (n= ', sum(grepl("EARLY ACCESS",bd_noticias3$OBS)),
                    ')\\\\l &#8226; Exclusivamente internacional (n= ', sum(grepl("INTERNACIONAL",bd_noticias3$OBS)),
                    ')\\\\l&#8226; Exclusivamente fact-checking (n= ', sum(grepl("FACT-CHECKING",bd_noticias3$OBS)),')\\\\l')

tab3_lab<- paste0('Registros incluidos\n para la evaluación de calidad\n(n = ', formatC(nrow(dplyr::filter(bd_noticias3,!grepl("SACAR",OBS))), format='f', big.mark=',', digits=0), ')')

tab3_5_lab<- paste0('&#8226; Contiene citas o referencias de otros actores (n=', sum(grepl("DESCARTAR.*OTROS ACTORES",bd_noticias3$OBS)),
                    ')\\\\l&#8226; No contiene citas o referencias (n= ', sum(grepl("DESCARTAR.*DIRECTAS O INDIRECTAS",bd_noticias3$OBS)),')\\\\l')

tab4_lab<- paste0('Registros incluidos para\nel análisis cualitativo\n(n = ', formatC(nrow(dplyr::filter(bd_noticias3,!grepl("SACAR|DESCARTAR",OBS))), format='f', big.mark=',', digits=0), ')')

#https://stackoverflow.com/questions/46750364/diagrammer-and-graphviz
#https://mikeyharper.uk/flowcharts-in-r-using-diagrammer/
#http://blog.nguyenvq.com/blog/2012/05/29/better-decision-tree-graphics-for-rpart-via-party-and-partykit/
#http://blog.nguyenvq.com/blog/2014/01/17/skeleton-to-create-fast-automatic-tree-diagrams-using-r-and-graphviz/
#https://cran.r-project.org/web/packages/DiagrammeR/vignettes/graphviz-mermaid.html
#https://stackoverflow.com/questions/39133058/how-to-use-graphviz-graphs-in-diagrammer-for-r
#https://subscription.packtpub.com/book/big_data_and_business_intelligence/9781789802566/1/ch01lvl1sec21/creating-diagrams-via-the-diagrammer-package
#https://justlegal.be/2019/05/using-flowcharts-to-display-legal-procedures/
# paste0("No. of treatments: ",table(table(t_id_1)) %>% formatC(big.mark = ","),"; No. of controls: ",table(table(c_id_1))%>% formatC(big.mark = ","))
#
library(DiagrammeR) #⋉
plot_grviz<-
grViz("digraph flowchart {
      # node definitions with substituted label text
      node [fontname = Times, shape = rectangle,fontsize = 9]        
      tab1 [label = '@@1']
      tab2 [label = '@@2']
      tab25 [label = '@@3',fontsize = 7]
      tab3 [label = '@@4']
      tab35 [label = '@@5',fontsize = 7]
      tab4 [label = '@@6']
      blank [label = '', width = 0.0001, height = 0.0001]
      blank2 [label = '', width = 0.0001, height = 0.0001]

      # edge definitions with the node IDs
      tab1 -> tab2 [label='    Eliminación duplicados',fontsize = 8];
      tab2 -> blank  [arrowhead = none];
      blank -> tab25 
            subgraph {
              rank = same; tab25; blank;
            }
      blank -> tab3 [label='      Resultado de la eliminación (al menos un criterio)',fontsize = 8];
      tab3 -> blank2 [arrowhead = none];
      blank2 -> tab35 
            subgraph {
              rank = same; tab35; blank2;
            }
      blank2 -> tab4
      }

      [1]:  tab1_lab
      [2]:  tab2_lab
      [3]:  tab2_5_lab
      [4]:  tab3_lab
      [5]:  tab3_5_lab
      [6]:  tab4_lab
      ", width = 1200,
        height = 900)

DPI = 1200
WidthCM = 8
HeightCM = 11

library(rsvg)
library(DiagrammeRsvg)

plot_grviz %>%
  export_svg %>% charToRaw %>% rsvg_pdf("./_fig/_flowchart4.pdf")
plot_grviz %>% export_svg()%>%charToRaw %>% rsvg(width = WidthCM *(DPI/2.54), height = HeightCM *(DPI/2.54)) %>% 
  png::writePNG("./_flowchart_merge_wo_fmt4.png")

htmlwidgets::saveWidget(plot_grviz, "./_fig/_flowchart_merge_222_4.html")
webshot::webshot("./_fig/_flowchart_merge_222_4.html","./_fig/_flowchart_merge_formatted_4.png",vwidth = 900, vheight = 1200,
                 zoom = 2)

#save.image("trabajo_20221205.RData")

bd_noticias4<-
bd_noticias3 %>% 
  dplyr::filter(!grepl("SACAR",OBS)) %>% 
  dplyr::filter(!grepl("DESCARTAR",OBS))

#save.image("./_data/trabajo_20221206.RData")

#save.image("./_data/trabajo_20221211.RData")

#save.image("./_data/trabajo_20221227.RData")


save.image("./_data/trabajo_20221228.RData")

invisible("to check all eliminated articles and see if they have any nodes in Nvivo")
bd_noticias3 %>% 
  dplyr::filter(grepl("SACAR",OBS)) %>% distinct(rn2) %>% print(n=200)

#get incomplete rows
dplyr::filter(bd_noticias2,rn>=2) %>% distinct(rn2) %>% print(n=200)

bd_noticias2 %>% dplyr::filter(rn2 %in% unlist(dplyr::filter(bd_noticias2,rn>=2) %>% distinct(rn2) %>% print(n=200)))


dplyr::filter(bd_noticias3,rn2 %in% as.character(unlist(bd_noticias3 %>% dplyr::filter(grepl("SACAR",OBS))%>% 
              distinct(rn2) %>% print(n=200)))) %>% View()



# Wordclouds --------------------------------------------------------------


list_pdf<-list()

pdf.text <- pdftools::pdf_text("./_trabajo/a01.pdf")
pdf.text<-unlist(pdf.text)
pdf.text<-tolower(pdf.text)


pdf.text[2]