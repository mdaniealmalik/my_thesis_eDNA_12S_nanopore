library(tidyverse)

setwd("D:/S2_UNDIP/Riset_S2_Danie/Data_Analysis")

## Functional Traits
#####
#Load table taxon and functional eDNA
table_taxon_functional_eDNA <- read.csv("D:/S2_UNDIP/Riset_S2_Danie/Data_Analysis/table_taxon_functional_eDNA.csv")

#Load table taxon and functional vc
table_taxon_functional_vc <- read.csv("D:/S2_UNDIP/Riset_S2_Danie/Data_Analysis/table_taxon_functional_vc.csv")

#select column
eDNA_data_functional <- table_taxon_functional_eDNA %>% select(Species, AnaCat, DemersPelag, BodyShapeI, category_troph, Nocturnal_fish, IUCN_Code, Site, Data) %>%
  data.frame()

vc_data_functional <- table_taxon_functional_vc %>% select(Species, AnaCat, DemersPelag, BodyShapeI, category_troph, Nocturnal_fish, IUCN_Code, Site, Data) %>%
  data.frame()

full_join(eDNA_data_functional, vc_data_functional)

###Habitat
eDNA_functional_habitat <- eDNA_data_functional %>% select(Species, DemersPelag, Site, Data) %>% unique()

eDNA_functional_habitat_total <- eDNA_functional_habitat %>% group_by(Site, Data, DemersPelag) %>% summarise(total_count = n()) %>% data.frame()

vc_functional_habitat <- vc_data_functional %>% select(Species, DemersPelag, Site, Data) %>% unique()
vc_functional_habitat_total <- vc_functional_habitat %>% group_by(Site, Data, DemersPelag) %>% summarise(total_count = n()) %>% data.frame()

data_functional_habitat_total <- full_join(eDNA_functional_habitat_total, vc_functional_habitat_total)

data_functional_habitat_total$DemersPelag <- factor(data_functional_habitat_total$DemersPelag, 
                                                    levels = c("reef-associated", "demersal", "benthopelagic", "pelagic-neritic", "pelagic-oceanic", "bathydemersal", "bathypelagic", "Unknown"))

data_functional_habitat_total$Site <- factor(data_functional_habitat_total$Site, levels = c("Taka Malang E", "Menjangan Kecil", "Cilik/Kecil", "Genting 1"))

###category_troph
eDNA_functional_troph <- eDNA_data_functional %>% select(Species, category_troph, Site, Data) %>% unique()
eDNA_functional_troph_total <- eDNA_functional_troph %>% group_by(Site, Data, category_troph) %>% summarise(total_count = n()) %>% data.frame()

vc_functional_troph <- vc_data_functional %>% select(Species, category_troph, Site, Data) %>% unique()
vc_functional_troph_total <- vc_functional_troph %>% group_by(Site, Data, category_troph) %>% summarise(total_count = n()) %>% data.frame()

data_functional_troph_total <- full_join(eDNA_functional_troph_total, vc_functional_troph_total)

data_functional_troph_total$Site <- factor(data_functional_troph_total$Site, levels = c("Taka Malang E", "Menjangan Kecil", "Cilik/Kecil", "Genting 1"))

###Nocturnal
eDNA_functional_nocturnal <- eDNA_data_functional %>% select(Species, Nocturnal_fish, Site, Data) %>% unique()
eDNA_functional_nocturnal %>% filter(Nocturnal_fish == "Yes")

eDNA_functional_nocturnal_total <- eDNA_functional_nocturnal %>% group_by(Site, Data, Nocturnal_fish) %>% summarise(total_count = n()) %>% data.frame()
eDNA_functional_nocturnal_total <- eDNA_functional_nocturnal_total %>% filter(Nocturnal_fish == "Yes")

vc_functional_nocturnal <- vc_data_functional %>% select(Species, Nocturnal_fish, Site, Data) %>% unique()
vc_functional_nocturnal %>% filter(Nocturnal_fish == "Yes")
vc_functional_nocturnal_total <- vc_functional_nocturnal %>% group_by(Site, Data, Nocturnal_fish) %>% summarise(total_count = n()) %>% data.frame()
vc_functional_nocturnal_total <- vc_functional_nocturnal_total %>% filter(Nocturnal_fish == "Yes")

data_functional_nocturnal_total <- full_join(eDNA_functional_nocturnal_total, vc_functional_nocturnal_total)

data_functional_nocturnal_total$Site <- factor(data_functional_nocturnal_total$Site, levels = c("Taka Malang E", "Menjangan Kecil", "Cilik/Kecil", "Genting 1"))

### Anacat status (migratory)
eDNA_functional_migratory <- eDNA_data_functional %>% select(Species, AnaCat, Site, Data) %>% unique()
eDNA_functional_migratory_total <- eDNA_functional_migratory %>% group_by(Site, Data, AnaCat) %>% summarise(total_count = n()) %>% data.frame()

vc_functional_migratory <- vc_data_functional %>% select(Species, AnaCat, Site, Data) %>% unique()
vc_functional_migratory_total <- vc_functional_migratory %>% group_by(Site, Data, AnaCat) %>% summarise(total_count = n()) %>% data.frame()

data_functional_migratory_total <- full_join(eDNA_functional_migratory_total , vc_functional_migratory_total )

data_functional_migratory_total$Site <- factor(data_functional_migratory_total$Site, levels = c("Taka Malang E", "Menjangan Kecil", "Cilik/Kecil", "Genting 1"))

data_functional_migratory_total$AnaCat <- factor(data_functional_migratory_total$AnaCat, levels = c("non-migratory", "oceanodromous", "catadromous", "amphidromous", "potamodromous", "Unknown"))

###combine all visual
data_functional_habitat_edt <- data_functional_habitat_total %>% cbind(Functional = "Environment preference")
colnames(data_functional_habitat_edt)[3] <- "Category"

data_functional_troph_edt <- data_functional_troph_total %>% cbind(Functional = "Trophic preference")
colnames(data_functional_troph_edt)[3] <- "Category"

data_functional_migratory_edt <- data_functional_migratory_total %>% cbind(Functional = "Migratory preference")
colnames(data_functional_migratory_edt)[3] <- "Category"

data_functional_nocturnal_edt <- data_functional_nocturnal_total %>% cbind(Functional = "Nocturnal Fish")
data_functional_nocturnal_edt$Nocturnal_fish[data_functional_nocturnal_edt$Nocturnal_fish == 'Yes'] <- 'Nocturnal'
colnames(data_functional_nocturnal_edt)[3] <- "Category"

join_functional <- full_join(data_functional_habitat_edt, data_functional_troph_edt)
join_functional <- full_join(join_functional, data_functional_migratory_edt)
join_functional <- full_join(join_functional, data_functional_nocturnal_edt)

levels(join_functional$Site)
levels(join_functional$Site) <- c("Taka Malang (Core)", "Menjangan Kecil  (Protection)", "Cilik (Tourism)", "Genting (Open access)")

join_functional$Category <- factor(join_functional$Category, 
                                   levels = c("reef-associated", "demersal", "benthopelagic", "pelagic-neritic", "pelagic-oceanic", "bathydemersal", "bathypelagic",
                                              "non-migratory", "oceanodromous", "amphidromous", "Carnivore", "Herbivore", "Omnivore", "Nocturnal","Unknown"))

functional_plot <- ggplot(data = join_functional, aes(x=Category, y=total_count, fill = Data)) +
  geom_bar(stat = "identity", position = position_dodge(), alpha = .9) +
  geom_text(aes(label = total_count), position=position_dodge(width=1), vjust=-0.3, size =5) +
  scale_fill_manual(values=c("#1AFF1A", "#4B0092"),
                    labels = c('eDNA', 'Visual Census'), name = NULL) +
  scale_y_continuous(expand = c(0.05, 0), limits=c(0, 95)) +
  labs(y = "Number of Species", x = "", size = 16) +
  facet_grid(Site ~ Functional, scales = "free", space = "free")

functional_plot + theme_bw() + theme(legend.position='bottom', axis.text.x = element_text(size=18, face = "bold", angle = 50, hjust = 1),
                                     axis.text.y = element_text(size=14),
                                     strip.text.x = element_text(size = 16, face = "bold"),
                                     strip.text.y = element_text(size = 14, face = "bold"),
                                     legend.text = element_text(size=16)) 


table_functional_eDNA_vc <- full_join(eDNA_data_functional, vc_data_functional)
write.csv2(table_functional_eDNA_vc, "table_functional_eDNA_vc.csv", row.names = F)
