#Packages
library(microbiome)
library(phyloseq)
library(tidyverse)
library(ape)
library(ggvenn)
library(ggpubr)
library(readxl)
library(vegan)
library(FSA)
library(car)
library(agricolae)
library(microViz)
library(ggside)
library(VennDiagram)
library(wesanderson)


setwd("D:/S2_UNDIP/Riset_S2_Danie/Data_Analysis")

otu_table <- read.csv("D:/S2_UNDIP/Riset_S2_Danie/Data_Analysis/otu_table.csv", row.names=1)
taxon_table <- read.csv("D:/S2_UNDIP/Riset_S2_Danie/Data_Analysis/taxon_table.csv", row.names=1)
taxon_table <- as.matrix(taxon_table)
metadata <- read.csv("D:/S2_UNDIP/Riset_S2_Danie/Data_Analysis/metadata.csv", row.names=1)


#   Import as phyloseq objects
OTU = otu_table(otu_table, taxa_are_rows=TRUE)
TAX = tax_table(taxon_table)
META = sample_data(metadata)

phy_taxa_indo = phyloseq(OTU, TAX, META)
phy_taxa_indo

# clean any possible non-fish taxa
phy_taxa_indo_clean <- phy_taxa_indo %>% subset_taxa(
  Class != "Mammalia"); phy_taxa_indo_clean

phy_taxa_indo_clean <- phy_taxa_indo_clean %>% subset_taxa(
  Class != "Lepidosauria"); phy_taxa_indo_clean

phy_taxa_indo_clean <- phy_taxa_indo_clean %>% subset_taxa(
  Class != "Amphibia"); phy_taxa_indo_clean

phy_taxa_indo_clean <- phy_taxa_indo_clean %>% subset_taxa(
  Class != "Aves"); phy_taxa_indo_clean

#Remove Singletons and Zeros
phy_taxa_indo_clean = filter_taxa(phy_taxa_indo_clean, function(x) sum(x) > 1, TRUE)
phy_taxa_indo_clean

ssum <- sum(sample_sums(phy_taxa_indo_clean)); ssum
sample_sums(phy_taxa_indo_clean)
get_taxa_unique(phy_taxa_indo_clean, "Species")

#relative abundace plot
family_phy <- phy_taxa_indo_clean %>%
  tax_glom(taxrank = "Family") %>%                         # agglomerate at Class level
  psmelt() %>%                                            # Melt to long format
  filter(Abundance > 0.00) %>%                            # Filter out low abundance taxa
  arrange(Class)  

family_phy <- family_phy %>%
  as.tibble() %>%
  arrange(desc(Abundance)) %>%
  mutate(Abundance_in_percent = Abundance * 100) %>%
  select(Zone, Family, MPA, Abundance) %>%
  data.frame()

family_phy <- family_phy %>% group_by(Zone ,Family, MPA) %>% summarize_if(is.numeric, sum) %>% ungroup()

Core_abun <- family_phy %>% filter(Zone == "Core") %>% mutate(Abundance_in_percent = Abundance/sum(Abundance)* 100) 
Protection_abun <- family_phy %>% filter(Zone == "Protection") %>% mutate(Abundance_in_percent = Abundance/sum(Abundance)* 100) 
Tourism_abun <- family_phy %>% filter(Zone == "Tourism") %>% mutate(Abundance_in_percent = Abundance/sum(Abundance)* 100) 
Open_abun <- family_phy %>% filter(Zone == "Open access") %>% mutate(Abundance_in_percent = Abundance/sum(Abundance)* 100) 

family_rel_abun <- full_join(Core_abun, Protection_abun)
family_rel_abun <- full_join(family_rel_abun, Tourism_abun)
family_rel_abun <- full_join(family_rel_abun, Open_abun)


#family_rel_abun$Family[family_rel_abun$Abundance_in_percent <= 2] <- "Z_Other"

#family_colors <- c(
  #"#00998F", "#003380","#FFA405",
  #"#990000", "#673770","#D14285", 
  #"#5E738F","#426600", "darkturquoise", 
  #"darkkhaki", "#993F00","#2BCE48","#FFA8BB",
  #"cornflowerblue","#C20088","darkgrey")

#p <- ggplot(family_rel_abun, aes(x = factor(Zone, level=c('Core', 'Protection', 'Tourism', 'Open access')), y = Abundance_in_percent, fill = Family)) +
  #geom_col()+
  #scale_fill_manual(values= family_colors, labels=c("Z_Other" = "Other")) +
  #guides(fill = guide_legend(reverse = FALSE, keywidth = 1, keyheight = 1)) +
  #xlab("") +
  #ylab("Relative Abundance (%)") 

#p +theme_bw() +theme(axis.text.x = element_text(size=10, angle=0, hjust=0.5, vjust=0, family="serif"), 
                     #axis.text.y = element_text(family="serif"),
                     #legend.text = element_text(family="serif"),
                     #legend.title = element_text(family="serif"),
                     #legend.position="right") 


plot_bubble <- ggplot(family_rel_abun, aes(x=factor(Zone, level=c('Core', 'Protection', 'Tourism', 'Open access')), y=Family, size=Abundance_in_percent, color = MPA)) +
  geom_point(alpha= 1) +
  scale_size(range = c(1, 20), breaks = c(0.1, 1, 10, 20, 30, 40, 50), "Relative Abundance %") +
  theme(axis.text.x = element_text(size=14, angle=0, hjust=0.5, vjust=0, family="serif"), 
        axis.text.y = element_text(size=14, family="serif"),
        legend.text = element_text(family="serif"),
        legend.title = element_text(family="serif"),
        panel.background = element_rect(fill = "white", colour = "grey50")) +
  theme(axis.title.y = element_text(size=14, vjust = +3)) +
  theme(axis.title.x = element_text(size=12, vjust = -2)) +
  ylab("Family") +
  xlab("") ; plot_bubble

plot_bubble + theme_bw() + theme(axis.text.x = element_text(size=12, angle=0, hjust=0.5, vjust=0, family="serif"), 
                               axis.text.y = element_text(size=12, family="serif")) +
  guides(color = guide_legend(override.aes = list(size = 10))) 

#### Alpha Diversity
plot_alpha <- plot_richness(phy_taxa_indo_clean, x= "Zone", measures= c("Observed", "Shannon")) +
  theme(axis.text.x = element_text(angle = 0, hjust=0.9, vjust=0)) +
  geom_boxplot(size=0.7, alpha=0.5) +
  stat_summary(fun.y = mean, geom = "point",
               shape = 18, size = 3, color = "black") +
  theme(legend.position="bottom") +
  theme_bw()

order_Zone <- c('Core', 'Protection', 'Tourism', 'Open access') 
p1$data$Zone <- as.character(p1$data$Zone)
p1$data$Zone <- factor(p1$data$Zone, levels=order_Zone)
print(p1)

alpha.diversity <- estimate_richness(phy_taxa_indo_clean,
                                     measures = c("Observed", "Shannon"))


data <- cbind(sample_data(phy_taxa_indo_clean), alpha.diversity)

levene <- leveneTest(Observed ~ Zone, data) #change location to your variable of interest

physeq.anova <- aov(Shannon ~ Zone, data)
summary(physeq.anova) ## significant/not significant effect of zone on richness

tukey_result <- HSD.test(physeq.anova, "Zone", group = TRUE)
print(tukey_result)
TukeyHSD(physeq.anova)

#### Beta Diversity
ps_clr <- microbiome::transform(phy_taxa_indo_clean, "clr")  

clr_dist_matrix <- phyloseq::distance(ps_clr, method = "euclidean") 
#ADONIS test
vegan::adonis(clr_dist_matrix ~ phyloseq::sample_data(ps_clr)$Zone)

dispr <- vegan::betadisper(clr_dist_matrix, phyloseq::sample_data(ps_clr)$Zone)
permutest(dispr)
vegan::adonis2(clr_dist_matrix ~ phyloseq::sample_data(ps_clr)$Zone)

phy_taxa_indo_clean %>%
  tax_transform("clr") %>%
  dist_calc(dist = "euclidean") %>%
  ord_calc("PCoA") %>%
  ord_plot(color = "Zone", shape = "MPA", size = 4) +
  scale_colour_brewer(palette = "Dark2", aesthetics = c("fill", "colour")) +
  geom_polygon(aes(fill=Zone), alpha = .5) +
  theme_bw()


?dist_calc()

#### Ven Diagram
# Load species
physeq_species <- phy_taxa_indo_clean %>%
  tax_glom(taxrank = "Species") %>%                         # agglomerate at Class level
  transform_sample_counts(function(x) {x/sum(x)} ) %>%    # Transform to rel. abundance
  psmelt() %>%                                            # Melt to long format
  filter(Abundance > 0.00) %>%                            # Filter out low abundance taxa
  arrange(Species) 

taxon_visual_census <- read.csv("D:/S2_UNDIP/Riset_S2_Danie/Data_Analysis/taxon_visual_census.csv")

##all location
list_VC <- paste(taxon_visual_census$Species)
list_eDNA <- paste(unique(physeq_species$Species))

x <- list(
  eDNA = sample(list_eDNA),
  VisualCensus= sample(list_VC)
)

venn_species_all_location <- ggvenn(x,
                                    fill_color = c("#1AFF1A", "#4B0092"),
                                    fill_alpha = .6,
                                    text_size = 6,
                                    stroke_linetype = 1,
                                    stroke_size = 1,
                                    set_name_size = 4,
                                    show_percentage = F,
                                    auto_scale = F); venn_species_all_location


## Core Zone 
physeq_species_core_zone <- physeq_species  %>% filter(Zone == "Core")

vc_table_tk_malang <- taxon_visual_census %>% filter(Zone == "Core")

list_VC <- paste(vc_table_tk_malang$Species)
list_eDNA <- paste(unique(physeq_species_core_zone$Species))

x <- list(
  eDNA = sample(list_eDNA),
  VisualCensus = sample(list_VC)
)

venn_species_core_zone <- ggvenn(x,
                                 fill_color = c("#1AFF1A", "#4B0092"),
                                 fill_alpha = .6,
                                 text_size = 6,
                                 stroke_linetype = 1,
                                 stroke_size = 1,
                                 set_name_size = 4,
                                 show_percentage = F,
                                 auto_scale = F); venn_species_core_zone

## protection Zone 
physeq_species_protection_zone <- physeq_species  %>% filter(Zone == "Protection")

vc_table_men_kecil <- taxon_visual_census %>% filter(Zone == "Protection")

list_VC <- paste(vc_table_men_kecil$Species)
list_eDNA <- paste(unique(physeq_species_protection_zone$Species))

x <- list(
  eDNA = sample(list_eDNA),
  VisualCensus = sample(list_VC)
)

venn_species_protection_zone <- ggvenn(x,
                                       fill_color = c("#1AFF1A", "#4B0092"),
                                       fill_alpha = .6,
                                       text_size = 6,
                                       stroke_linetype = 1,
                                       stroke_size = 1,
                                       set_name_size = 4,
                                       show_percentage = F,
                                       auto_scale = F); venn_species_protection_zone

## tourism Zone 
physeq_species_tourism_zone <- physeq_species  %>% filter(Zone == "Tourism")

vc_table_cilik <- taxon_visual_census %>% filter(Zone == "Tourism")

list_VC <- paste(vc_table_cilik$Species)
list_eDNA <- paste(unique(physeq_species_tourism_zone$Species))

x <- list(
  eDNA = sample(list_eDNA),
  VisualCensus = sample(list_VC)
)

venn_species_tourism_zone <- ggvenn(x,
                                    fill_color = c("#1AFF1A", "#4B0092"),
                                    fill_alpha = .6,
                                    text_size = 6,
                                    stroke_linetype = 1,
                                    stroke_size = 1,
                                    set_name_size = 4,
                                    show_percentage = F,
                                    auto_scale = F); venn_species_tourism_zone

## open Zone 
physeq_species_open_zone <- physeq_species  %>% filter(Zone == "Open access")

vc_table_genting_1 <- taxon_visual_census %>% filter(Zone == "Open access")

list_VC <- paste(vc_table_genting_1$Species)
list_eDNA <- paste(unique(physeq_species_open_zone$Species))

x <- list(
  eDNA = sample(list_eDNA),
  VisualCensus = sample(list_VC)
)

venn_species_open_zone <- ggvenn(x,
                                 fill_color = c("#1AFF1A", "#4B0092"),
                                 fill_alpha = .6,
                                 text_size = 6,
                                 stroke_linetype = 1,
                                 stroke_size = 1,
                                 set_name_size = 4,
                                 show_percentage = F,
                                 auto_scale = F); venn_species_open_zone

###visual with one frame
ggarrange(venn_species_all_location,                                                
          ggarrange(venn_species_core_zone, venn_species_protection_zone, venn_species_tourism_zone, venn_species_open_zone, 
                    ncol = 4, labels = c("b. Core", "c. Protection", "d. Tourism", "e. Open access")), 
          nrow = 2, 
          labels = "a. All location"                                       
)

###Diagram venn Family
##all location
eDNA <- paste(unique(physeq_species$Family))
VisualCensus <- paste(taxon_visual_census$Family)


# Generate plot
v <- venn.diagram(list(eDNA=eDNA, VisualCensus=VisualCensus),
                  fill = c("#1AFF1A", "#4B0092"),
                  text_size = 20, filename=NULL)

# have a look at the default plot
grid.newpage()
grid.draw(v)

# have a look at the names in the plot object v
lapply(v,  names)
# We are interested in the labels
lapply(v, function(i) i$label)

# Over-write labels (5 to 7 chosen by manual check of labels)
# in foo only
v[[5]]$label  <- paste(setdiff(eDNA, VisualCensus), collapse="\n")  
# in baa only
v[[6]]$label <- paste(setdiff(VisualCensus, eDNA)  , collapse="\n")  
# intesection
v[[7]]$label <- paste(intersect(eDNA, VisualCensus), collapse="\n")  

# plot  
grid.newpage()
grid.draw(v)

