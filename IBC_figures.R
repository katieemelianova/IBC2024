
#######################################################################################################################################
#     get LTR and DNA transposon counts for species                                                                                   #
#    grep -e "Copia" -e "Gypsy" -e "DNA transposons" /lisc/scratch/botany/katie/assembled_genomes/assemblies/*/working/*tbl  
#.    pase output into this directory under "te_counts.txt"
#######################################################################################################################################

te_counts<-read.table("te_counts.txt") %>% 
  set_colnames(c("species", "class", "number", "length", "bp", "percent", "%")) %>% 
  dplyr::select(-c("bp", "%")) %>%
  mutate(species = case_when(grepl('impolita', species) ~ "D. impolita",
                          grepl('pancheri', species) ~ "D. pancheri",
                          grepl('revolutissima', species) ~ "D. revolutissima",
                          grepl('sandwicensis', species) ~ "D. sandwicensis",
                          grepl('vieillardii', species) ~ "D. vieillardii",
                          grepl('yahouensis', species) ~ "D. yahouensis"),
         class = ifelse(class == "transposons", "DNA transposon", class))


##################################################
#                read in species tree            #
################################################## 

species_tree<-ape::read.tree("/Users/katieemelianova/Desktop/Diospyros/diospyros_plots/lib1234_speciestree.editedTiplabs.nwk")
species_tree.rooted <- root(species_tree, which(species_tree$tip.label == "D.sandwicensis"))
tip<-c("D.olen", "D.fasciculosa", "D.macrocarpa", "D.ferrea")
species_tree.rooted<-drop.tip(species_tree.rooted, tip)
species_tree.rooted$species <- species_tree.rooted$tip.label
species_tree.rooted$tip.label
species_tree.rooted$tip.label<-str_replace(species_tree.rooted$tip.label, "D.", "D. ")

outgroup<-c("D. sandwicensis")
ultramafic<-c("D. vieillardii", "D. pancheri", "D. revolutissima")
volcanic<-c("D. impolita", "D. yahouensis")


colours_tips <- case_when(species_tree.rooted$tip.label %in% outgroup ~ "Outgroup",
                          species_tree.rooted$tip.label %in% ultramafic ~"Ultramafic",
                          species_tree.rooted$tip.label %in% volcanic ~"Volcanic",
                          !(species_tree.rooted$tip.label %in% c(outgroup, ultramafic, volcanic)) ~ "No data")


dd <- data.frame(taxa=species_tree.rooted$tip.label, tipcols=colours_tips)
p<-ggtree(species_tree.rooted, size=1)
p <- p %<+% dd + geom_tippoint(aes(color=tipcols), size=7)
p2<-p + geom_tiplab(size=8, aes(color=tipcols), offset=0.002, show.legend=FALSE) + 
  scale_color_manual(values=c("cornflowerblue", "limegreen", "orchid3"), limits = c("Outgroup", "Ultramafic", "Volcanic"), na.value = "grey77") + 
  theme(legend.title = element_blank(),
        legend.text = element_text(size=10)) +
  expand_limits(x = 0.07)


####################################################################
#    add in other species to TE counts to plot alongside tree      #
####################################################################

te_counts_total<-data.frame(species=rep(species_tree.rooted$tip.label, each=3),
           class=rep(te_counts$class %>% unique(), length(species_tree.rooted$tip.label)),
           number = rep(0, length(species_tree.rooted$tip.label) * 3),
           length = rep(0, length(species_tree.rooted$tip.label) * 3),
           percent = rep(0, length(species_tree.rooted$tip.label) * 3)) %>% 
  filter(!(species %in% te_counts$species)) %>%
  rbind(te_counts)

####################################################################
#                 plot tree alongaside TE abundance                #
####################################################################

p4<-p2 + geom_facet(panel = 'bar', data = te_counts_total, geom = geom_bar, 
                    mapping = aes(x = number, fill = as.factor(class)), 
                    orientation = 'y', width = 0.8, stat='identity') +
  scale_fill_manual(values = c("#F21A00", "#EBCC2A", "#3B9AB2")) +
  theme(strip.text.x = element_text(size = 22),
        legend.text = element_text(size=22),
        panel.border = element_rect(colour = "grey76", fill=NA, size=1))

p4<-facet_labeller(p4, c(Tree = "Species", bar = "Number of TEs"))

pdf("IBC2024_te_count_tree.pdf", width=12, height=10, onefile=FALSE)
facet_widths(p4, widths = c(3, 1))
dev.off()

pdf("IBC2024_tree.pdf", width=9, height=9, onefile=FALSE)
p2 +
  theme(legend.text = element_text(size=22),
        legend.position = c(.18, .90))
dev.off()


######################################################
#              read in tables of LTR %IDs           #
######################################################


impolita_ltr_distance<-read.table("ltr_lengths/impolita.fasta.mod.EDTA.intact.gff3.ltr_lengths") %>% mutate(species="D. impolita") %>% set_colnames(c("ltr_distance", "species"))
revolutissima_ltr_distance<-read.table("ltr_lengths/revolutissima.fasta.mod.EDTA.intact.gff3.ltr_lengths") %>% mutate(species="D. revolutissima") %>% set_colnames(c("ltr_distance", "species"))
vieillardii_ltr_distance<-read.table("ltr_lengths/vieillardii.fasta.mod.EDTA.intact.gff3.ltr_lengths") %>% mutate(species="D. vieillardii") %>% set_colnames(c("ltr_distance", "species"))
pancheri_ltr_distance<-read.table("ltr_lengths/pancheri.fasta.mod.EDTA.intact.gff3.ltr_lengths") %>% mutate(species="D. pancheri") %>% set_colnames(c("ltr_distance", "species"))
sandwicensis_ltr_distance<-read.table("ltr_lengths/sandwicensis.fasta.mod.EDTA.intact.gff3.ltr_lengths") %>% mutate(species="D. sandwicensis") %>% set_colnames(c("ltr_distance", "species"))
yahouensis_ltr_distance<-read.table("ltr_lengths/yahouensis.fasta.mod.EDTA.intact.gff3.ltr_lengths") %>% mutate(species="D. yahouensis") %>% set_colnames(c("ltr_distance", "species"))

ltr<-rbind(impolita_ltr_distance, 
      revolutissima_ltr_distance,
      vieillardii_ltr_distance,
      pancheri_ltr_distance,
      sandwicensis_ltr_distance,
      yahouensis_ltr_distance)


ltr$species <- factor(ltr$species, levels=c("D. yahouensis", "D. pancheri",
                                            "D. impolita", "D. revolutissima",
                                            "D. vieillardii", "D. sandwicensis"))


ltr %>% 
  ggplot(aes(x = ltr_distance, fill = species)) +
  geom_density(aes(y = after_stat(count)), alpha = 0.25) +
  facet_wrap(~ species, ncol=2) +
  scale_fill_manual(values=c("pink", "green", "pink", "green", "green", "beige")) +
  theme(legend.text = element_text(size=22),
        legend.position = "none",
        strip.text = element_text(size = 20))
  




######################################################
#      example of how to combine plots with trees    #
######################################################

tr <- rtree(30) 
p <- ggtree(tr) 
d1 <- data.frame(id=tr$tip.label, location=sample(c("GZ", "HK", "CZ"), 30, replace=TRUE)) 
p1 <- p %<+% d1 + geom_tippoint(aes(color=location)) 
d2 <- data.frame(id=tr$tip.label, val=rnorm(30, sd=3)) 
p2 <- facet_plot(p1, panel="dot", data=d2, geom=geom_point, aes(x=val), color="firebrick") + theme_tree2()
d3 <- data.frame(id = rep(tr$tip.label, each=2),
                 value = abs(rnorm(60, mean=100, sd=50)),
                 category = rep(LETTERS[1:2], 30))
p3 <- facet_plot(p2, panel = 'Stacked Barplot', data = d3, 
                 geom = geom_barh, 
                 mapping = aes(x = value, fill = as.factor(category)), 
                 stat='identity' ) 

#############################################################
#     another example of how to combine plots with trees    #
#############################################################


set.seed(123)
tree <- rtree(30)
p <- ggtree(tree, branch.length = "none") + 
  geom_tiplab() + theme(legend.position='none')
a <- runif(30, 0,1)
b <- 1 - a
df <- data.frame(tree$tip.label, a, b)
df <- melt(df, id = "tree.tip.label")
p2 <- p + geom_facet(panel = 'bar', data = df, geom = geom_bar, 
                     mapping = aes(x = value, fill = as.factor(variable)), 
                     orientation = 'y', width = 0.8, stat='identity') + 
  xlim_tree(9)


