##################################################
#     draw rooted highltighted species tree      #
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
p<-ggtree(species_tree.rooted, size=2)
p <- p %<+% dd + geom_tippoint(aes(color=tipcols), size=5)
p2<-p + geom_tiplab(size=6, aes(color=tipcols), offset=0.002, show.legend=FALSE) + 
  scale_color_manual(values=c("cornflowerblue", "limegreen", "orchid3"), limits = c("Outgroup", "Ultramafic", "Volcanic"), na.value = "grey77") + 
  theme(legend.title = element_blank(),
        legend.text = element_text(size=10)) +
  expand_limits(x = 0.1)

p2


d3 <- data.frame(id = rep(species_tree.rooted$tip.label, each=2),
                 value = abs(rnorm(56, mean=100, sd=50)),
                 category = rep(LETTERS[1:2], 28))


p3<-p2 + geom_facet(panel = 'bar', data = d3, geom = geom_bar, 
               mapping = aes(x = value, fill = as.factor(category)), 
               orientation = 'y', width = 0.8, stat='identity')

facet_widths(p3, widths = c(2, 1))


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


