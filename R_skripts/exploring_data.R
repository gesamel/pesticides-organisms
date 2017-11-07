# setwd("D:/Gesa/Dokumente/Ecotoxicology/RPC/diatom_data")
diatom_dat<- read.table("C_Proben_Taxalisten.txt",header=T,sep="\t",dec=".")
# all samples from 2004 were removed from the excel table

head(diatom_dat)
taxa_list <-unique(diatom_dat$Taxon)  # Gets a list of the different diatom species in the dataset
taxa_list

taxa_cha <- as.character(taxa_list)
species_list <- sort(taxa_cha) #list of all species
species_list
nrow(as.data.frame(species_list))
# 1130 different species

taxa_temp <- sapply(strsplit(taxa_cha, ' '), '[', 1) 
genus_list<- sort(unique(taxa_temp))    #list of all genera
genus_list
nrow(as.data.frame(genus_list))
#80 different genera

# ----------------------------------------------------------------

# Trying out stuff subsetting:

class(diatom_dat$Datum)
diatom_dat[1:100,5]   #gets first 100 entries of column taxon
diatom_dat$Taxon[1:100] #same result
diatom_dat[1:100,c("Taxon")] #same result
diatom_dat[1:100, c("Deckung")]

str(diatom_dat) 
#Plotting
plot(diatom_dat$Deckung)
plot(diatom_dat[1:100, c("Deckung")])
class(diatom_dat$Deckung)
beanplot(diatom_dat$Deckung)
beanplot(diatom_dat[1:100, c("Deckung")])
windows()
plot(diatom_dat$Deckung ~ diatom_dat$Taxon)

# Wie ist die Deckung verteilt?
mean(diatom_dat$Deckung[diatom_dat$ID_PN=="BW_AI025.00_01.09.2006"])
mean_deckung1 <-tapply(diatom_dat$Deckung,diatom_dat$ID_PN, mean)
hist(mean_deckung1, main="Mean coverage of each species per sampling event", xlab="Mean coverage (%)")

mean_deckung2 <-tapply(diatom_dat$Deckung,diatom_dat$Taxon, mean)
hist(mean_deckung2, main="Mean coverage of each species per species", xlab="Mean coverage (%)")
# Most species are present at coverage < 5 %.
# Wie ist die Artenzusammensetzung an einem Probenahmeevent?
diatom_dat$Taxon[diatom_dat$ID_PN=="BW_AI025.00_01.09.2006"]
boxplot(Deckung~Taxon,data = diatom_dat)  # kann nicht alles darstellen dar zu viele arten

hist(diatom_dat$Deckung[diatom_dat$Taxon=="Diatoma vulgaris"], main="Coverage of Diatoma vulgaris", xlab="Coverage (%)")

# Wie haeufig ist welches taxon?
taxon_count <- table(diatom_dat$Taxon)
summary(taxon_count)
taxon_count <-as.data.frame(taxon_count)
head(taxon_count)
taxon_count[taxon_count$Freq<5,]
nrow(taxon_count[taxon_count$Freq<5,])
taxon_count[taxon_count$Freq<3,]
nrow(taxon_count[taxon_count$Freq<3,])
nrow(taxon_count[taxon_count$Freq<2,])
# 435 species occur at less than 5 sampling events.
# 308 species occur at less than 3 sampling events.
# 200 species occur at only 1 sampling event.
# 1130 species in total.
# Sollte ich seltene Arten weglassen? Wenn ja, welche Arten stufe ich als selten ein?
# Compare with:
# Cao, Y.; Larsen, D. P.; Thorne, R. S., Rare species in multivariate analysis for bioassessment: 
# some considerations. J. N. Am. Benthol. Soc. 2001, 20, (1), 144-153.
# Marchant, R., Do rare species have any place in multivariate analysis for bioassessment?
# J. N. Am. Benthol. Soc. 2002, 21, (2), 311-313.
# Rare species may especially cause problems in Correspondence Analysis and Canonical Correspondence Analysis

hist(taxon_count$Freq, breaks=20)