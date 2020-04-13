# Sasha's Script April 12th, 2020
# Trying to see if I can remake the phylogenetic tree that Navneetha made and doesn't have code for

# The only bit of code from Navneetha- pretty useless - I tried running it and nothing works
# I don't think it's actually valid...missing important aspects
#nucleotide database (nuccore) and use history to retrieve all results
#birds_completegenome_search <- entrez_search(db="nuccore", term=birds_completegenome, use_history = T)
#birds_completegenome_search
#birds_completegenome_search$ids #gives you the NCBI ids
#birds_completegenome_search$web_history #retrieve from web history
#gets your sequences as a character vector
#seqs <- entrez_fetch(db="nuccore", rettype="fasta",web_history = T)
#seqs
#fasta<-write(seqs, "birds.fasta", sep="\n") #gets sequence to a fasta file
#run blast with 40 hits in blastn
#Readfile<-read.fasta(file=”birds.fasta”,package=”seqinr”), seqtype = c("DNA"), as.string = F, forceDNAtolower = F
#  blastSeq<- blastSequences (Readfile, database = "nr", hitListSize = "40",
#                             filter = "L", expect = "10", program = "blastn",
#                             attempts = 10)
#  baseUrl <- http://www.ncbi.nlm.nih.gov/blast/Blast.cgi
#  View(blastSeq)
#paste query sequence
#  query <- paste("QUERY=", as.character(Readfile), "&DATABASE=", database=”nr”,
#                 "&HITLIST_SIZE=", hitListSize=”40”, "&FILTER=", filter”L”, "&EXPECT=",
#                 Expect=”10”, "&PROGRAM=", program=”blastn”, sep = "")

# Looks like I'm starting from scratch

# Download packages
install.packages("taxize")

# Load libraries
library(rentrez)
library(taxize)

# Load data
data<-read.csv("NEwDataset.csv", header=TRUE, stringsAsFactors = FALSE, row.names = "X")
head(data)
colnames(data)
dim(data)
unique(data$Species.Name)
View(data)

# From Orhun's code
#Creating table arranged by frequency
X = table(data$Species.Name)
X = sort(table(data$Species.Name),decreasing=T)

#Removing Unknown species and renaming column
X = as.data.frame(X)
X = X[- grep("UNKNOWN", X$Var1),]
names(X)[names(X) == "Var1"] = "Species"
head(X)
dim(X)
Species<-X$Species

# I think I need to get the scientific names then the NCBI IDs
?taxize
?comm2sci
Species<-as.character(Species)
length(Species)
str(Species)
SpeciesSci<-comm2sci(Species,db="ncbi",itisby="search", simplify=TRUE)
# Didn't make it through all these species since got this error
# Also some common names like gull and dove don't seem to work
# Error: '{"error":"API rate limit exceeded","api-key":"65.93.252.175","count":"4","limit":"3"}
#' does not exist in current working directory ('C:/Users/sasha/Desktop/School 2019-2020/BIOL 432/Group-Project').
# Going to need to figure out this API stuff

# Rr it seems I can pass the common name directly into get_uid to get the taxonomy ID
get_uid("barn owl", modifier="Common Name")
str(get_uid("barn owl", modifier="Common Name"))
?get_uid
test<-get_uid("barn owl", modifier="Common Name")
print(test)
test[1] # would only want the first element which is the species ID
# Test this far
ttest<-entrez_fetch(db = "nuccore", id =test[1], rettype = "fasta")
print(ttest)
# This doesn't work since it's putting in the species ID

# Try something else
# Not sure
?entrez_search
?entrez_fetch
entrez_db_searchable("nuccore")
barnowlsci<-comm2sci("barn owl",db="ncbi",itisby="search", simplify=TRUE)
print(barnowlsci$`barn owl`)

# This seems to work but we get so many different genes- want to compare the same one across all species...
barnowl<-"Tyto alba[Organism]"
barnowl_search <- entrez_search(db="nuccore", term=barnowl)
print(barnowl_search)
barnowl_seqs <- entrez_fetch(db="nuccore", id=barnowl_search$ids, rettype="fasta")
print(barnowl_seqs)

# Try common name and CYTB gene
barnowl_search <-entrez_search(db="nuccore", term="barn owl[Organism] AND CYTB[Gene]") # So can search common name- don't need taxize
print(barnowl_search) #47 hits still
barnowl_seqs <- entrez_fetch(db="nuccore", id=barnowl_search$ids, rettype="fasta")
print(barnowl_seqs) # This gives us a lot of hits! Probably will just take the first for each one- seems like the easiest option (and I'm in a time crunch)

owl_summs <- entrez_summary(db="nuccore", id=barnowl_search$ids)
print(owl_summs)
