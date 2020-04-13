# Sasha's Script April 12th, 2020
# Trying to see if I can remake the phylogenetic tree that Navneetha made and doesn't have code for
# Except she won't help and doesn't seem to recall any of the steps- so I'm pretty much just remaking my own tree

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
library(purrr)

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

# Going to work through the whole process with one species, then go back to do all of them
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
# I'm choosing the cytb gene since it's one of the most frequently used genetic loci to id species- I think it will give us a really nice tree
barnowl_search <-entrez_search(db="nuccore", term="barn owl[Organism] AND CYTB[Gene]") # So can search common name- don't need taxize
print(barnowl_search) #47 hits still
barnowl_seqs <- entrez_fetch(db="nuccore", id=barnowl_search$ids, rettype="fasta")
print(barnowl_seqs) # This gives us a lot of hits! Probably will just take the first for each one- seems like the easiest option (and I'm in a time crunch)

owl_summs <- entrez_summary(db="nuccore", id=barnowl_search$ids)
print(owl_summs)
titles <- extract_from_esummary(owl_summs, "title")
unname(titles)
# We can see the first hit is "Tyto alba cytochrome b (cytb) gene, partial cds; mitochondrial" 

# Try to just get the first hit sequence
owl_ids <- barnowl_search$ids[1]
print(owl_ids)
CYTBowl <- entrez_fetch(db="nuccore", id=owl_ids, rettype="fasta")
print(CYTBowl) # YAY so that's the cytochrome b gene of a barn owl

# Now I just need to get this for all the species....
# If I can just get the NCBI ids (first hit) of CYTB in all the species then I can put it into a vector and into entrez_fetch

# Test to see if I can use a vector of terms in entrez search
testlist<-c("barn owl[Organism] AND CYTB[Gene]","red-tailed hawk[Organism] AND CYTB[Gene]")
barnowl_search <-entrez_search(db="nuccore", term=testlist) # It says value must be one
# So it looks like I need to search each species one at a time
barnowl_search <-entrez_search(db="nuccore", term=testlist[1]) #this works though so I could make a vector and use a loop
print(barnowl_search) #47 hits still

# Do I need to make all my species lowercase? Test it out
barnowl_search <-entrez_search(db="nuccore", term="BARN OWL[Organism] AND CYTB[Gene]")
print(barnowl_search)
# Okay cool upper case searches work well too

# Okay first I need to append [Organism] AND CYTB[Gene] to each species name
str(Species)
# Try this
SpeciesSearch <- paste(Species, "[Organism] AND CYTB[Gene]", sep="")
SpeciesSearch # Looks good
length(SpeciesSearch)

# Now write loop
# Need to incorportate aspect for species with no hits- for example "SPARROW"
idList<-c()
SpeciesFoundList<-c()
SpeciesNotFoundList<-c()
for(i in 1:length(SpeciesSearch)){
  searchtest <-entrez_search(db="nuccore", term=SpeciesSearch[i])
  if(length(searchtest$ids)==0){
    SpeciesNotFoundList<-append(SpeciesNotFoundList, SpeciesSearch[i])
  }else{
    idList<-append(idList, searchtest$ids[1])
    SpeciesFoundList<-append(SpeciesFoundList, SpeciesSearch[i])
  }
}
length(idList)
idList 
SpeciesFoundList
SpeciesNotFoundList

# Another test- troubleshooting
# Getting error Error in ans[[1]] : subscript out of bounds
idList<-c()
SpeciesFoundList<-c()
SpeciesNotFoundList<-c()
for(i in 1:length(SpeciesSearch)){
  searchtest <-entrez_search(db="nuccore", term=SpeciesSearch[i])
  if(length(searchtest$ids)==0){
    SpeciesNotFoundList<-append(SpeciesNotFoundList, SpeciesSearch[i])
  }else{
    idList<-append(idList, searchtest$ids[1])
    SpeciesFoundList<-append(SpeciesFoundList, SpeciesSearch[i])
  }
}
length(idList)
idList 
SpeciesFoundList
SpeciesNotFoundList
# Got the error again, but it seems to have made it a lot further through the list
# Made it through 687 of the 710 species
# So I'm going to move on for now, and come back later- hopefully ferris and orhun can help troubleshoot
# when I come back maybe try seq_along
###UPDATE: restarted R and the loop worked no problem

BirdCYTBList<-entrez_fetch(db="nuccore", id=idList, rettype="fasta")
# Got another error
#Error in entrez_check(response) : 
#HTTP failure 414, the request is too large. For large requests, try using web history as described in the rentrez tutorial

# Need to use web history
?entrez_post
upload <- entrez_post(db="nuccore", id=idList) # Still too large- going to need to split it up more
idListback<-idList
length(idListback)
str(idListback)
idListback2<-idListback[1:200]
idListback3<-idListback[200:428]

# Test lengths of about 200- go through it
?entrez_fetch
upload2 <- entrez_post(db="nuccore", id=idListback2) 
BirdCYTBList2<-entrez_fetch(db="nuccore", web_history = upload2, rettype="fasta")
head(BirdCYTBList2) # just one huge blob
str(BirdCYTBList2)

upload3 <- entrez_post(db="nuccore", id=idListback3) 
BirdCYTBList3<-entrez_fetch(db="nuccore", web_history = upload3, rettype="fasta")
head(BirdCYTBList3)

# AH so I'm realizing taking the first hit doesn't really work since sometimes the full mitochondrial genome pops up first
# So I need to find a way to use the species name and then "cytochrome b (cytb) gene, partial cds; mitochondrial" and then choose that hit
# So test out the first one again
SpeciesSearch[1]
searchtesestt <-entrez_search(db="nuccore", term=SpeciesSearch[1])
tester2<-strsplit(searchtesestt$QueryTranslation[1],"\"")
tester2[[1]][2] # that isolates the species name- in this case Zenaida macroura
# so to actually get what I want would be
see<-paste(tester2[[1]][2],"cytochrome b (cytb) gene, partial cds; mitochondrial")
# The question is how can I use this to get the specific hit
str(searchtesestt)
summs <- entrez_summary(db="nuccore", id=searchtesestt$ids)
titles <- extract_from_esummary(summs, "title")
unname(titles) # here you can see the third one is the one I want
# but gonna double check
check<-entrez_fetch(db="nuccore", id=searchtesestt$ids[3], rettype="fasta")
print(check)
# Okay cool
length(unname(titles))
unname(titles)[unname(titles)==see]
position<-match(see,unname(titles))
position
?detect

# Now need to incorporate this into my loop to double check it's the right sequence- not the full genome
idList<-c()
SpeciesFoundList<-c()
SpeciesNotFoundList<-c()
CYTBnotFound<-c()
for(i in 1:2){
  searchtest <-entrez_search(db="nuccore", term=SpeciesSearch[i])
  if(length(searchtest$ids)==0){
    SpeciesNotFoundList<-append(SpeciesNotFoundList, SpeciesSearch[i])
  }else{
    speciesname<-strsplit(searchtest$QueryTranslation[1],"\"")[[1]][2]
    check<-paste(speciesname,"cytochrome b (cytb) gene, partial cds; mitochondrial")
    summs <- entrez_summary(db="nuccore", id=searchtest$ids)
    titles <- extract_from_esummary(summs, "title")
    position<-match(check,unname(titles))
    idList<-append(idList, searchtest$ids[position])
    SpeciesFoundList<-append(SpeciesFoundList, SpeciesSearch[i])
  }
}

length(idList)
idList 
SpeciesFoundList
SpeciesNotFoundList

head(SpeciesSearch)

# So just in the preliminary test gull is NA
searchtesestt <-entrez_search(db="nuccore", term=SpeciesSearch[2])
print(searchtesestt)
tester2<-strsplit(searchtesestt$QueryTranslation[1],"\"")
tester2[[1]][2] 
see<-paste(tester2[[1]][2],"cytochrome b (cytb) gene, partial cds; mitochondrial")
str(searchtesestt)
summs <- entrez_summary(db="nuccore", id=searchtesestt$ids)
titles <- extract_from_esummary(summs, "title")
unname(titles) # here you can see the third one is the one I want
# but gonna double check
check<-entrez_fetch(db="nuccore", id=searchtesestt$ids[3], rettype="fasta")
print(check)
# Okay cool
length(unname(titles))
unname(titles)[unname(titles)==see]
position<-match(see,unname(titles))
position

# Still working on loop
idList<-c()
SpeciesFoundList<-c()
SpeciesNotFoundList<-c()
CYTBnotFound<-c()
for(i in 1:2){
  searchtest <-entrez_search(db="nuccore", term=SpeciesSearch[i])
  if(length(searchtest$ids)==0){
    SpeciesNotFoundList<-append(SpeciesNotFoundList, SpeciesSearch[i])
  }else{
    speciesname<-strsplit(searchtest$QueryTranslation[1],"\"")[[1]][2]
    check<-paste(speciesname,"cytochrome b (cytb) gene, partial cds; mitochondrial")
    summs <- entrez_summary(db="nuccore", id=searchtest$ids)
    titles <- extract_from_esummary(summs, "title")
    position<-match(check,unname(titles))
    if(is.na(position)){
      CYTBnotFound<-append(CYTBnotFound,SpeciesSearch[i])
    }else{
      idList<-append(idList, searchtest$ids[position])
      SpeciesFoundList<-append(SpeciesFoundList, SpeciesSearch[i])
    }
    
  }
}