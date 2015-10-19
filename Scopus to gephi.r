setwd("---------")# change for any folder you are using for your project
library(httr)
library(jsonlite)
install.packages("igraph")# just install it one time :D 
library(igraph)

#function to change null results to NA, this is important for formatting the data
null2na<- function(input){# function to transfor NULL to NA to keep them after unlisting
    input[unlist(lapply(input, is.null))] <- NA
    output<-unlist(input)
    return(output)
}

#This function gives me the other set of search results missing from the first call
#This is neccesary becasue the API gives you the results by page not full. 
scopusPage <- function(t=0,p=25,Q="TITLE-ABS-KEY({Big data}) AND (DOCTYPE(ar) OR DOCTYPE(ip) OR DOCTYPE(cp) )", D="2015"){ 
    t<-as.numeric(t)
    p<-as.numeric(p)
    #give more
    st=t-p
    #calculate the number of times to do the loop
    times<-ceiling(st/p)
    output<-vector("list",times)
    #making a tracking bar
    pb <- winProgressBar(title = "progress bar", min = 0,
                         max = times, width = 300)
    #do the loop
    for(i in 1:times){
        ind<-p*i
        r <- GET("http://api.elsevier.com/content/search/scopus",
                 query = list(apiKey="-------",
                              query=paste(Q),
                              date=paste(D),
                              field="dc:title,dc:description,authkeywords,intid,citedby-count,prism:doi",
                              start=paste(ind),
                              count="200"
                 )
        )
        #visualizing the tracking bar
        setWinProgressBar(pb, i, title=paste( round(i/times*100, 0),
                                              "% done"))
        #
        buff1=content(r)
        buff3=jsonlite::fromJSON(toJSON(buff1))
        output[i]<-buff3
        #returns a list of all the json objects
        }
    close(pb)
    return(output)
}

#Gets the research results from scopus and put them into a matrix 
# Parameters Q for the query you want to do to scopus
# Paramater D to select the year that you want the data from.
# as I explained you can expand the function to bring the fields you want. 
# super important to be able to use Scopus API you have to make an account 
# they will give you and API key that allows you to use their API 
# that key is neccesary for make querys to scopus
# you can get that API key form here http://dev.elsevier.com/myapikey.html

scopusSearch<- function(Q="TITLE-ABS-KEY({Big data}) AND (DOCTYPE(ar) OR DOCTYPE(ip) OR DOCTYPE(cp) )", D="2015"){
    r <- GET("http://api.elsevier.com/content/search/scopus",
             query = list(apiKey="--insert here your scopus apikey---",
                          query=paste(Q),
                          date=paste(D),
                          field="dc:title,dc:description,authkeywords,intid,citedby-count,prism:doi",
                          count="200"
             )
    )
    json1=content(r)
    json3=jsonlite::fromJSON(toJSON(json1))
    
    
    outputTitle<-null2na(json3$`search-results`$entry["dc:title"][[1]])
    outputAbs<-null2na(json3$`search-results`$entry["dc:description"][[1]])
    outputKw<-null2na(json3$`search-results`$entry["authkeywords"][[1]])
    outputCited<-null2na(json3$`search-results`$entry["citedby-count"][[1]])
    outputScore<-null2na(json3$`search-results`$entry["intid"][[1]])
    outputDOI<-null2na(json3$`search-results`$entry["prism:doi"][[1]])
    
    total<-as.numeric(json3$`search-results`$`opensearch:totalResults`)
    pag<-as.numeric(json3$`search-results`$`opensearch:itemsPerPage`)
    
    # do we need more pages of data? 
    if(total>pag){
        x<-scopusPage(total,pag,Q,D)
        for(i in 1:length(x)){
            outputTitle<-c(outputTitle,null2na(x[[i]]$entry["dc:title"][[1]]))
            outputAbs<-c(outputAbs,null2na(x[[i]]$entry["dc:description"][[1]]))
            outputKw<-c(outputKw,null2na(x[[i]]$entry["authkeywords"][[1]]))
            outputCited<-c(outputCited,null2na(x[[i]]$entry["citedby-count"][[1]]))
            outputScore<-c(outputScore,null2na(x[[i]]$entry["intid"][[1]]))
            outputDOI<-c(outputDOI,null2na(x[[i]]$entry["prism:doi"][[1]]))
        }
    }
    output<-cbind(outputTitle,outputAbs,outputKw,outputCited,outputScore,outputDOI)
    return(output) 
}

#Getting the non NA KW fields
getnoNAKw<-function(x){output <- x[!is.na(x[,3]),]}#ab

#A function to work on keywords
docvsKw<-function(ac, rmkey="big data"){#ac
    z<-unlist(strsplit(ac[,3],"|",fixed = TRUE)) # sapply to get the big vector
    #Data processing
    ## lower case
    zl<-tolower(z)
    ## remove trailing and leading white space
    zlt<-zl<-trim(zl)
    ## getting unique keywords
    zltu<-unique(zlt)
    #rmkey="big data"
    zltue <- zltu[zltu != rmkey]
    ## Elmination of selected keywords. 
    
    ## in case of weird characters eliminate them
    #Creating the Doc Vs word matrix
    dvk<-matrix(0,nrow = dim(ac)[1], ncol=length(zltue), dimnames = list(c(1:dim(ac)[1]),zltue))

    for(i in 1:dim(ac)[1]){
        yl<-tolower(unlist(strsplit(ac[i,3],"|",fixed = TRUE)))
        ylt<-trim(yl)
        ##This line is for the regular expression matching between keywords.
        #thing<-unlist(sapply(ylt,grep,x=zltue,fixed = TRUE,simplify = TRUE))
        ##-------------------------------------------------------------------
        thing<-vector("numeric")
        for(j in 1:length(ylt)){
            #buff<-grep(ylt[j],zltue,fixed = TRUE)
            buff<-match(ylt[j], zltue)##This lines is for exact match keywords
            thing<-c(thing,buff)
        }
        ##-------------------------------------------------------------------
        dvk[i,thing]<-1
    }
    return(dvk)
    #return(thing)
}
#-----------------------------------------------------
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
#------------------------------------------------------
#Execution
ab<-scopusSearch(D="2012")
ab<-scopusSearch(D="2013")
ab<-scopusSearch(D="2014")
ac<-getnoNAKw(ab)

ad<-docvsKw(ac)
adsum<-colSums(ad)#raw matrix 
adop<-ad[,adsum>1]#optimizated matrix
adjm<-t(adop)%*%(adop)# adjacency matrix
#Export to gephi for visualization
#---------------------------------
#Build the Graph...
g1 <- graph.adjacency(adjm, mode="undirected",weighted="c", diag=FALSE)#,add.colnames="Names")# here we get the weights as an edge attribute, to use them later on gephi. in this step was the problem, the NA value was taken as a valid connection for the network!!
#------------Working with community detection using igraph----------------#
#Edgebetweenness community detection. weight: on 
ceb<-cluster_edge_betweenness(g1,directed = FALSE,edge.betweenness = FALSE, 
                              merges = FALSE, bridges = FALSE,
                              modularity = TRUE, membership = TRUE)

##------------calculating the nodes properties np------------------###
#Betweenness<-npb weight: on
npb<-betweenness(g1,directed = FALSE, normalized = TRUE)# gives in the same order
npd<-degree(g1,mode = c("all"))#,loops = TRUE, normalized = FALSE)
##node role in cluster

#--------------Exporting node list ot gephi------------------------
npcm<-as.numeric(membership(ceb))
np<-cbind(as.character(unlist(attributes(npb)[1])),as.character(unlist(attributes(npb)[1])),as.numeric(npb),as.numeric(npd),as.numeric(npcm))
colnames(np) <- c("Id","Label","Betweenness Centrality","Degree","Modularity Class")
write.csv(np,"nodesop2012.csv")
#---------------------Exporting edge list------------------------
#export the list to be used in gephi for better visualization options...
edge1 <- get.edgelist(g1, names=TRUE)# getting the Edgelist
edgeW <- get.edge.attribute(g1,"c", index=E(g1))# getting the weights of each link
nT<-vector(mode = "character",length=length(edgeW))
nT[]<-"Undirected"
edgefull1= cbind(edge1,edgeW,nT)# joining them 
dimnames(edgefull1) = list(NULL,c("Source","Target","Weight","Type"))# preparing the file
write.csv(edgefull1,"edgeop2012.csv")
#----------------------------------------------------------------------
