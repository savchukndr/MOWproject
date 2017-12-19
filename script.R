# ------------------ DF GENERATOR ------------------

# Function for reading data from csv file
read_csv = function(fl, rowLen){
  read.csv(file=fl,nrows=rowLen, sep=";")
  return(read.csv(file=fl,nrows=rowLen, sep=";"))
}


# Write results into csv file
write_csv = function(fl, df){
  write.table(df, file=fl, sep=";")
}


# Function for genereting data frame
generate_empty_df = function(unique_user_id, unique_isbn){
  df = data.frame(user_id=c(unique_user_id))
  for (column in unique_isbn){
    df[[toString(column)]] = 0
  }
  return(df)
}


# Function for filling data frame
generate_complete_df = function(df, user_id, isbn, rating){
  
  # browser()
  # Write book ratings into data frame
  for (i in seq(length(rating))){
    cols = toString(isbn[[i]])
    row = which(df$user_id == user_id[[i]])
    df[[cols]][[row]] = rating[[i]]
  }
  return(df)
}

# -------------------------- END --------------------------------


# ----------------- ITEM-BASED AND USER-BASED -------------------

# Create a helper function to calculate the cosine between two vectors
get_cosine = function(x,y) 
{
  this.cosine = sum(x*y) / (sqrt(sum(x*x)) * sqrt(sum(y*y)))
  return(this.cosine)
}

# A helper function to calculate the scores
get_score <- function(history, similarities)
{
  x <- sum(history*similarities)/sum(similarities)
  x
}

colaborative_filtering = function(df){
  # Drop any column named "user_id"
  df.ibs = (df[,!(names(df) %in% c("user_id"))])
  
  # Create a placeholder dataframe listing item vs.item
  df.ibs.similarity  = matrix(NA, nrow=ncol(df.ibs),ncol=ncol(df.ibs),dimnames=list(colnames(df.ibs),colnames(df.ibs)))
  
  # Lets fill in those empty spaces with cosine similarities
  # Loop through the columns
  for(i in 1:ncol(df.ibs)) {
    # Loop through the columns for each column
    for(j in 1:ncol(df.ibs)) {
      # Fill in placeholder with cosine similarities
      df.ibs.similarity[i,j] = get_cosine(as.matrix(df.ibs[i]),as.matrix(df.ibs[j]))
    }
  }
  
  # Back to dataframe
  df.ibs.similarity = as.data.frame(df.ibs.similarity)
  
  # Get the top 10 neighbours for each
  df.neighbours = matrix(NA, nrow=ncol(df.ibs.similarity),ncol=11,dimnames=list(colnames(df.ibs.similarity)))
  
  for(i in 1:ncol(df.ibs)) 
  {
    df.neighbours[i,] = (t(head(n=11,rownames(df.ibs.similarity[order(df.ibs.similarity[,i],decreasing=TRUE),][i]))))
  }
  
  #################################
  
  # A placeholder matrix
  holder <- matrix(NA, nrow=nrow(df),ncol=ncol(df)-1,dimnames=list((df$user_id),colnames(df[-1])))
  
  # Loop through the users (rows)
  for(i in 1:nrow(holder)) 
  {
    # Loops through the products (columns)
    for(j in 1:ncol(holder)) 
    {
      # Get the user's name and th product's name
      # We do this not to conform with vectors sorted differently 
      user <- rownames(holder)[i]
      product <- colnames(holder)[j]
      
      # We do not want to recommend products you have already consumed
      # If you have already consumed it, we store an empty string
      if(as.integer(df[df$user_id==user,product]) == 1)
      { 
        holder[i,j]<-""
      } else {
        
        # We first have to get a product's top 10 neighbours sorted by similarity
        topN<-((head(n=11,(df.ibs.similarity[order(df.ibs.similarity[,product],decreasing=TRUE),][product]))))
        topN.names <- as.character(rownames(topN))
        topN.similarities <- as.numeric(topN[,1])
        
        # Drop the first one because it will always be the same song
        topN.similarities<-topN.similarities[-1]
        topN.names<-topN.names[-1]
        
        # We then get the user's purchase history for those 10 items
        topN.purchases<- df[,c("user_id",topN.names)]
        topN.userPurchases<-topN.purchases[topN.purchases$user_id==user,]
        topN.userPurchases <- as.numeric(topN.userPurchases[!(names(topN.userPurchases) %in% c("user_id"))])
        
        # We then calculate the score for that product and that user
        holder[i,j]<-get_score(similarities=topN.similarities,history=topN.userPurchases)
        
      } # close else statement
    } # end product for loop   
  } # end user for loop
  
  df.user.scores <- holder
  
  # Lets make our recommendations pretty
  df.user.scores.holder <- matrix(NA, nrow=nrow(df.user.scores),ncol=100,dimnames=list(rownames(df.user.scores)))
  for(i in 1:nrow(df.user.scores)) 
  {
    df.user.scores.holder[i,] <- names(head(n=100,(df.user.scores[,order(df.user.scores[i,],decreasing=TRUE)])[i,]))
  }
  
  return(list(df.neighbours, df.user.scores.holder))
}

# -------------------- END -----------------------


# -------------------- MAIN ----------------------

#import data from csv files
books_rating = read_csv(fl="~/Documents/BX-CSV-Dump 2/BX-Book-Ratings.csv", rowLen = 1000)
#books_rating = read_csv(fl="~/Documents/MOWproj/BX-Book-Ratings.csv", rowLen = 35)

# get specified columns from imported data
col_br_user = books_rating[[1]]
col_br_isbn = books_rating[[2]]
col_br_rating = books_rating[[3]]

# Unique users and book's isbn
unique_user_id = sort(unique(col_br_user))
unique_book_id = sort(unique(col_br_isbn))

# Generating empty data frame
df.item.holder = generate_empty_df(unique_user_id, unique_book_id)

# Filling df.item.holder with ratings
df.item.complete = generate_complete_df(df.item.holder, col_br_user, col_br_isbn, col_br_rating)

# Writing results to a csv file
# write_csv(fl="~/Documents/Mow_project/res.csv", df=df.item.complete)

# List that contains results from item-based and user-based algorithms 
list_results = colaborative_filtering(df.item.complete)

# Writing results to a csv file
write_csv(fl="~/Documents/Mow_project/item.csv", df=list_results[[1]])
write_csv(fl="~/Documents/Mow_project/user.csv", df=list_results[[2]])

# Using the package

#setwd(some_path/MOWproject)
install.packages('comandante', repo=NULL, type='source')
library(comandante)

# Constructor should at least take additional argument with user-item matrix
m <- new('RecommendationsModel')
# Give 5 recommendations to users of IDs 1, 2 and 3
recommendations <- Recommend(m, userList=c(1, 2, 3), n=5)
# Predict what rating will user 1 give to item 1
rating <- PredictRating(m, 1, 1)
