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


# ----------------- ITEM-BASED -------------------

# Create a helper function to calculate the cosine between two vectors
getCosine = function(x,y) 
{
  this.cosine = sum(x*y) / (sqrt(sum(x*x)) * sqrt(sum(y*y)))
  return(this.cosine)
}

item_based = function(df){
  # Drop any column named "user"
  df.ibs = (df[,!(names(df) %in% c("user_id"))])
  
  # Create a placeholder dataframe listing item vs. item
  df.ibs.similarity  = matrix(NA, nrow=ncol(df.ibs),ncol=ncol(df.ibs),dimnames=list(colnames(df.ibs),colnames(df.ibs)))
  
  # Lets fill in those empty spaces with cosine similarities
  # Loop through the columns
  for(i in 1:ncol(df.ibs)) {
    # Loop through the columns for each column
    for(j in 1:ncol(df.ibs)) {
      # Fill in placeholder with cosine similarities
      df.ibs.similarity[i,j] = getCosine(as.matrix(df.ibs[i]),as.matrix(df.ibs[j]))
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
  
  return(df.neighbours)
}

# --------------- END ITEM-BASED------------------



# ------MAIN------

#import data from csv files
books_rating = read_csv(fl="~/Documents/BX-CSV-Dump 2/BX-Book-Ratings.csv", rowLen = 20)

# get specified columns from imported data
col_br_user = books_rating[[1]]
col_br_isbn = books_rating[[2]]
col_br_rating = books_rating[[3]]

# Unique users and book's isbn
unique_user_id = sort(unique(col_br_user))
unique_book_id = sort(unique(col_br_isbn))

# Generating empty data frame
df_zero = generate_empty_df(unique_user_id, unique_book_id)

# Filling df_zero with ratings
df_complete = generate_complete_df(df_zero, col_br_user, col_br_isbn, col_br_rating)

# Writing results to a csv file
write_csv(fl="~/Documents/Mow_project/res.csv", df=df_complete)

# Item-based algorithm results
df_item_based = item_based(df_complete)


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
