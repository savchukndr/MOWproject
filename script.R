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

# ------MAIN------

#import data from csv files
books_rating = read_csv(fl="~/Documents/BX-CSV-Dump 2/BX-Book-Ratings.csv", rowLen = 10000)

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
# write_csv(fl="~/Documents/mowScript/res.csv", df=df_complete)

