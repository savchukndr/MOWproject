# Function for reading data from csv file
read_csv = function(fl, rowLen){
  read.csv(file=fl,nrows=rowLen, sep=";")
  return(read.csv(file=fl,nrows=rowLen, sep=";"))
}


# Write results into csv file
write_csv = function(fl, df){
  write.table(df, file=fl, sep=";")
}


# Function for generating data frame
generate_df = function(book_id, user_id, br_isbn, br_user, br_rating){
  
  # Creating data frame with '0' values
  df = data.frame(user_id=c(user_id))
  for (column in book_id){
    df[[column]] = 0
  }
  
  # Write book ratings into data frame
  for (i in seq(length(br_user))){
    cols = toString(br_isbn[[i]])
    rows = br_user[[i]]
    df[[cols]][[rows]] = br_rating[[i]]
  }
  return(df)
}

# ------MAIN------

#import data from csv files
books_rating = read_csv(fl="~/Documents/MOWproj/BX-Book-Ratings.csv",rowLen=39)
users = read_csv(fl="~/Documents/MOWproj/BX-Users.csv",rowLen=20)
books = read_csv(fl="~/Documents/MOWproj/BX-Books.csv",rowLen=10)

# get specified columns from imported data
col_br_user = books_rating[[1]]
col_br_isbn = books_rating[[2]]
col_br_rating = books_rating[[3]]
col_users_id = users[[1]]
col_books_id = books[[1]]

# Generating converted data frame
df = generate_df(col_books_id, col_users_id, col_br_isbn, col_br_user, col_br_rating)

# Writing results to a csv file
write_csv(fl="~/Documents/mowScript/res.csv", df=df)

