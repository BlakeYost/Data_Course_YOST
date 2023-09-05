
# Assignment 2

# 4.list all csv files and store it in csv_files 
csv_files <- list.files(path="Data",pattern = ".csv" , recursive = TRUE, full.names = TRUE)

# 5. Find length of csv_files
length(csv_files)

# 6. Open the wingspan_vs_mass.csv file and store the contents as an R object named “df” using the read.csv() function
df <- read.csv("data/wingspan_vs_mass.csv")

# 7. Inspect the first 5 lines of this data set using the head() function
head(df, n=5)

# 8. Find any files (recursively) in the Data/ directory that begin with the letter “b” (lowercase)
x <- list.files(path="Data", pattern = "^b", recursive = TRUE,
                full.names = TRUE)

#9. Write a command that displays the first line of each of those “b” files
for (i in x){
  print(readLines(i, n=1))
}

# 10. Do the same thing for all files that end in “.csv”
for (i in csv_files){
  print(readLines(i, n=1))
}
