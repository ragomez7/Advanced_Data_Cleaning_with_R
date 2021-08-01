Advanced\_Data\_Cleaning\_on\_R
================
Rafael A. Gómez
7/30/2021

We show a method to quickly analyze all the column vectors in a dataset
to be able to discern inconsistencies that need to be cleaned.
Naturally, this extends the normal NA and facilitates determining other
missing values imputed differently, for example: ‘-999’,‘xxx’,‘?’, etc.

Read the CSV file.

``` r
df = read.csv("breast-cancer-wisconsin-data.csv")
```

Take a quick look.

``` r
head(df)
```

    ##        Id ClumpThickness UnifCellSize UnifCellShape MargAdhesion EpitCellSize
    ## 1 1002945              5            4             4            5            7
    ## 2 1015425              3            1             1            1            2
    ## 3 1016277              6            8             8            1            3
    ## 4 1017023              4            1             1            3            2
    ## 5 1017122              8           10            10            8            7
    ## 6 1018099              1            1             1            1            2
    ##   BareNuclei BlandChromatin NormalNucleoli Mitoses Class
    ## 1         10              3              2       1     2
    ## 2          2              3              1       1     2
    ## 3          4              3              7       1     2
    ## 4          1              3              1       1     2
    ## 5         10              9              7       1     4
    ## 6         10              3              1       1     2

``` r
tail(df)
```

    ##         Id ClumpThickness UnifCellSize UnifCellShape MargAdhesion EpitCellSize
    ## 693 763235              3            1             1            1            2
    ## 694 776715              3            1             1            1            3
    ## 695 841769              2            1             1            1            2
    ## 696 888820              5           10            10            3            7
    ## 697 897471              4            8             6            4            3
    ## 698 897471              4            8             8            5            4
    ##     BareNuclei BlandChromatin NormalNucleoli Mitoses Class
    ## 693          1              2              1       2     2
    ## 694          2              1              1       1     2
    ## 695          1              1              1       1     2
    ## 696          3              8             10       2     4
    ## 697          4             10              6       1     4
    ## 698          5             10              4       1     4

Create and run the function

``` r
get_unique_per_col = function(df){
    '
    Parameters:
      df - The R data.frame file.
    '
  columns = colnames(df)
  all_unique = vector(mode = "list")
  for(i in 2:ncol(df)){
    all_unique = append(all_unique,as.list(unique(df[columns[i]])))
  }
  #Printing all unique values per variable
  print(all_unique)
}
get_unique_per_col(df=df)
```

    ## $ClumpThickness
    ##  [1]  5  3  6  4  8  1  2  7 10  9
    ## 
    ## $UnifCellSize
    ##  [1]  4  1  8 10  2 NA  3  7  5  6  9
    ## 
    ## $UnifCellShape
    ##  [1]  4  1  8 10  2  3  5  6  7  9 NA
    ## 
    ## $MargAdhesion
    ##  [1]  5  1  3  8 10  4  6 NA  2  9  7
    ## 
    ## $EpitCellSize
    ##  [1]  7  2  3  1  6  4  5  8 10  9
    ## 
    ## $BareNuclei
    ##  [1] "10" "2"  "4"  "1"  "3"  "9"  "7"  "?"  "5"  "8"  "6" 
    ## 
    ## $BlandChromatin
    ##  [1]  3  9  1  2  4  5  7  8  6 10
    ## 
    ## $NormalNucleoli
    ##  [1]  2  1  7  4  5  3 10  6  9  8
    ## 
    ## $Mitoses
    ## [1]  1  5  4  2  3  7 10  8  6
    ## 
    ## $Class
    ## [1] 2 4

With this output, we can quickly make decisions on how to handle the
values. Missing data is usually labeled as ‘NA’, but can also be labeled
as ‘?’, ‘X’, ‘xxx’, ‘-999’ among infinite other options that are not
resolved with na.omit and should be treated according to the problem in
question.

Imputing existing NAs with vector mean

``` r
#Imputing existing NAs with vector mean
for(i in 1:ncol(df)) {
  df[ , i][is.na(df[ , i])] <- mean(df[ , i], na.rm = TRUE)
}
```

    ## Warning in mean.default(df[, i], na.rm = TRUE): argument is not numeric or
    ## logical: returning NA

``` r
get_unique_per_col(df)
```

    ## $ClumpThickness
    ##  [1]  5  3  6  4  8  1  2  7 10  9
    ## 
    ## $UnifCellSize
    ##  [1]  4.000000  1.000000  8.000000 10.000000  2.000000  3.140603  3.000000
    ##  [8]  7.000000  5.000000  6.000000  9.000000
    ## 
    ## $UnifCellShape
    ##  [1]  4.000000  1.000000  8.000000 10.000000  2.000000  3.000000  5.000000
    ##  [8]  6.000000  7.000000  9.000000  3.205165
    ## 
    ## $MargAdhesion
    ##  [1]  5.000000  1.000000  3.000000  8.000000 10.000000  4.000000  6.000000
    ##  [8]  2.811782  2.000000  9.000000  7.000000
    ## 
    ## $EpitCellSize
    ##  [1]  7  2  3  1  6  4  5  8 10  9
    ## 
    ## $BareNuclei
    ##  [1] "10" "2"  "4"  "1"  "3"  "9"  "7"  "?"  "5"  "8"  "6" 
    ## 
    ## $BlandChromatin
    ##  [1]  3  9  1  2  4  5  7  8  6 10
    ## 
    ## $NormalNucleoli
    ##  [1]  2  1  7  4  5  3 10  6  9  8
    ## 
    ## $Mitoses
    ## [1]  1  5  4  2  3  7 10  8  6
    ## 
    ## $Class
    ## [1] 2 4

We see an ‘argument is not numeric or logical’ error, this is because
the “BareNuclei” variable is of type character and is linked with the
below.

We see that the “BareNuclei” variable has a ‘?’ as a unique value and
also is of type character. How to deal with this? Since it’s preferred
to have all quantitative variables sa a number type, we cast the vector
to integer and the ‘?’ are casted to NAs.

``` r
#Cast variable as integer
df[, 'BareNuclei'] = as.integer(df[, 'BareNuclei'])
```

    ## Warning: NAs introduced by coercion

``` r
df[ , 'BareNuclei'][is.na(df[ , 'BareNuclei'])] <- mean(df[ , i], na.rm = TRUE)
```

We now verify again all the unique values in each variable to keep on
cleaning.

``` r
get_unique_per_col(df)
```

    ## $ClumpThickness
    ##  [1]  5  3  6  4  8  1  2  7 10  9
    ## 
    ## $UnifCellSize
    ##  [1]  4.000000  1.000000  8.000000 10.000000  2.000000  3.140603  3.000000
    ##  [8]  7.000000  5.000000  6.000000  9.000000
    ## 
    ## $UnifCellShape
    ##  [1]  4.000000  1.000000  8.000000 10.000000  2.000000  3.000000  5.000000
    ##  [8]  6.000000  7.000000  9.000000  3.205165
    ## 
    ## $MargAdhesion
    ##  [1]  5.000000  1.000000  3.000000  8.000000 10.000000  4.000000  6.000000
    ##  [8]  2.811782  2.000000  9.000000  7.000000
    ## 
    ## $EpitCellSize
    ##  [1]  7  2  3  1  6  4  5  8 10  9
    ## 
    ## $BareNuclei
    ##  [1] 10.000000  2.000000  4.000000  1.000000  3.000000  9.000000  7.000000
    ##  [8]  2.690544  5.000000  8.000000  6.000000
    ## 
    ## $BlandChromatin
    ##  [1]  3  9  1  2  4  5  7  8  6 10
    ## 
    ## $NormalNucleoli
    ##  [1]  2  1  7  4  5  3 10  6  9  8
    ## 
    ## $Mitoses
    ## [1]  1  5  4  2  3  7 10  8  6
    ## 
    ## $Class
    ## [1] 2 4

We have cleaned this dataset!
