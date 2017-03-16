# Create data frame with EN, CN city names, and provinces
trim <- function(x) gsub("^\\s+|\\s+$", "", x)  # function to remove whitespaces

cnennames <- read.csv("citynames85.csv", header = T, sep = ";", stringsAsFactors = F)
cnennames[1] <- trim(cnennames$cityname)  # Remove whitespaces (which hinder merging)


# Function to read and match variable file with cityname; file = 'filename', pos = 1 + position of excelcolumn of
# variable, name = new name of variable (column)
readdata <- function(file, pos, name) {
    pos_n <- pos + 1
    excel <- loadWorkbook(file)
    sheetnames <- head(names(getSheets(excel)), -1)  # last sheetname is always 'CNKI', therefore useless
    excelsheets <- mapply(read.xlsx, file, sheetName = sheetnames, stringsAsFactors = F)  # sheets are read as list of data frames
    variable <- as.data.frame(apply(excelsheets, 1, unlist), stringsAsFactors = FALSE)  # unlisted into one data frame
    colnames(variable)[1] <- "cityname"  # first column of every raw data file is Chinese city name
    variable[is.na(variable)] <- 0  # in my raw data, NA usually means 0
    variable$cityname <- trim(variable$cityname)  # Remove whitespaces (which hinder merging)
    merged <- merge(cnennames, variable, by = "cityname", all.x = TRUE)  # merge with English city names and provinces
    colnames(merged)[pos_n] <- name  # rename the variable of interest (+1 because provincess were added later, so cityname, cityname_e, and provinces are the first three columns)
    merged[pos_n] <- gsub(",", "", merged[, pos_n])  # remove spaces and commas
    merged[pos_n] <- as.numeric(unlist(merged[pos_n]))  # coerce variable of interest to numeric format
    merged[is.na(merged)] <- 0  # turn new NAs into 0 too
    merged[!(is.na(merged$cityname)), c(1, 2, 3, pos_n)]  # remove columns which are not of interest
    merged <- merged[!(is.na(merged$cityname)), c(1, 2, 3, pos_n)]  # remove columns which are not of interest
    jilin <- as.data.frame(merged[which(tolower(merged$cityname_e) == tolower(merged$provinces)), ][4])  # Jilin is both province and city, therefore the row with the province (larger value or first value, if they are the same) needs to be removed
    if (all(jilin[1, ] == jilin[2, ])) {
        merged[-as.numeric(rownames(jilin)[1]), ]
    } else {
        merged[-which(merged[, 4] == max(merged[which(tolower(merged$cityname_e) == tolower(merged$provinces)), ][4])), 
            ]
    }
}
