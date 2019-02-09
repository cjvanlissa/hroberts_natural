library(xlsx)

zipers_sd <- list(c(n = 102, sd = 1.25), # Hartig 1996
                  c(n = 102, sd = .59),
                  c(n = 102, sd = .56),
                  c(n = 102, sd = 1),
                  c(n = 102, sd = .7),
                  c(n = 102, sd = .75),
                  c(n = 69, sd = .85), # Valtchanov & Ellard, 2010
                  c(n = 69, sd = .35),
                  c(n = 69, sd = .61),
                  c(n = 69, sd = .41))

zipers_sd <- mean(sapply(zipers_sd, function(x)x[2]))

#data <- read.xlsx("Metaanalysis_table_cj_hr_cj.xlsx", 1, stringsAsFactors = FALSE)
data <- read.table("data_update.csv", sep =";", header = TRUE, stringsAsFactors = FALSE)
data <- data[!rowSums(is.na(data)) == ncol(data),]
data[data == "N/A"] <- NA
data[data == "Unknown"] <- NA


# Drop these! -------------------------------------------------------------
drop_cols <- c("Additional.Environment..1.", "Additional.Environment..2.")
data <- data[, -match(drop_cols, names(data))]

names(data) <- tolower(names(data))
names(data)[1] <- "title"
codes <- read.xlsx("Metaanalysis_table_cj_hr_cj.xlsx", 2, stringsAsFactors = T)
codes <- codes[, -match(drop_cols, names(codes))]
names(codes) <- tolower(names(codes))

distances <- apply(adist(names(data), y = names(codes), fixed = TRUE,
      ignore.case = TRUE), 2, which.min)

names(data)[distances] <- names(codes)
#names(codes)[which(is.na(pmatch(names(codes), names(data))))]
#names(data)[!sapply(names(data), `%in%`, names(codes))][-1]

#names(codes)[!sapply(names(codes), `%in%`, names(data))] <- names(data)[!sapply(names(data), `%in%`, names(codes))][-1]

for(x in names(codes)){
  #x <- names(codes)[3]
  cods <- levels(codes[[x]])[grepl("(\\d+)\\s+=\\s+.*$", levels(codes[[x]]))]
  if(!length(cods)) next
  val <- gsub("(\\d+)\\s+=\\s+.*$", "\\1", cods)
  if(!(all(sort(as.numeric(val)) == 1:length(val)) | all(sort(as.numeric(val)+1) == 1:length(val)))){
    cat("Labels ", val)
    stop("Labels not in order for variable ", x)
  } 
  lab <- gsub("\\d+\\s+=\\s+(.*)$", "\\1", cods)
  tmp <- data[[x]]
  if(any(!grepl("^\\d+$", tmp))) warning("Some labels are not numeric in variable ", x)
  tmp <- gsub("(\\d+).*$", "\\1", tmp)
  if(any(!grepl("^\\d+$", na.omit(tmp)))){
    warning("Some labels are still not numeric in variable ", x)
    browser()
  } 
  data[[x]] <- lab[match(data[[x]], val)]
}
write.csv(data, "cleaned_data.csv", row.names = FALSE)
