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
                  c(n = 27, sd = .53),# Hartig, our data
                  c(n = 27, sd = .67),
                  c(n = 27, sd = .74),
                  c(n = 27, sd = .53))

zipers_sd <- mean(sapply(zipers_sd, function(x)x[2]))

#data <- read.xlsx("Metaanalysis_table_cj_hr_cj.xlsx", 1, stringsAsFactors = FALSE)
#data <- read.table("data_update.csv", sep =";", header = TRUE, stringsAsFactors = FALSE)
#write.csv(data, "data.csv", row.names = FALSE)
data <- read.csv("data.csv")
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

# Copy secondary depression measure name to primary depression measure, for
# studies with multiple depression measures
data$primary.depression.measure[!c(1, diff(data$id_sample))] <- 
data$secondary.depression.measure[!c(diff(data$id_sample), 1)]

write.csv(data, "cleaned_data.csv", row.names = FALSE)


# Calculate ES ------------------------------------------------------------

dat <- read.csv("cleaned_data.csv", stringsAsFactors = FALSE)

dat$scale <- dat$primary.depression.measure

dat$scale[grepl("panas", tolower(dat$scale))] <- "PANAS"
dat$scale[grepl("poms", tolower(dat$scale))] <- "POMS"

dat$id_es <- unlist(lapply(rle(dat$id_sample)$lengths, function(x) 1:x))

dat$study.design[grepl("crossover", tolower(dat$study.design))] <- "Crossover"
dat$study.design[grepl("parallel", tolower(dat$study.design))] <- "Parallel"
dat$study.design[grepl("factorial", tolower(dat$study.design))] <- "Factorial"
dat$country[dat$country %in% c("China", "Japan", "Korea")] <- "Asia"
dat$country[dat$country %in% c("Denmark", "Finland", "Poland", "Spain", "Sweden", "UK")] <- "Europe"
dat$country[dat$country == "US"] <- "US"

dat$time.between.environments[dat$time.between.environments %in% c("Afternoon", "Immediately")] <- "Same day"
dat$time.between.environments[dat$time.between.environments %in% c("At least five days apart", "Next week")] <- "Next week"
dat$time.between.environments[dat$time.between.environments %in% c("Within two weeks", "Within same season")] <- "Longer"
dat$time.between.environments <- ordered(dat$time.between.environments, levels = c("Same day", "Next day", "Next week", "Longer"))

dat$urban.environment[dat$urban.environment %in% c("Building site", "Railway station", "Urban rooftop")] <- "Other"



# Which effect size -------------------------------------------------------


# Filter out posttest only accidentally coded as pretest-posttest
#pre_post <- dat[, grep("(pre|post).+1\\.$", names(dat))]
no_pre <- rowSums(is.na(dat[, grep("pre.+1\\.$", names(dat))])) == 6
yes_post <- rowSums(!is.na(dat[, grep("post.+1\\.$", names(dat))])) > 0
posttest_only <- no_pre & yes_post
dat[posttest_only, grep("post.+2\\.$", names(dat))] <- dat[posttest_only, grep("post.+1\\.$", names(dat))]
dat[posttest_only, grep("post.+1\\.$", names(dat))] <- NA

# For t-score
no_pre <- rowSums(is.na(dat[, grep("pre\\.{2}t\\.score", names(dat))])) == 6
yes_post <- rowSums(!is.na(dat[, grep("post\\.{2}t\\.score", names(dat))])) > 0
posttest_only <- no_pre & yes_post

dat$post_only_t_m_postt <- NA
dat$post_only_t_sd_postt <- NA
dat$post_only_t_n_postt <- NA
dat$post_only_t_m_postc <- NA
dat$post_only_t_sd_postc <- NA
dat$post_only_t_n_postc <- NA
dat[posttest_only, grep("^post_only_t", names(dat))] <- dat[posttest_only, grep("post\\.{2}t\\.score", names(dat))]
dat[posttest_only, grep("post\\.{2}t\\.score", names(dat))] <- NA

es_cols <- list(
  SMCR = grep("(pre|post).+1\\.$", names(dat), value = TRUE),
  SMD = grep("post.+2\\.$", names(dat), value = TRUE),
  SMCR_t = grep("(pre|post)\\.{2}t\\.score", names(dat), value = TRUE),
  SMD_t = grep("^post_only_t", names(dat), value = TRUE)
)

which_es <- sapply(es_cols, function(x){
  !apply(is.na(dat[, x]), 1, all)
})

# Drop rows without es
dat <- dat[-which(rowSums(which_es)==0), ]

dat$es_type <- colnames(which_es)[unlist(apply(which_es, 1, which))]

# T-scores are normed scores, not t-test scores
dat[dat$es_type == "SMCR_t", es_cols$SMCR] <- dat[dat$es_type == "SMCR_t", es_cols$SMCR_t]
dat[dat$es_type == "SMD_t", es_cols$SMD] <- dat[dat$es_type == "SMD_t", es_cols$SMD_t]


# Compute SMCR ------------------------------------------------------------


# For pre-post treatment-control designs ----------------------------------
# Following http://www.metafor-project.org/doku.php/analyses:morris2008

# Assumptions:
assume_pretest_sds_equal <- FALSE
# "In practice, one is likely to encounter difficulties in actually obtaining
# those correlations from the information reported in the articles. In that
# case, one can substitute approximate values (e.g., based on known properties
# of the dependent variable being measured) and conduct a sensitivity analysis
# to ensure that the conclusions from the meta-analysis are unchanged when
# those correlations are varied."
#
# I am assuming a pre/posttest correlation of .5
assumed_pre_post_cor <- .5

SMCR_rows <- dat$es_type %in% c("SMCR", "SMCR_t")

pre_post <- dat[SMCR_rows, es_cols$SMCR]
names(pre_post) <-
  gsub("(natural|urban)\\.(pre|post)\\.(\\w+)\\.{2}1\\.",
       "\\3_\\2\\1",
       names(pre_post))
names(pre_post) <- gsub("natural", "t", names(pre_post))
names(pre_post) <- gsub("urban", "c", names(pre_post))
names(pre_post) <- gsub("mean", "m", names(pre_post))

#Ns <- pre_post[, grep("^n_", names(pre_post))]
#dat$study.design[SMCR_rows]

# Calculate es
if (!assume_pretest_sds_equal) {
  datT <-
    with(pre_post, {
      escalc(
        measure = "SMCR",
        m1i = m_postt,
        m2i = m_pret,
        sd1i = sd_pret,
        ni = n_pret,
        ri = rep(assumed_pre_post_cor, length(n_pret))
      )
    })
  datC <-
    with(pre_post, {
      escalc(
        measure = "SMCR",
        m1i = m_postc,
        m2i = m_prec,
        sd1i = sd_prec,
        ni = n_prec,
        ri = rep(assumed_pre_post_cor, length(n_prec))
      )
    })
  pre_post <-
    data.frame(
      Type = "SMCR",
      yi = datT$yi - datC$yi,
      vi = datT$vi + datC$vi,
      stringsAsFactors = F
    )
  rm(datC, datT)
} else {
  sd_pool <-
    with(pre_post, {
      sqrt(((n_pret - 1) * sd_pret ^ 2 + (n_prec - 1) * sd_prec ^ 2) / (n_pret + n_prec - 2))
    })
  pre_post$yi <-
    with(pre_post, {
      metafor:::.cmicalc(n_pret + n_prec - 2) * (((m_postt - m_pret) - (m_postc - m_prec)) / sd_pool)
    })
  pre_post$vi <- with(pre_post, {
    2 * (1 - assumed_pre_post_cor) * (1 / n_pret + 1 / n_prec) + yi ^ 2 / (2 *
                                                                             (n_pret + n_prec))
  })
  pre_post <-
    data.frame(Type = "SMCR", pre_post[, c("yi", "vi")], stringsAsFactors = F)
  
}


# Compute SMD -------------------------------------------------------------

smd <- dat[dat$es_type %in% c("SMD", "SMD_t"), es_cols$SMD]
smd[is.na(smd)]<- zipers_sd
names(smd) <-
  gsub("(natural|urban)\\.(pre|post)\\.(\\w+)\\.{2}2\\.",
       "\\3_\\2\\1",
       names(smd))
names(smd) <- gsub("natural", "t", names(smd))
names(smd) <- gsub("urban", "c", names(smd))
names(smd) <- gsub("mean", "m", names(smd))

smd <-
  data.frame(Type = "SMD", with(smd, {
    escalc(
      measure = "SMD",
      m1i = m_postt,
      m2i = m_postc,
      sd1i = sd_postt,
      sd2i = sd_postc,
      n1i = n_postt,
      n2i = n_postc
    )
  }), stringsAsFactors = FALSE)



# Merge effect sizes ------------------------------------------------------

available_es <- c(
  pre_post$yi,
  smd$yi
)
if(anyNA(available_es)) stop("Some effect sizes are missing")

dat$yi <- NA
dat$vi <- NA
dat$yi[dat$es_type %in% c("SMCR", "SMCR_t")] <- pre_post$yi
dat$vi[dat$es_type %in% c("SMCR", "SMCR_t")] <- pre_post$vi
dat$yi[dat$es_type %in% c("SMD", "SMD_t")] <- smd$yi
dat$vi[dat$es_type %in% c("SMD", "SMD_t")] <- smd$vi

dat$es_type <- gsub("_t", "", dat$es_type)
#grep("\\.n($|\\.)", names(dat), value = T)

write.csv(dat, "dat_hroberts.csv", row.names = FALSE)