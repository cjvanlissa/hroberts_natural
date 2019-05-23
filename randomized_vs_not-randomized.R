library(missRanger)
library(motley)
fulldat <- dat <- read.csv("dat_hroberts.csv")
moderators <- c("study.design", "country", "age..mean.", "students", "gender",
                "female..", "health.condition", "exposure.time..mins.",
                "time.between.environments", "natural.environment",
                "number.of.natural.envts", "urban.environment",
                "number.of.urban.envts", "baseline.measurement",
                "measurement.at.environment", "measurement.during.activity",
                "measurement.final", "scale",
                "activity.category", "es_type")
newnames <- c("Design", "Country", "Mean age", "Students", "Sex",
              "Female", "Health condition", "Exposure time",
              "Time between environments", "Natural environment",
              "Number of natural envts", "Urban environment",
              "Number of urban envts", "Baseline measurement",
              "Measurement at environment", "Measurement during activity",
              "Measurement final", "Scale",
              "Activity category", "Type of ES")
names(newnames) <- moderators
rename_fun <- function(x, old, new){
  x[na.omit(match(old, x))] <- new[old %in% x]
  x
}

df <- dat[, moderators]

is_char <- sapply(df, is.character)
df[, is_char] <- lapply(df[, is_char], as.factor)
df$id_effectsize <- 1L:nrow(dat)
missingness <- sapply(df, function(x) sum(is.na(x))/length(x))
miss_tab <- table(missingness)
many_missings <- names(dat)[missingness > .2]
many_missings
# CJ: None of the moderators used have substantial missingness.

# Any factor variables with missingness? No.
names(df)[sapply(df, function(x) inherits(x, c("factor", "character"))) & missingness > 0]

set.seed(47484)
imputed <- missRanger(df, pmm.k = 3, num.trees = 1000, returnOOB = TRUE)
attr(imputed, "oob")

df <- data.frame(imputed, dat[, c("id_sample", "yi", "vi")])

df$time.between.environments <- ordered(df$time.between.environments, levels = c("Same day", "Next day", "Next week", "Longer"))

df$randomized <- grepl("^(Joung|Li|Roe|Song et al\\. \\((2015b|2013|2014)\\)|Stigsdotter|Tsunetsugu)", dat$title)
df$randomized <- factor(df$randomized, labels = c("Not randomized", "Randomized"))

rma(yi, vi, mods = ~randomized-1, data = df)