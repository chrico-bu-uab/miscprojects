# define normalize function
normalize <- function(counts, methodToApply, methodName, removeZeros=T) {
  
  # Get rid of rows whose columns sum to zero. Note that p-values
  # shouldn't change, but fdr will if you remove zeros. Removing
  # zeros is recommended primarily for runtime purposes.
  if (removeZeros) {
    counts <- counts[apply(counts, MARGIN = 1, FUN = sum) != 0,]
  }
  
  # loop through columns and apply normalization method to each row
  applytocounts <- apply(counts, MARGIN = 2, methodToApply)
  norm_counts <- counts
  for (i in 1:dim(counts)[2]) {
    norm_counts[, i] <- counts[, i] / applytocounts[i]
  }
  
  ## plot and return norm_counts variable
  #boxplot(norm_counts, outline = F)
  
  fout <- paste(strsplit(fnin, ".", fixed = T)[[1]][1], methodName, "normalized.txt", sep = ".")
  df <- data.frame(firstcol = rownames(norm_counts), norm_counts)
  names(df)[[1]] = colnames(counts)[1]
  write.table(df, file = fout, quote = F, row.names = F, sep = "\t")
  df
}

# get filename
fnin <- commandArgs(trailingOnly = T)[1]

# extract data
counts <- read.table("Downloads/pnas_expression.txt", header = T) #<- read.table(fnin, header = T) # assume header
rownames(counts) <- counts[, 1]
counts <- as.matrix(counts[, -c(1, ncol(counts))])
head(counts)
counts <- counts[rowSums(counts) != 0,]

# Examples with two different methods: median and upper quartile (uq)
# Note that in both cases, method is defined to remove zeros
# for calculating normalization, regardless of whether the zero
# rows are left in place or removed from the output data.

# median normalization - not a good method, but I included it for comparison
median_norm <- normalize(counts, function(x) {median(x[x != 0])}, "median")
head(median_norm)

# uq normalization - better
uq_norm <- normalize(counts, function(x) {quantile(x[x != 0], 0.75)}, "uq")
head(uq_norm)