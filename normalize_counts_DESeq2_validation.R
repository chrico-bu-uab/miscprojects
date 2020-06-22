suppressPackageStartupMessages(library(DESeq2))

# get filename
fnin <- commandArgs(trailingOnly = T)[1]

# extract data
counts <- read.table(fnin, header = T) # assume header

firstcol = colnames(counts)[1]
row.names(counts) <- counts[[1]]

counts <- as.matrix(counts[, -c(1, ncol(counts))])
counts <- counts[rowSums(counts) != 0,]
coldata <- data.frame(condition = c(rep("C", 4), rep("T", 3)))
row.names(coldata) <- colnames(counts)

dds <- DESeqDataSetFromMatrix(countData = counts, colData = coldata, design = ~ condition)
dds <- estimateSizeFactors(dds)
sizeFactors(dds)

norm_counts <- counts(dds, normalized = T)

# verify script output with the output contained
# in the RNASeq.pdf handout from class
fout <- paste(strsplit(fnin, ".", fixed = T)[[1]][1], "DESeq2", "normalized", "validation.txt", sep = ".")
df <- data.frame(firstcol = rownames(norm_counts), norm_counts)
names(df)[[1]] = firstcol
write.table(df, file = fout, quote = F, row.names = F, sep = "\t")
head(read.table(fout, header = T))