df <- file.choose()
df <- readRDS(df)

write.csv(df, "outfile.csv", fileEncoding = "UTF-8", row.names=FALSE)