df <- file.choose()
df <- readRDS(df)

# Checking the shape of the dataset
dim(df)

# Checking the null count in the dataset
colSums(is.na(df))

# Load the ggplot2 library
library(ggplot2)

# Create a count plot for the number of unique strands
ggplot(data = df, aes(x = strand)) +
  geom_bar() +
  labs(title = "Count of Unique Strands") +
  scale_fill_manual(values = c("+" = "blue", "-" = "red")) +
  xlab("Strand") +
  ylab("Count")

#Task 1: How many unique transcripts are there? 
library(dplyr)
# Count the number of unique values in the 'transcript_id' column
y <- df %>% 
  distinct(transcript_id) %>% 
  nrow()

# Printing the unique number of transcripts
cat("The number of unique transcripts are:", y, "\n")

#Task 2: How many unique exons are there?
# Count the number of unique values in the 'exon_id' column
z <- df %>% 
  distinct(exon_id) %>% 
  nrow()

# Printing the unique number of exons
cat("Number of unique exons are:", z, "\n")

#Task 3:  What is the average length of an exon? What is the median length?

# Calculate the average length of an exon
avg <- mean(df$width, na.rm = TRUE)
cat("The average length of an exon is:", avg, "\n")

# Calculate the median length of an exon
median <- median(df$width, na.rm = TRUE)
cat("The median length of an exon is:", median, "\n")

# Task 4: Find the length of the introns between the exons.(Length must be a positive integer). For the first exon in the transcript intron length is 0. For the (n)th intron in the transcript intron length corresponds to the intron between the (n)th and (n-1)th exon. There is no intron after the last exon in the transcript(to the right)

# Sorting the dataset by transcript_id and rank
df <- df %>%
  arrange(transcript_id, rank)

# Calculating intron lengths based on strand direction
df$intron <- 0
# Calculating intron lengths, since coordinates are present on exon difference between start coordinate and previous end coordinate - 1
# df$intron[df$rank > 1] <- (df$start[df$rank > 1] - df$end[df$rank > 0]) - 1
df$intron[df$rank > 1] <- (lead(df$start) - df$end) - 1
df$intron <- ifelse(df$strand == '-', -df$intron, abs(df$intron))

# Reset the intron length for the first exon in each transcript
df$intron[df$transcript_id != lag(df$transcript_id, default = 0)] <- 0

# Displaying the updated dataset with the intron column
df


#Bonus Task

# Sorting the dataset by transcript_id and rank
df <- df %>%
  arrange(transcript_id, rank)

# Calculating half_width based on exon width
df$half_width <- floor(df$width / 2)
df$half_width_intron <- floor(df$intron / 2)

# Calculating L1, L2, U1, U2 based on half_width and strand
df$L1 <- df$start - pmin(100, floor(df$intron / 2))
df$L2 <- df$start + pmin(100, floor(df$width / 2))
df$U1 <- df$end - pmin(100, floor(df$width / 2))
df$U2 <- df$end + pmin(100, floor(df$intron / 2))

# Replacing L1, L2, U1, U2 with half the length of exon or intron if less than 200 units
df$L1[df$intron < 200] <- df$start[df$intron < 200] - df$half_width_intron[df$intron < 200]
df$L2[df$width < 200] <- df$start[df$width < 200] + df$half_width[df$width < 200]
df$U1[df$width < 200] <- df$end[df$width < 200] - df$half_width[df$width < 200]
df$U2[df$intron < 200] <- df$end[df$intron < 200] + df$half_width_intron[df$intron < 200]


# Adjusting coordinates for '-' strand
df[df$strand == '-', c('L1', 'L2', 'U1', 'U2')] <- df[df$strand == '-', c('U2', 'U1', 'L2', 'L1')]

# Set L1=0 for leftmost exon and U2=0 for rightmost exon in each transcript
df <- df %>%
  group_by(transcript_id) %>%
  mutate(L1 = ifelse(rank == min(rank), 0, L1),
         U2 = ifelse(rank == max(rank), 0, U2))

# Dropping the 'half_width' and 'half_width_intron column 
df <- df %>%
  select(-half_width)
df <- df %>%
  select(-half_width_intron)

# Ensuring that L1, L2, U1, U2 are integers
df$L1 <- as.integer(df$L1)
df$L2 <- as.integer(df$L2)
df$U1 <- as.integer(df$U1)
df$U2 <- as.integer(df$U2)

# Displaying the updated dataset with the new columns
df







