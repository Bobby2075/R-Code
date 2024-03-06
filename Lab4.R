library(readxl)

# Read clinical attributes
clinical <- read_excel("CPTAC_Cohort_Clinical_Data.xlsx", sheet = "Patient_Clinical_Attributes")

# Read associations between patiens and samples

patien_sample <- read_excel("CPTAC_Cohort_Clinical_data.xlsx", sheet ="Biospecimen_Attributes")

# Read associations between samples and instruments

sample_run <- read_excel("CohortSpecimens.xlsx")

## Exercise 1 ----

# question 1 How many patiens are in the data set? 
# Clinical contains 140 patients

for (i in clinical$gender) {
  if (i == "Male") {
    gender = gender + 1

  }
}

# Question 2 there are 74 males

# question 3 
# What is the average BMI?
mean(clinical$BMI, na.rm = TRUE)
hist(clinical$BMI)

# It would make sense to look at the average BMI because the data is normal distributed


## Exercise 2 ----

table <- table(clinical$tumor_site)
barplot(table)

boxplot(clinical$tumor_size_in_cm)
abline(h = mean(clinical$tumor_size_in_cm, na.rm = TRUE), col = "orange")
abline(h = mean(clinical$tumor_size_in_cm, na.rm = TRUE) - sd(clinical$tumor_size_in_cm, na.rm = TRUE), col = "blue")
abline(h = mean(clinical$tumor_size_in_cm, na.rm = TRUE) + sd(clinical$tumor_size_in_cm, na.rm = TRUE), col = "blue")

table <- table(clinical$histologic_type)
barplot(table)


## Exercise 3 ----

# question 7: how many samples are in the data set
summary(patien_sample)

# The data set contains 300 samples

table_id <- table(patien_sample$case_id)
hist(table_id)
barplot(table_id)
abline(h = mean(table_id))


## Exercise 4 ----

# question 10, how many tumor samples have been acquired?
# there are 154 samples

tumor_sample <- table(sample_run$`Case ID`)


## Exercise 5 ----

reduced_proteins <- read.csv("biomarker_proteins.csv", header = TRUE, sep = ",")

#---- Filter for the samples that we have proteomics data for ----
# First we need to remove the ".Unshared.Log.Ratio" suffix from the
# column names
colnames(reduced_proteins) <- gsub(
  "\\.Unshared\\.Log\\.Ratio", "",
  colnames(reduced_proteins)
)

#---- Extract the sample ids to Tumor and Normal Adjacent Tissue ----
# Get sample IDs for Tumor, called Aliquot ID in sample_runs
tumor_sample_ids <- sample_run$"Aliquot ID"[
  sample_run$"Tumor/Normal" == "Tumor"
]
# Get sample IDs for tissues containing the word Normal
normal_sample_ids <- sample_run$"Aliquot ID"[
  grep("Normal", sample_run$"Tumor/Normal")
]

# Remove NA values from new objects
tumor_sample_ids <- tumor_sample_ids[!is.na(tumor_sample_ids)]
normal_sample_ids <- normal_sample_ids[!is.na(normal_sample_ids)]

# Take only the samples that are in the reduced_proteins data
tumor_sample_ids <- tumor_sample_ids[
  tumor_sample_ids %in% colnames(reduced_proteins)
]
normal_sample_ids <- normal_sample_ids[
  normal_sample_ids %in% colnames(reduced_proteins)
]

# ---- The resulting numbers of samples are: ----
cat("Number of tumor samples:", length(tumor_sample_ids), "\n")
cat("Number of normal / normal adjacent samples:", length(normal_sample_ids), "\n")

# ---- Create tumor_vs_normal object and visualize ----
# Reduce the protein data to the sample_ids above
tumor_vs_normal <- reduced_proteins[, c(normal_sample_ids, tumor_sample_ids)]

# plot the data for the first protein
# you can change the number to see the data for other proteins
protein_number <- 1
plot(t(tumor_vs_normal[protein_number, ]),
     pch = 16, col = ifelse(colnames(tumor_vs_normal) %in% tumor_sample_ids, "red", "blue"),
     xlab = "Sample", ylab = "Protein expression", main = rownames(tumor_vs_normal)[protein_number]
)
legend("topright", legend=c("Normal", "Tumor"), col=c("blue", "red"), pch=1)

# Question 14: what is the name of the first protein? do you observe any particular behavior when comparing tumor versus normal


## Exercise 7 ----

# ---- get case IDs for the tumor_sample_ids ----
# Extract subset of sample_run object for the tumor samples
tumor_sample_run <- sample_run[sample_run$"Aliquot ID" %in%
                                 tumor_sample_ids, ]
# reorder according to the tumor_sample_ids and get case IDs
tumor_case_ids <- unlist(tumor_sample_run[
  match(
    tumor_sample_ids,
    tumor_sample_run$"Aliquot ID"
  ),
  "Case ID"
])

# ---- Get the gender for each case id ----
# Extract subset of clinical object for the tumor samples
tumor_clinical <- clinical[clinical$case_id %in% tumor_case_ids, ]
# reorder according to the original tumor_sample_ids so that the order
# of the gender values is the same as in tumor_sample_ids
tumor_gender <- unlist(clinical[match(
  tumor_case_ids,
  tumor_clinical$case_id
), "gender"])

#Question 23: Which are the five most differently abundant proteins from the above comparison? Sort the proteins by the p-value of the t-test.

#Question 24: When plotting histograms of these proteins for each gender, do you see any particular differences? Do this only for the tumor samples. You can use the following code to plot the histograms:
  
  # Set the layout of the plot area to 2 rows and 3 columns
  par(mfrow=c(2,3))

# Loop through each protein in the top 5
for (p in top5) {
  # Get the range of protein expression values
  prange <- range(tumor_vs_normal[p,])
  
  # Get protein values for tumors and protein p for males
  tumor_values <- unlist(tumor_vs_normal[p, tumor_sample_ids[gender == "Male"]])
  
  # Plot histogram for male samples
  hist(tumor_values, 
       col="#3333AA77", xlim=prange, breaks=seq(prange[1], prange[2], 
                                                length.out=20), main=p, xlab="Protein expression", ylim=c(0,10))
  
  # Get protein values for tumors and protein p for females
  tumor_values <- unlist(tumor_vs_normal[p, tumor_sample_ids[gender == "Female"]])
  
  # Add histogram for female samples to the same plot
  hist(tumor_values, 
       col="#33AA3377", add=T, breaks=seq(prange[1], prange[2], 
                                          length.out=20))
  
  # Add a legend to the plot
  legend("topleft", c("Female", "Male"), 
         col=c("#33AA3377", "#3333AA77"), pch=1)
}

# Reset the layout of the plot area to 1 row and 1 column
par(mfrow=c(1,1))