install.packages("dplyr")

# Making sure that the last entry of a person is their most recent visit
data <- read.csv("/Users/azra/Desktop/internship 2023/naccID_36KID_naccdata_Pheno36K.csv")
filtered_data <- data %>%
  group_by(SUBJID) %>%
  filter(NACCYOD == max(NACCYOD)) %>%
  ungroup()
write.csv(filtered_data, "filtered_data.csv", row.names = FALSE)



# Select and keep only the six columns 
filtered_data <- read.csv("filtered_data.csv")
selected_columns <- filtered_data %>%
  select(NACCNEUR, NPTHAL, NACCBRAA, NPADNC, NACCDIFF, SUBJID, VISITYR, NACCYOD, EDUC, APOE_reported, CDRSUM)



# Keep only the rows with the latest VISITYR for each SampleID
data <- read.csv("/Users/azra/Desktop/internship 2023/naccID_36KID_naccdata_Pheno36K.csv")
# Sort the data frame by SampleID and VISITYR in descending order
data <- arrange(data, SUBJID, desc(VISITYR))
data <- distinct(data, SUBJID, .keep_all = TRUE)
write.csv(data, "latest_visits.csv", row.names = FALSE)



# Remove rows with 8888 in the NACCYOD column
data <- read.csv("latest_visits_edited.csv")
data_filtered <- data %>%
  filter(NACCYOD != 8888)
write.csv(data_filtered, "latest_visits_edited_deceasedPatients.csv", row.names = FALSE)

# Filter rows with a difference between VISITYR and NACCYOD less than or equal to 3
data <- read.csv("latest_visits_edited_deceasedPatients.csv")
data_filtered <- data %>%
  filter(abs(VISITYR - NACCYOD) <= 3)
write.csv(data_filtered, "latest_visits_edited_deceasedPatients_filtered.csv", row.names = FALSE)




# Making a chart to see  the count of rows where both corresponding columns do not contain any excluded values
data <- read.csv("latest_visits_edited_deceasedPatients_filtered.csv")
columns_of_interest <- c("NACCNEUR", "NPTHAL", "NACCBRAA", "NPADNC", "NACCDIFF")
count_matrix <- matrix(0, nrow = length(columns_of_interest), ncol = length(columns_of_interest))
excluded_values <- c(-4, 8, 9)
for (i in 1:length(columns_of_interest)) {
  for (j in 1:length(columns_of_interest)) {
    count_matrix[i, j] <- sum(!data[, columns_of_interest[i]] %in% excluded_values & 
                                !data[, columns_of_interest[j]] %in% excluded_values)
  }
}
count_df <- as.data.frame(count_matrix)
row.names(count_df) <- columns_of_interest
colnames(count_df) <- columns_of_interest
print(count_df)


# Filtering out -4 and 99 values in NACCNEUR, NACCBRAA, EDUC
data <- read.csv("latest_visits_edited_deceasedPatients_filtered.csv")
data_filtered <- data %>% filter(NACCNEUR != -4 & NACCBRAA != -4)
write.csv(data_filtered, "FINAL_latest_visits_filtered.csv", row.names = FALSE)
df <- read.csv("FINAL_latest_visits_filtered.csv")
df <- df %>% filter(EDUC != 99)
write.csv(df, file = "FINAL_latest_visits_filtered.csv", row.names = FALSE)


# Make a box plot of CDRSUM by NACCNEUR and EDUC category
data <- read.csv("filtered_file.csv")
data <- data %>%
  mutate(
    category = cut(EDUC, breaks = c(-Inf, 8, 12, Inf), labels = c("<=8", "9-12", ">12")),
    apoe4 = factor(apoe4)
  )
filtered_data <- data %>%
  filter(!(NACCNEUR %in% c(-4, 8, 9)))
ggplot(filtered_data, aes(x = as.factor(NACCNEUR), y = CDRSUM)) +
  geom_boxplot() +
  geom_jitter(width = 0.2) +
  labs(x = "Neuropathology", y = "CDRSUM") +
  ggtitle("Boxplot of CDRSUM by NACCNEUR and EDUC category") +
  facet_wrap(~ category + apoe4, ncol = 6)


# Perform the Mann-Whitney U test
data$CDRSUM <- as.numeric(data$CDRSUM)

data.8and9_12 = data |> filter(category %in% c("<=8","9-12"))
data.8andG12 = data |> filter(category %in% c("<=8",">12"))
data.9_12andG12 = data |> filter(category %in% c("9-12",">12"))
m1 <- wilcox.test(CDRSUM ~ category, data=data.8and9_12, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
m2<- wilcox.test(CDRSUM ~ category, data=data.8andG12, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
m3<- wilcox.test(CDRSUM ~ category, data=data.9_12andG12, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(m1)
print(m2)
print(m3)

# Taking a look at the statistical significance of the presence of APOE4
data <- read.csv("filtered_file.csv")
data$category <- cut(data$EDUC, breaks = c(-Inf, 8, 12, Inf), labels = c("<=8", "9-12", ">12"))
data$apoe4 <- factor(data$apoe4)
filtered_data <- data %>%
  filter(!(NACCNEUR %in% c(-4, 8, 9)))


# Testing presence of APOE4 among 9-12
data_9_12 <- data[data$category == "9-12", ]
results <- lapply(unique(data_9_12$NACCNEUR), function(naccneur_value) {
  subset_data <- data_9_12[data_9_12$NACCNEUR == naccneur_value, ]
  apoe4_present <- subset_data[subset_data$apoe4 == 1, "CDRSUM"]
  apoe4_absent <- subset_data[subset_data$apoe4 == 0, "CDRSUM"]
  test_result <- wilcox.test(apoe4_present, apoe4_absent, paired = FALSE, exact = FALSE, conf.int = TRUE)
  return(test_result)
})
results_df <- data.frame(
  NACCNEUR = unique(data_9_12$NACCNEUR),
  p.value = sapply(results, function(x) x$p.value)
)
print(results_df)

# Testing presence of APOE4 among <=8
data_8 <- data[data$category == "<=8", ]
results <- lapply(unique(data_8$NACCNEUR), function(naccneur_value) {
  subset_data <- data_8[data_8$NACCNEUR == naccneur_value, ]
  apoe4_present <- subset_data[subset_data$apoe4 == 1, "CDRSUM"]
  apoe4_absent <- subset_data[subset_data$apoe4 == 0, "CDRSUM"]
  if (length(apoe4_present) > 1 && length(apoe4_absent) > 1) {
    test_result <- wilcox.test(apoe4_present, apoe4_absent, paired = FALSE, exact = FALSE, conf.int = TRUE)
  } else {
    test_result <- list(p.value = NA)
  }
  return(test_result)
})
results_df <- data.frame(
  NACCNEUR = unique(data_8$NACCNEUR),
  p.value = sapply(results, function(x) x$p.value)
)
print(results_df)

# Testing presence of APOE4 among >12
data_12 <- data[data$category == ">12", ]
results <- lapply(unique(data_12$NACCNEUR), function(naccneur_value) {
  subset_data <- data_12[data_12$NACCNEUR == naccneur_value, ]
  apoe4_present <- subset_data[subset_data$apoe4 == 1, "CDRSUM"]
  apoe4_absent <- subset_data[subset_data$apoe4 == 0, "CDRSUM"]
  test_result <- wilcox.test(apoe4_present, apoe4_absent, paired = FALSE, exact = FALSE, conf.int = TRUE)
  return(test_result)
})
results_df <- data.frame(
  NACCNEUR = unique(data_12$NACCNEUR),
  p.value = sapply(results, function(x) x$p.value)
)
print(results_df)


