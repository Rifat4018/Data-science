pkgs <- c("tidyverse","janitor","fastDummies","GGally","ggcorrplot","gridExtra","broom","scales")
# gridExtra" → arranges multiple ggplots on one page.
# "broom" → converts statistical analysis objects into tidy data frames.
# "scales" → tools for scaling data (used with ggplot2).
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install) > 0) install.packages(to_install, repos="https://cloud.r-project.org")
# Load libraries quietly (suppress startup messages)
invisible(lapply(pkgs, function(pkg) {
  suppressWarnings(suppressPackageStartupMessages(library(pkg, character.only = TRUE)))
}))

# ---- Build combined dataset we have combined it that way ----
#gender_submission <- read.csv("E:\\10th\\IDS mid project\\gender_submission.csv")
#train_data <- read.csv("E:\\10th\\IDS mid project\\train.csv")
#test_data  <- read.csv("E:\\10th\\IDS mid project\\test.csv")
#test_data  <- cbind(Survived = gender_submission$Survived, test_data)
#bind <- rbind(train_data, test_data)
#write.csv(bind, "E:\\10th\\IDS mid project\\titanic.csv", row.names = FALSE)

df <- read.csv("https://raw.githubusercontent.com/MahinSarkarAtul/r/main/titanic.csv")
head(df)
str(df)
summary(df)

df <- df[!duplicated(df), ]
df
# ---- Count missing/empty values ----
empty_counts <- sapply(df, function(x) sum(is.na(x) | x == ""))
empty_counts[empty_counts > 0]

# Load original dataset again for missing value exploration
df_raw <- read.csv("https://raw.githubusercontent.com/MahinSarkarAtul/r/main/titanic.csv")

# Count missing values per column
miss_tbl <- sapply(df_raw, function(x) sum(is.na(x) | (is.character(x) & x=="")))
miss_df  <- tibble(Variable = names(miss_tbl), Missing = as.integer(miss_tbl)) %>%
  arrange(desc(Missing))

print(miss_df %>% filter(Missing > 0))

# ---- Missing values bar chart ----
ggplot(miss_df, aes(x = reorder(Variable, Missing), y = Missing)) +
  geom_col() + coord_flip() +
  labs(title="Raw: Missing/Empty counts by variable", x=NULL, y="Count")

# ---- Distributions ----
ggplot(df_raw, aes(Age)) + geom_histogram(binwidth = 5) +
  labs(title="Raw: Age distribution", x="Age", y="Count")

ggplot(df_raw, aes(Fare)) + geom_histogram(bins = 20) +
  labs(title="Raw: Fare distribution", x="Fare", y="Count")

# ---- Survival by Sex ----
ggplot(df_raw, aes(x = factor(Survived), fill = factor(Sex))) +
  geom_bar(position = "dodge") +
  labs(title="Raw: Survival by Sex", x="Survived", y="Count")

# ---- Fare by Passenger Class ----
ggplot(df_raw, aes(x = factor(Pclass), y = Fare)) +
  geom_boxplot() +
  labs(title="Raw: Fare by Pclass", x="Pclass", y="Fare")

# ---- Correlation Heatmap (numeric features) ----
raw_num <- df_raw %>% select(where(is.numeric))
raw_num <- raw_num[, colSums(is.na(raw_num)) < nrow(raw_num), drop=FALSE]  # drop all-NA cols
raw_corr <- cor(raw_num, use = "pairwise.complete.obs")
ggcorrplot::ggcorrplot(raw_corr, lab = TRUE, lab_size = 3) + 
  labs(title = "Raw: Correlation Heatmap (Numeric Features)")

# ---- Survival by Embarked × Sex ----
emb_sex_raw <- df_raw %>%
  mutate(Sex = tolower(Sex), Embarked = toupper(Embarked)) %>%
  filter(Embarked %in% c("C","Q","S")) %>%
  group_by(Embarked, Sex) %>%
  summarise(SurvivalRate = mean(Survived, na.rm=TRUE), .groups="drop")

ggplot(emb_sex_raw, aes(x = Sex, y = Embarked, fill = SurvivalRate)) +
  geom_tile(color="white") +
  geom_text(aes(label = scales::percent(SurvivalRate, accuracy = 1)), size=4) +
  scale_fill_gradient(low="red", high="green") +
  labs(title = "Raw: Survival Rate by Embarked × Sex",
       x = "Sex", y = "Embarked", fill = "Survival Rate") +
  theme_minimal()

# ---- Age groups for survival analysis ----
df_raw <- df_raw %>%
  mutate(AgeGroup_raw = cut(Age, breaks=c(-Inf,12,18,35,60,Inf),
                            labels=c("Child","Teen","YoungAdult","Adult","Senior")))

pclass_age_raw <- df_raw %>%
  group_by(Pclass, AgeGroup_raw) %>%
  summarise(SurvivalRate = mean(Survived, na.rm=TRUE), .groups="drop")

ggplot(pclass_age_raw, aes(x = factor(Pclass), y = AgeGroup_raw, fill = SurvivalRate)) +
  geom_tile(color="white") +
  geom_text(aes(label = scales::percent(SurvivalRate, accuracy = 1)), size=4) +
  scale_fill_gradient(low="red", high="green") +
  labs(title = "Raw: Survival Rate by Pclass × Age Group",
       x = "Pclass", y = "Age Group", fill = "Survival Rate") +
  theme_minimal()

# ---- Handle missing / invalid values ----
mean(df$Age, na.rm = TRUE)
median(df$Age, na.rm = TRUE)
freq_table <- table(df$Age)
names(freq_table)[which.max(freq_table)]

# Standardize sex & embarked values
df$Sex      <- tolower(df$Sex)
df$Embarked <- toupper(df$Embarked)
valid_emb   <- c("C","Q","S")
df$Embarked[!df$Embarked %in% valid_emb] <- NA
# Fill missing Embarked with mode
emb_mode <- names(sort(table(df$Embarked), decreasing = TRUE))[1]
emb_mode
df$Embarked[is.na(df$Embarked)] <- emb_mode

# Detect invalid values
fare_bad <- with(df, is.finite(Fare) & Fare <= 0)
age_bad  <- with(df, is.finite(Age)  & (Age <= 0 | Age > 100))
sib_bad  <- with(df, is.finite(SibSp) & SibSp < 0)
parch_bad<- with(df, is.finite(Parch) & Parch < 0)
tibble::tibble(Column=c("Fare","Age","SibSp","Parch"),
               Rule=c("<=0","<=0 or >100","<0","<0"),
               Count=c(sum(fare_bad,na.rm=TRUE),sum(age_bad,na.rm=TRUE),
                       sum(sib_bad,na.rm=TRUE),sum(parch_bad,na.rm=TRUE))) %>% print()

# ---- Impute Fare (median by class) ----
med_by_class <- tapply(df$Fare[df$Fare > 0], df$Pclass[df$Fare > 0], median, na.rm = TRUE)
na_idx   <- is.na(df$Fare)
zero_idx <- df$Fare == 0
df$Fare[na_idx]   <- med_by_class[ as.character(df$Pclass[na_idx]) ]
df$Fare[zero_idx] <- med_by_class[ as.character(df$Pclass[zero_idx]) ]

# ---- Impute Age (median by Pclass × Sex groups) ----
bad_age <- is.na(df$Age) | (is.finite(df$Age) & (df$Age <= 0 | df$Age > 100))
grp <- interaction(df$Pclass, df$Sex, drop = TRUE)
age_by_grp <- ave(df$Age, grp, FUN=function(x) median(x[x > 0 & x <= 100], na.rm=TRUE))
df$Age[bad_age] <- age_by_grp[bad_age]

view(df)

# ---- Drop irrelevant columns ----
df <- df[, !(names(df) %in% c("PassengerId", "Name", "Ticket", "Cabin"))]
df

# ---- Winsorization (limit extreme outliers) ----
#his code is performing winsorization on the Fare and Age columns to limit extreme outliers
winsor_iqr <- function(x) {
  q1 <- quantile(x, 0.25, na.rm=TRUE); q3 <- quantile(x, 0.75, na.rm=TRUE)
  iqr <- q3 - q1; low <- q1 - 1.5*iqr; high <- q3 + 1.5*iqr
  pmin(pmax(x, low), high)
}

df$Fare_w <- winsor_iqr(df$Fare)
df$Age_w  <- winsor_iqr(df$Age)

install.packages("gridExtra")
library(gridExtra)

# ---- Boxplots before vs after Winsorization ----
gridExtra::grid.arrange(
  ggplot(df, aes(x=factor(1), y=Fare))   + geom_boxplot() + labs(title="Fare (Raw)", x=NULL, y="Fare"),
  ggplot(df, aes(x=factor(1), y=Fare_w)) + geom_boxplot() + labs(title="Fare (Winsorized)", x=NULL, y="Fare"),
  nrow=1
)#vertical boxplot

#raw age distribution vs winsorized age.
gridExtra::grid.arrange(
  ggplot(df, aes(x=factor(1), y=Age))   + geom_boxplot() + labs(title="Age (Raw)", x=NULL, y="Age"),
  ggplot(df, aes(x=factor(1), y=Age_w)) + geom_boxplot() + labs(title="Age (Winsorized)", x=NULL, y="Age"),
  nrow=1
)


# ---- Feature Transformation ----
df$Fare_log <- log1p(df$Fare)        # Log transform (reduce skewness)
df$Age_scaled <- scale(df$Age)       # Standardize Age (z-score)
df

# ---- Feature Engineering ----
df_feat <- read.csv("https://raw.githubusercontent.com/MahinSarkarAtul/r/main/titanic.csv")
df$FamilySize     <- df_feat$SibSp + df_feat$Parch + 1
df$IsAlone        <- ifelse(df$FamilySize==1, 1, 0)
df$FarePerPerson  <- df_feat$Fare / pmax(df$FamilySize,1)
df

# Extract passenger title (Mr, Mrs, Miss, etc.)
extract_title <- function(nm) {#nm → expects a vector of names from the dataset (Name column).
  t <- stringr::str_extract(nm, "(?<=,\\s)([^.]+)(?=\\.)")#str_extract → extracts a string that matches a regular expression
  t <- trimws(t)#Removes leading and trailing whitespace from the extracted title.
  dplyr::case_when(#case_when → vectorized if-else statement in dplyr.
    #group rare or similar titles into broader categories
    t %in% c("Mlle","Ms") ~ "Miss",
    t %in% c("Mme") ~ "Mrs",
    t %in% c("Capt","Col","Major","Dr","Rev") ~ "Officer",
    t %in% c("Don","Sir","Jonkheer") ~ "Nobleman",
    t %in% c("Lady","Dona","the Countess") ~ "Noblewoman",
    TRUE ~ t
  )
}

df$Title <- extract_title(df_feat$Name)
# Categorize Age into groups
df$AgeGroup <- cut(df$Age, breaks=c(-Inf,12,18,35,60,Inf),#cut() → discretizes numeric data into intervals (bins).
                   labels=c("Child","Teen","YoungAdult","Adult","Senior"))

df

# ---- One-hot Encoding ----

#Convert basic columns to proper types
df_encoded <- df %>%
  mutate(#mutate() → modifies or creates columns in a dplyr pipeline
    Sex = factor(Sex),#Sex, Embarked, Pclass → converted to factor (categorical type in R).
    Embarked = factor(Embarked),#Factors are needed for dummy encoding and some modeling functions.
    Pclass = factor(Pclass),
    Survived = as.integer(Survived)
  ) %>%
  #One-hot encode categorical features
  fastDummies::dummy_cols(#creates dummy/one-hot variables for categorical columns.
    select_columns = c("Sex","Embarked","Pclass","Title","AgeGroup"),
    remove_selected_columns = TRUE,#removes original factor columns after encoding.
    remove_first_dummy = TRUE#avoids dummy variable trap (prevents perfect multicollinearity by dropping the first dummy).
  )
view(df_encoded)#opens a spreadsheet-like view in RStudio.
df
head(df_encoded)
setdiff(names(df_encoded), names(df))#shows new columns created by one-hot encoding.
df_encoded

# ---- Feature Scaling z-score normalization----
scale_cols <- c("Age","Fare","Fare_w","Age_w","FamilySize","FarePerPerson")#a vector of numeric columns in the dataset that we want to normalize.
for (cn in scale_cols) {
  if (cn %in% names(df_encoded)) {#only scale the column if it exists in the dataframe.
    #Apply z-score scaling
    df_encoded[[paste0(cn,"_z")]] <- as.numeric(scale(df_encoded[[cn]]))
  }#paste0(cn,"_z") → creates a new column name for the scaled feature, e.g., Age_z, Fare_z.
}

df_encoded %>% 
  select(ends_with("_z")) %>% #selects all columns whose names end with _z (your scaled numeric features).
  head()

#Verify that scaling transformed the data (centered around 0, standard deviation = 1) without changing relative order.
data.frame(
  Raw_Age = df_encoded$Age,
  Scaled_Age = df_encoded$Age_z
) %>% head()

names(df_encoded)


str(df_encoded)
dim(df_encoded)  
View(df_encoded)

#Count unique values for all columns
column_names <- names(df)#stores all column names from the dataframe df
for (col_name in column_names) {
  cat(col_name, ":", length(unique(df[[col_name]])), "\n")
}
#Inspect unique values for specific categorical columns
cols <- c("Pclass", "Embarked")
for (col in cols) {
  if (is.character(df[[col]])) {
    cat(col, ":", unique(df[[col]]), "\n")#cat() prints the column name and its values.
  }
}

hist(na.omit(df$Age), breaks = 8, col = "lightblue",
     main = "General Passenger Profile", xlab = "Age")#breaks = 8 → divides the range of Age into 8 bin

png(filename = "histograms.png", width = 800, height = 800)
#mfrow = c(2,2) → splits the plotting area into 2 rows × 2 columns, allowing 4 plots in one image.
#mar = c(4, 4, 2, 1) → sets margins: bottom, left, top, right.
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))  # 2x2 layout, margins adjusted

hist(df$Pclass, breaks = 9, col = "lightblue", main = "Passenger Class", xlab = "Pclass")
hist(df$Survived, breaks = 9, col = "lightblue", main = "Survival Status", xlab = "Survived")
hist(df$Age, breaks = 9, col = "lightblue", main = "Age Distribution", xlab = "Age")
hist(df$SibSp, breaks = 9, col = "lightblue", main = "Siblings/Spouses Aboard", xlab = "SibSp")
dev.off()#dev.off() → closes the graphics device and saves the PNG file.

par(mfrow=c(1,4))
pie(table(df$Survived), main="Survival", labels=c("Died", "Survived"), col=c("red", "blue"))
pie(table(df$Pclass), main="Pclass", labels=c("1st", "2nd", "3rd"), col=c("yellow", "red", "blue"))
pie(table(df$Sex), main="Sex", labels=c("Male", "Female"), col=c("yellow", "red"))
pie(table(df$Embarked), main="Embarked", labels=c("Cherbourg", "Queenstown", "Southampton"), col=c("yellow", "red", "blue"))

#table(df$Pclass, df$Survived) → creates a cross-tabulation (contingency table) between passenger class (Pclass) and survival (Survived).
pclass <- table(df$Pclass, df$Survived)
pclass_percentage <- round((pclass / sum(pclass)) * 100, 2)
print(pclass_percentage)

pclass <- as.data.frame(table(df$Pclass, df$Survived))
pclass$Percentage <- round(pclass$Freq * 100 / sum(pclass$Freq), 2)
colnames(pclass) <- c("Pclass", "Survived", "Count", "Percentage")

ggplot(pclass, aes(x = factor(Pclass), y = Percentage, fill = factor(Survived))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Percentage of Survivors by Socio-Economic Class",
       x = "Pclass",
       y = "Percentage") +
  scale_fill_manual(values = c("blue", "red")) +
  scale_x_discrete(labels = c("1st", "2nd", "3rd")) +
  theme_minimal()

gender <- table(df$Sex, df$Survived)
gender_percentage <- round((gender / sum(gender)) * 100, 2)
print(gender_percentage)

gender <- as.data.frame(table(df$Sex, df$Survived))
gender$Percentage <- round(gender$Freq * 100 / sum(gender$Freq), 2)
colnames(gender) <- c("Sex", "Survived", "Count", "Percentage")
print(gender)

ggplot(gender, aes(x = factor(Sex), y = Percentage, fill = factor(Survived))) +
 # stat = "identity" → use the actual Percentage values from the dataframe instead of counting rows.
#position = "dodge" → side-by-side bars for survived vs died in each class.
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Percentage of Survivors by Gender",
       x = "Sex",
       y = "Percentage") +
  scale_fill_manual(values = c("blue", "red")) +
  scale_x_discrete(labels = c("Female", "Male")) +
  theme_minimal()#Converts x-axis factor labels (1, 2, 3) into readable labels.

ggplot(df, aes(x = Age, fill = factor(Survived))) +
  geom_density(alpha = 0.5) +
  labs(title = "Age & Survival Relationship", x = "Age", y = "Density") +
  scale_fill_manual(values = c("blue", "green")) +
  theme_minimal()


ggplot(df, aes(x = factor(Parch), fill = factor(Survived))) +
  geom_bar(position = "fill", alpha = 0.7, stat = "count") +
  labs(title = "Probability of Survival of those with Parents/Children", 
       x = "Parents/Children", y = "Percentage") +
  scale_fill_manual(values = c("blue", "green")) +
  theme_minimal()

ggplot(df, aes(x = Fare, fill = factor(Survived))) +
  geom_density(alpha = 0.5) +
  labs(title = "Fare & Survival Relationship", x = "Fare", y = "Density") +
  scale_fill_manual(values = c("blue", "green")) +
  theme_minimal()

embark <- table(df$Embarked, df$Survived)
embark_percentage <- round((embark / sum(embark)) * 100, 2)
print(embark_percentage)

ggplot(df, aes(x=factor(IsAlone), fill=factor(Survived))) +
  geom_bar(position="fill") + labs(title="Survival by IsAlone", x="IsAlone (1=yes)", y="Proportion")

ggplot(df, aes(x=Title, fill=factor(Survived))) +
  geom_bar(position="fill") + labs(title="Survival by Title", x="Title", y="Proportion") +
  theme(axis.text.x = element_text(angle=30, hjust=1))


num_only <- df_encoded %>% select(where(is.numeric))
num_only <- num_only[, sapply(num_only, function(x) !all(is.na(x)))]
corr_mat <- cor(num_only, use="pairwise.complete.obs")
ggcorrplot(corr_mat, lab=FALSE) + labs(title="Correlation Heatmap (Numeric Features)")

pp_cols <- df_encoded %>% 
  select(Survived, Age, Fare, FamilySize, FarePerPerson, starts_with("Sex_"), starts_with("Embarked_")) %>%
  select(where(function(x) is.numeric(x) || is.integer(x)))
GGally::ggpairs(pp_cols, progress = FALSE)

surv_matrix <- df %>%
  group_by(AgeGroup, Pclass) %>%
  summarise(SurvivalRate = mean(Survived), Count = n(), .groups="drop")
surv_matrix 

ggplot(surv_matrix, aes(x = factor(Pclass), y = AgeGroup, fill = SurvivalRate)) +
  geom_tile(color="white") +
  geom_text(aes(label = scales::percent(SurvivalRate, accuracy = 1)),
            color="black", size=4) +
  scale_fill_gradient(low = "red", high = "green") +
  labs(title = "Survival Rate Matrix by Age Group & Class",
       x = "Passenger Class", y = "Age Group", fill = "Survival Rate") +
  theme_minimal()

surv_matrix_sex <- df %>%
  group_by(Sex, AgeGroup, Pclass) %>%
  summarise(SurvivalRate = mean(Survived), Count = n(), .groups="drop")
surv_matrix_sex

ggplot(surv_matrix, aes(x = factor(Pclass), y = AgeGroup, fill = SurvivalRate, size = Count)) +
  geom_point(shape=21, color="black") +
  scale_fill_gradient(low="red", high="green") +
  scale_size_continuous(range = c(3, 12)) +
  labs(title="Survival Rate & Passenger Count by Age Group & Class",
       x="Passenger Class", y="Age Group", fill="Survival Rate", size="Passenger Count") +
  theme_minimal() 
df