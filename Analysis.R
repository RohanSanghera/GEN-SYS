library(readxl)
library(tidyverse)
library(gridExtra)
library(vir)

#### Early trial

bias <- c(
  rep("heavy", 23),
  rep("moderate", 23),
  rep("mild", 23),
  rep("none", 23)
)

bias_study <- rep(
  c("overall", "bellon", "buchan", "clezar", "cutting", "dopper", "ghoraba", "hjetland",
           "karkou", "lin", "lynch", "malik", "mohamed", "roy", "santos", "setthawong", "sevaux",
           "singh_glasses", "singh_pemphigoid", "sulewski", "white", "younis", "zhu"),
  4)

bias_sens <- c(
  1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
  0.985239852398524, 1.0, 1.0, 1.0, 1.0, 1.0, 0.8, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.7777777777777778, 0.9545454545454546, 1.0, 1.0,
  0.948339483394834, 0.9565217391304348, 1.0, 0.9032258064516129, 1.0, 1.0, 0.8, 1.0, 1.0, 0.9375, 1.0, 1.0, 0.9090909090909091, 0.8928571428571429, 1.0, 1.0, 1.0, 1.0, 0.9375, 0.8888888888888888, 0.9545454545454546, 0.5, 1.0,
  0.8856088560885609, 0.8695652173913043, 1.0, 0.8064516129032258, 1.0, 0.875, 0.8, 1.0, 1.0, 0.9375, 1.0, 0.95, 0.9090909090909091, 0.7142857142857143, 1.0, 1.0, 0.75, 1.0, 0.9375, 0.3333333333333333, 0.9545454545454546, 1.0, 1.0
)

bias_spec <- c(
  0.39090909090909093, 0.6, 0.85, 0.4, 0.3, 0.55, 0.05, 0.4, 0.45, 0.55, 0.15, 0.5, 0.7, 0.45, 0.2, 0.1, 0.75, 0, 0.25, 0.75, 0.35, 0.2, 0.05, 
  0.5045454545454545, 0.5, 0.8, 0.55, 0.45, 0.8, 0.5, 0.4, 0.6, 0.45, 0.5, 0.65, 0.55, 0.35, 0.6, 0.0, 0.85, 0.25, 0.25, 0.9, 0.6, 0.55, 0.0, 
  0.5545454545454546, 0.75, 0.9, 0.65, 0.55, 0.6, 0.6, 0.4, 0.65, 0.6, 0.2, 0.65, 0.85, 0.35, 0.8, 0.1, 0.9, 0.05, 0.35, 0.95, 0.65, 0.5, 0.15,
  0.7318181818181818, 0.7, 0.9, 0.9, 0.75, 0.9, 1.0, 0.75, 0.55, 0.6, 0.55, 0.95, 0.8, 0.55, 0.95, 0.25, 1.0, 0.7, 0.5, 1.0, 0.85, 0.7, 0.25
)

overall_bias_data <- data.frame(bias, bias_study, bias_sens, bias_spec)

overall_bias_roc <- ggplot(overall_bias_data, aes(x = bias_spec, y = bias_sens)) +
  geom_point(aes(color = bias), alpha = 0.5) +
  geom_path(aes(color = bias_study)) +
  ylim(0, 1) +
  xlim(1, 0) +
  ylab("Sensitivity") +
  xlab("Specificity") +
  theme_linedraw() # consider abandoning geom_path, adding jitter to points, and making all points translucent/small except for overall stats (larger, solid)

#### Prompt-dependence

library(dunn.test)
library(rcompanion)
shapiro.test(ped_first$sens) # non-normal distribution

kruskal_test <- kruskal.test(sens ~ prompt, data = ped_first)
print(kruskal_test) #significant variation 

# Dunn's Test for pairwise comparisons
dunn_results <- dunn.test(ped_first$sens, ped_first$prompt, method = "bonferroni")
print(dunn_results)

#### Prompt Engineering
# GPT-3.5 and GPT-4 data across prompt biases (with three repeats), stratified by review (with larger panel for all reviews)

# Import data
read_all_excel_files <- function(directory) {
  # Get a list of all Excel files in the directory
  excel_files <- list.files(path = directory, pattern = "\\.xlsx$", full.names = TRUE)
  
  # Read each Excel file into a data frame and store it in a list
  excel_data <- lapply(excel_files, read_excel)
  
  # Set the names of the list to be the file names without extensions
  names(excel_data) <- tools::file_path_sans_ext(basename(excel_files))
  
  # Assign each data frame in the list to the global environment
  list2env(excel_data, envir = .GlobalEnv)
}

read_all_excel_files("Results/10June 2024/gpt_results/GPT3 Sample Final")
read_all_excel_files("Results/10June 2024/gpt_results/GPT4 Sample Final")
read_all_excel_files("Results/10June 2024/gpt_results/GPT4o Sample Final") # not necessary; better to concatanate immediately

concatenate_excel_files <- function(directory) {
  # Get a list of all Excel files in the directory
  excel_files <- list.files(path = directory, pattern = "\\.xlsx$", full.names = TRUE)
  
  # Read each Excel file into a data frame
  excel_data <- lapply(excel_files, read_excel)
  
  # Concatenate all data frames into one long data frame
  combined_data <- bind_rows(excel_data)
  
  # Return the combined data frame
  return(combined_data)
}

# Initialize an empty data frame to store the combined data
prompt_engineering_data <- data.frame()

# List of directories to process
directories <- c("Results/28Aug2024/Stats/Sample Runs/GPT3 Sample Final",
                 "Results/28Aug2024/Stats/Sample Runs/GPT4 Sample Final",
                 "Results/28Aug2024/Stats/Sample Runs/GPT4o Sample Final",
                 "Results/28Aug2024/Stats/Sample Runs/llama3 Sample Final",
                 "Results/28Aug2024/Stats/Sample Runs/geminipro Sample Final",
                 "Results/28Aug2024/Stats/Sample Runs/sonnet Sample Final") # check directory names (consider adding human data by changing column names: "Results/28Aug2024/Stats/Sample Runs/Human Reference Stats")
# Ensure format of files is consistent

# Loop through each directory, read the Excel files, and concatenate the data
for (dir in directories) {
  folder_data <- concatenate_excel_files(dir)
  prompt_engineering_data <- bind_rows(prompt_engineering_data, folder_data)
}

# Plot data

reviews <- c("all", "bellon", "buchan", "clezar",
             "cutting", "dopper", "ghoraba", "hjetland",
             "karkou", "lin", "lynch", "malik",
             "mohamed", "roy", "santos", "setthawong",
             "sevaux", "singh_glasses", "singh_pemphigoid", "sulewski",
             "sulistyo", "white", "younis", "zhu") # list of all reviews (to facilitate for loop)

View(prompt_engineering_data) # all data

ped_first <- prompt_engineering_data[prompt_engineering_data$`repeat no.`==1, ]
ped_second <- prompt_engineering_data[prompt_engineering_data$`repeat no.`==2, ]
ped_third <- prompt_engineering_data[prompt_engineering_data$`repeat no.`==3, ]

ped_first[ped_first$sens == 0,]$sens <- 1 # assume sensitivity perfect where no included articles in test set
ped_first$prompt <- factor(ped_first$prompt, levels = c("extreme", "heavy", "moderate", "mild", "none", "title")) #reorder factor so colours/key are in order

for (X in reviews) {
  # Create ggplot for each value
  assign(paste0("prompt_trial_", X),
         ggplot(data = ped_first[ped_first$review == X, ], aes(x = sens, y = ppv)) +
           geom_point(aes(shape = model, colour = prompt), alpha = 0.75) +
           geom_hline(yintercept = 0.5, linetype = "dotted", colour = "black") +
           scale_shape_manual(values = c(1, 0, 6, 2, 5, 3)) + # add 4/8/9 as required for more models
           scale_colour_manual(values = c("#003f5c", "#444e86", "#955196", "#dd5182", "#ff6e54", "#ffa600")) + # reorder factor to avoid alphabetical listing
           ylim(0, 1) +
           xlim(0, 1) +
           ylab("Precision") +
           xlab("Recall") +
           ggtitle(X) +
           theme_linedraw() +
           theme(legend.position="none") +
           theme(axis.text=element_text(size = 8))
         ) # change point shape to correspond with model; consider using colour to illustrate prompt; add data corresponding to repeat trials; consider using geom_count to plot overlapping data w size
} 

prompt_trial_all <- ggplot(data = ped_first[ped_first$review == "all", ], aes(x = sens, y = ppv)) +
  geom_point(aes(shape = model, colour = prompt), alpha = 0.75, size = 4) + 
  geom_hline(yintercept = 0.5, linetype = "dotted", colour = "black") +
  scale_shape_manual(values = c(1, 0, 6, 2, 5, 3)) + # add 4/8/9 as required for more models
  scale_colour_manual(values = c("#003f5c", "#444e86", "#955196", "#dd5182", "#ff6e54", "#ffa600")) +
  ylim(0, 1) +
  xlim(0, 1) +
  ylab("Precision") +
  xlab("Recall") +
  ggtitle("All reviews") +
  theme_linedraw() +
  theme(legend.position="none") # replace 'all' plot with larger points

# use one plot to generate useful key for colour and shapes.

lay <- rbind(
  c(1,1,1,2,3,4,5,6),
  c(1,1,1,7,8,9,10,11),
  c(1,1,1,12,13,14,15,16),
  c(17,18,19,20,21,22,23,24)
)

grid.arrange(
  grobs = list(
    prompt_trial_all, prompt_trial_bellon, prompt_trial_buchan, prompt_trial_clezar,
    prompt_trial_cutting, prompt_trial_dopper, prompt_trial_ghoraba, prompt_trial_hjetland,
    prompt_trial_karkou, prompt_trial_lin, prompt_trial_lynch, prompt_trial_malik,
    prompt_trial_mohamed, prompt_trial_roy, prompt_trial_santos, prompt_trial_setthawong,
    prompt_trial_sevaux, prompt_trial_singh_glasses, prompt_trial_singh_pemphigoid, prompt_trial_sulewski,
    prompt_trial_sulistyo, prompt_trial_white, prompt_trial_younis, prompt_trial_zhu
  ),
  layout_matrix = lay
) # ENSURE THAT THE CORRECT DATA IS USED (PED_FIRST/SECOND/THIRD). Print 

# add data from human replication
# consider no multipanel plot for prompt engineering

prompt_trial_all +
  theme(legend.position = "bottom") +
  ggtitle("Abstract screening performance of LLMs across a range of prompts") +
  guides(color = guide_legend(nrow = 2)) # to extract legend

### Human comparison

## Kappa statistic heat map

filtered_kappa_pairs <- read_excel("Results/6Oct2024/filtered_kappa_pairs_updated.xlsx")
all_kappa_pairs <- read_excel("Results/6Oct2024/kappa_pairs_updated.xlsx")

library(reshape2)

### Filtered (best LLM prompts only)

filtered_kappa_pairs$Label1 <- paste(filtered_kappa_pairs$Model1, filtered_kappa_pairs$Bias1, sep = "_")
filtered_kappa_pairs$Label2 <- paste(filtered_kappa_pairs$Model2, filtered_kappa_pairs$Bias2, sep = "_")
filtered_kappa_matrix <- filtered_kappa_pairs[, c("Label1", "Label2", "All")]

filtered_kappa_matrix$Label1 <- factor(filtered_kappa_matrix$Label1, levels = unique(filtered_kappa_matrix$Label1))
filtered_kappa_matrix$Label2 <- factor(filtered_kappa_matrix$Label2, levels = unique(filtered_kappa_matrix$Label2))

melted_filtered_kappa_matrix <- melt(
  filtered_kappa_matrix,
  id.vars = c("Label1", "Label2"),
  variable.name = "Metric",
  value.name = "Kappa"
  )

ggplot(filtered_kappa_matrix, aes(x=Label1, y=Label2, fill=All)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(limit = c(0, 1), name="Cohen's Kappa") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1)) +
  coord_fixed() +
  theme_bw() + 
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = "bottom",
    legend.direction = "horizontal",
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1, title.position = "top", title.hjust = 0.5)) + 
  geom_text(aes(Label1, Label2, label = round(All, digits = 2)), color = "white", size = 3)

### All permutations (no numbers)

library(reshape2)
melted_kappa_matrix <- melt(lower_kappa_matrix)
ggplot(melted_kappa_matrix, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(limit = c(0,1), name="Cohen's Kappa") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1)) +
  coord_fixed() +
  theme_bw() + 
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal",
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1, title.position = "top", title.hjust = 0.5)) + 
  geom_text(aes(Var1, Var2, label = round(value, digits = 2)), color = "white", size = 3)

#### Full trial

GPT3_heavy_full <- read_excel("Results/28Aug2024/Stats/Full Runs/GPT3 Full Final/GPT3_heavy_full_stats.xlsx")
GPT4o_extreme_full <- read_excel("Results/28Aug2024/Stats/Full Runs/GPT4o Full Final/GPT4o_extreme_full_stats.xlsx")
LLaMA3_none_full <- read_excel("Results/2Sep2024/llama3 Full Final/llama3_none_full_stats.xlsx")
Gemini_heavy_full <- read_excel("Results/28Aug2024/Stats/Full Runs/geminipro Full Final/geminipro_heavy_full_stats.xlsx")
Sonnet_none_full <- read_excel("Results/2Sep2024/sonnet Full Final/sonnet_none_full_stats.xlsx")

full_data <- do.call("rbind", list(GPT3_heavy_full, GPT4o_extreme_full, LLaMA3_none_full, Gemini_heavy_full, Sonnet_none_full)) # concatanate dataframes
full_data[full_data$sens == 0,]$sens <- 1 # assume sensitivity perfect where no included articles in test set
# consider full_data$prompt <- factor(full_data$prompt, levels = c("extreme", "heavy", "moderate", "mild", "none", "title")) if needed to reorder

# add sens (1.00) and spec (calculation required) for real humans from original reviews

for (X in reviews) {
  # Create ggplot for each value
  assign(paste0("full_trial_", X),
         ggplot(data = full_data[full_data$review == X, ], aes(x = sens, y = ppv)) + # change data source
           geom_point(aes(shape = model, colour = "#FF1F5B")) +
           geom_hline(yintercept = 0.5, linetype = "dotted", colour = "black") +
           scale_shape_manual(values = c(1, 0, 2, 5, 3)) + # 6 for GPT-4, 4/8/9 as required for more models
           ylim(0, 1) +
           xlim(0, 1) +
           ylab("Precision") +
           xlab("Recall") +
           ggtitle(X) + # ADD human data from original reviews; colour = #009ADE, shape = cross or point
           theme_linedraw() +
           theme(legend.position="none") +
           theme(axis.text=element_text(size = 8))
  )
}

full_trial_all <- ggplot() +
  geom_point(data = full_data[full_data$review == "all", ], aes(x = sens, y = ppv, shape = model, colour = "#FF1F5B"), size = 4) + # model data
  geom_point() + # human data
  geom_point() + # cochrane data
  geom_hline(yintercept = 0.5, linetype = "dotted", colour = "black") +
  scale_shape_manual(values = c(1, 0, 2, 5, 3)) + # add 4/8/9 as required for more models (or 6 for GPT-4)
  ylim(0, 1) +
  xlim(0, 1) +
  ylab("Precision") +
  xlab("Recall") +
  ggtitle("All reviews") + # ADD human data from original reviews; colour = #009ADE, shape = cross or point
  theme_linedraw() +
  theme(legend.position="none") # replace 'all' plot with larger points
  
grid.arrange(
  grobs = list(
    full_trial_all, full_trial_bellon, full_trial_buchan, full_trial_clezar,
    full_trial_cutting, full_trial_dopper, full_trial_ghoraba, full_trial_hjetland,
    full_trial_karkou, full_trial_lin, full_trial_lynch, full_trial_malik,
    full_trial_mohamed, full_trial_roy, full_trial_santos, full_trial_setthawong,
    full_trial_sevaux, full_trial_singh_glasses, full_trial_singh_pemphigoid, full_trial_sulewski,
    full_trial_sulistyo, full_trial_white, full_trial_younis, full_trial_zhu
  ),
  layout_matrix = lay
)



full_trial_all +
  theme(legend.position = "bottom") +
  ggtitle("Abstract screening performance across all records") +
  guides(color = guide_legend(nrow = 2)) # to extract legend


#### Combined human/LLM performance

parallel_results <- read_excel("Results/18Sep2024/parallel_results.xlsx", 
                               col_types = c("text", "text", "text", 
                                             "text", "text", "text", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric"))

parallel_results[(
  parallel_results$model1 != "reviewer1" & parallel_results$model1 != "reviewer2" & parallel_results$model1 != "reviewer3" &
  (parallel_results$model2 == "reviewer1" | parallel_results$model2 == "reviewer2" | parallel_results$model2 == "reviewer3")
  ),]$components <-"AH"

parallel_results[(
  parallel_results$model2 != "reviewer1" & parallel_results$model2 != "reviewer2" & parallel_results$model2 != "reviewer3" &
    (parallel_results$model1 == "reviewer1" | parallel_results$model1 == "reviewer2" | parallel_results$model1 == "reviewer3")
),]$components <-"AH" 

parallel_results[(
  parallel_results$model2 != "reviewer1" & parallel_results$model2 != "reviewer2" & parallel_results$model2 != "reviewer3" &
  parallel_results$model1 != "reviewer1" & parallel_results$model1 != "reviewer2" & parallel_results$model1 != "reviewer3"
),]$components <-"AA" 

parallel_results[(
  (parallel_results$model2 == "reviewer1" | parallel_results$model2 == "reviewer2" | parallel_results$model2 == "reviewer3") &
  (parallel_results$model1 == "reviewer1" | parallel_results$model1 == "reviewer2" | parallel_results$model1 == "reviewer3")
),]$components <-"HH" 


series_results <- read_excel("Results/18Sep2024/series_results.xlsx", 
                               col_types = c("text", "text", "text", 
                                             "text", "text", "text", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric"))

series_results[(
  series_results$model1 != "reviewer1" & series_results$model1 != "reviewer2" & series_results$model1 != "reviewer3" &
    (series_results$model2 == "reviewer1" | series_results$model2 == "reviewer2" | series_results$model2 == "reviewer3")
),]$components <-"AH"

series_results[(
  series_results$model2 != "reviewer1" & series_results$model2 != "reviewer2" & series_results$model2 != "reviewer3" &
    (series_results$model1 == "reviewer1" | series_results$model1 == "reviewer2" | series_results$model1 == "reviewer3")
),]$components <-"AH" 

series_results[(
  series_results$model2 != "reviewer1" & series_results$model2 != "reviewer2" & series_results$model2 != "reviewer3" &
    series_results$model1 != "reviewer1" & series_results$model1 != "reviewer2" & series_results$model1 != "reviewer3"
),]$components <-"AA" 

series_results[(
  (series_results$model2 == "reviewer1" | series_results$model2 == "reviewer2" | series_results$model2 == "reviewer3") &
    (series_results$model1 == "reviewer1" | series_results$model1 == "reviewer2" | series_results$model1 == "reviewer3")
),]$components <-"HH"

# Data for every permutation in one dataframe
ensemble_data <- rbind(series_results, parallel_results)

# PRCs with highest accuracy plot coloured gold

## Human (red) and LLM (blue) in series - precision recall curve
ah_series <- ggplot(ensemble_data[(ensemble_data$combination_type == "series" & ensemble_data$components == "AH"),],
       aes(x = sens, y = ppv)) +
  geom_point(aes(color = "black"), shape = 20, size = 1, 
             data = ~ subset(., accu != max(accu))) +  # Plot black points first
  geom_point(aes(color = "gold"), shape = 20, size = 1, 
             data = ~ subset(., accu == max(accu))) +  # Plot the gold point last
  geom_point(aes(x = 1, y = 0.235039029), colour = "red", shape = 8) +
  ylim(0, 1) +
  xlim(0, 1) +
  xlab("Recall") +
  ylab("Precision") +
  geom_hline(yintercept = 0.5, linetype = 'dotted', col = 'black') +
  scale_color_identity() +  # Use the specified colors directly
  ggtitle("LLM & human in series") +
  theme_linedraw()

## LLMs in series
aa_series <- ggplot(ensemble_data[(ensemble_data$combination_type == "series" & ensemble_data$components == "AA"),],
                    aes(x = sens, y = ppv)) +
  geom_point(aes(color = "black"), shape = 20, size = 1, 
             data = ~ subset(., accu != max(accu))) +  # Plot black points first
  geom_point(aes(color = "gold"), shape = 20, size = 1, 
             data = ~ subset(., accu == max(accu))) +  # Plot the gold point last
  geom_point(aes(x = 1, y = 0.235039029), colour = "red", shape = 8) +
  ylim(0, 1) +
  xlim(0, 1) +
  xlab("Recall") +
  ylab("Precision") +
  geom_hline(yintercept = 0.5, linetype = 'dotted', col = 'black') +
  scale_color_identity() +  # Use the specified colors directly
  ggtitle("LLM & LLM in series") +
  theme_linedraw()

## Humans in series
hh_series <- ggplot(ensemble_data[(ensemble_data$combination_type == "series" & ensemble_data$components == "HH"),],
                    aes(x = sens, y = ppv)) +
  geom_point(aes(color = "black"), shape = 20, size = 1, 
             data = ~ subset(., accu != max(accu))) +  # Plot black points first
  geom_point(aes(color = "gold"), shape = 20, size = 1, 
             data = ~ subset(., accu == max(accu))) +  # Plot the gold point last
  geom_point(aes(x = 1, y = 0.235039029), colour = "red", shape = 8) +
  ylim(0, 1) +
  xlim(0, 1) +
  xlab("Recall") +
  ylab("Precision") +
  geom_hline(yintercept = 0.5, linetype = 'dotted', col = 'black') +
  scale_color_identity() +  # Use the specified colors directly
  ggtitle("Human & human in series") +
  theme_linedraw()

## Human (red) and LLM (blue) in parallel
ah_parallel <- ggplot(ensemble_data[(ensemble_data$combination_type == "parallel" & ensemble_data$components == "AH"),],
                    aes(x = sens, y = ppv)) +
  geom_point(aes(color = "black"), shape = 20, size = 1, 
             data = ~ subset(., accu != max(accu))) +  # Plot black points first
  geom_point(aes(color = "gold"), shape = 20, size = 1, 
             data = ~ subset(., accu == max(accu))) +  # Plot the gold point last
  geom_point(aes(x = 1, y = 0.235039029), colour = "red", shape = 8) +
  ylim(0, 1) +
  xlim(0, 1) +
  xlab("Recall") +
  ylab("Precision") +
  geom_hline(yintercept = 0.5, linetype = 'dotted', col = 'black') +
  scale_color_identity() +  # Use the specified colors directly
  ggtitle("LLM & human in parallel") +
  theme_linedraw()

## LLMs in parallel
aa_parallel <- ggplot(ensemble_data[(ensemble_data$combination_type == "parallel" & ensemble_data$components == "AA"),],
                      aes(x = sens, y = ppv)) +
  geom_point(aes(color = "black"), shape = 20, size = 1, 
             data = ~ subset(., accu != max(accu))) +  # Plot black points first
  geom_point(aes(color = "gold"), shape = 20, size = 1, 
             data = ~ subset(., accu == max(accu))) +  # Plot the gold point last
  geom_point(aes(x = 1, y = 0.235039029), colour = "red", shape = 8) +
  ylim(0, 1) +
  xlim(0, 1) +
  xlab("Recall") +
  ylab("Precision") +
  geom_hline(yintercept = 0.5, linetype = 'dotted', col = 'black') +
  scale_color_identity() +  # Use the specified colors directly
  ggtitle("LLM & LLM in parallel") +
  theme_linedraw()

## Humans in parallel
hh_parallel <- ggplot(ensemble_data[(ensemble_data$combination_type == "parallel" & ensemble_data$components == "HH"),],
                      aes(x = sens, y = ppv)) +
  geom_point(aes(color = "black"), shape = 20, size = 1, 
             data = ~ subset(., accu != max(accu))) +  # Plot black points first
  geom_point(aes(color = "gold"), shape = 20, size = 1, 
             data = ~ subset(., accu == max(accu))) +  # Plot the gold point last
  geom_point(aes(x = 1, y = 0.235039029), colour = "red", shape = 8) +
  ylim(0, 1) +
  xlim(0, 1) +
  xlab("Recall") +
  ylab("Precision") +
  geom_hline(yintercept = 0.5, linetype = 'dotted', col = 'black') +
  scale_color_identity() +  # Use the specified colors directly
  ggtitle("Human & human in parallel") +
  theme_linedraw()

grid.arrange(ah_series, aa_series, hh_series, ah_parallel, aa_parallel, hh_parallel,
             nrow = 2) # print 6x9 inches

## Best ensembles

View(
  ensemble_data[ensemble_data$sens == 1, ]
) # Number of perfect ensembles = N/2 (except for human-involved ones) therefore 120/2 + 6 = 66

# Review-dependency

## Data cleaning
rd_llm <- ped_first[c(1,2,4,16:21)]
rd_llm$balacc <- (rd_llm$`true sens` + rd_llm$`true spec`) / 2

rd_human <- read_excel("Results/6Oct2024/human conc.xlsx")

rd_longdf <- rbind(rd_llm, rd_human)
rd_longdf <- rd_longdf %>%
  filter(review != 'all')
rd_longdf$prompt <- as.character(rd_longdf$prompt)


## Analysis

### Scatter plots - run from previous section to ensure data clean without name appending

#### Balacc
human_balacc <- rd_longdf %>%
  filter(model %in% c("reviewer1", "reviewer2", "reviewer3"), prompt == "human") %>%
  select(review, balacc, model)

# Filter the data for LLM models
llm_data <- rd_longdf %>%
  filter(model %in% c("GPT3", "GPT4", "GPT4o", "llama3", "geminipro", "sonnet"))

# Create a new dataframe with all combinations of human and LLM performances
rd_plot_balacc <- llm_data %>%
  left_join(human_balacc, by = "review", suffix = c("_llm", "_human"))

# Create the scatter plot
balacc_plot <- ggplot(rd_plot_balacc, aes(x = balacc_llm, y = balacc_human, color = model_human, shape = model_llm)) +
  geom_point(size = 1, alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
  labs(
    title = "Balanced Accuracy",
    y = "Human Performance",
    x = "LLM Performance",
    color = "Human Reviewer",
    shape = "LLM Model"
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_shape_manual(values = c(1, 0, 6, 2, 5, 3)) +
  coord_fixed(ratio = 1, xlim = c(0, 1), ylim = c(0, 1))

#### TrueSens
human_true_sens <- rd_longdf %>%
  filter(model %in% c("reviewer1", "reviewer2", "reviewer3"), prompt == "human") %>%
  select(review, `true sens`, model)

# Filter the data for LLM models
llm_data <- rd_longdf %>%
  filter(model %in% c("GPT3", "GPT4", "GPT4o", "llama3", "geminipro", "sonnet"))

# Create a new dataframe with all combinations of human and LLM performances
rd_plot_true_sens <- llm_data %>%
  left_join(human_true_sens, by = "review", suffix = c("_llm", "_human"))

# Create the scatter plot
sens_plot <- ggplot(rd_plot_true_sens, aes(x = `true sens_llm`, y = `true sens_human`, color = model_human, shape = model_llm)) +
  geom_point(size = 1, alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
  labs(
    title = "Sensitivity",
    y = "Human Performance",
    x = "LLM Performance",
    color = "Human Reviewer",
    shape = "LLM Model"
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_shape_manual(values = c(1, 0, 6, 2, 5, 3)) +
  coord_fixed(ratio = 1, xlim = c(0, 1), ylim = c(0, 1))

#### TrueSpec
human_true_spec <- rd_longdf %>%
  filter(model %in% c("reviewer1", "reviewer2", "reviewer3"), prompt == "human") %>%
  select(review, `true spec`, model)

# Filter the data for LLM models
llm_data <- rd_longdf %>%
  filter(model %in% c("GPT3", "GPT4", "GPT4o", "llama3", "geminipro", "sonnet"))

# Create a new dataframe with all combinations of human and LLM performances
rd_plot_true_spec <- llm_data %>%
  left_join(human_true_spec, by = "review", suffix = c("_llm", "_human"))

# Create the scatter plot
spec_plot <- ggplot(rd_plot_true_spec, aes(x = `true spec_llm`, y = `true spec_human`, color = model_human, shape = model_llm)) +
  geom_point(size = 1, alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
  labs(
    title = "Specificity",
    y = "Human Performance",
    x = "LLM Performance",
    color = "Human Reviewer",
    shape = "LLM Model"
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_shape_manual(values = c(1, 0, 6, 2, 5, 3)) +
  coord_fixed(ratio = 1, xlim = c(0, 1), ylim = c(0, 1))

#### TruePPV
human_true_ppv <- rd_longdf %>%
  filter(model %in% c("reviewer1", "reviewer2", "reviewer3"), prompt == "human") %>%
  select(review, `true ppv`, model)

# Filter the data for LLM models
llm_data <- rd_longdf %>%
  filter(model %in% c("GPT3", "GPT4", "GPT4o", "llama3", "geminipro", "sonnet"))

# Create a new dataframe with all combinations of human and LLM performances
rd_plot_true_ppv <- llm_data %>%
  left_join(human_true_ppv, by = "review", suffix = c("_llm", "_human"))

# Create the scatter plot
ppv_plot <- ggplot(rd_plot_true_ppv, aes(x = `true ppv_llm`, y = `true ppv_human`, color = model_human, shape = model_llm)) +
  geom_point(size = 1, alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
  labs(
    title = "Positive Predictive Value",
    y = "Human Performance",
    x = "LLM Performance",
    color = "Human Reviewer",
    shape = "LLM Model"
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_shape_manual(values = c(1, 0, 6, 2, 5, 3)) +
  coord_fixed(ratio = 1, xlim = c(0, 1), ylim = c(0, 1))

#### TrueNPV
human_true_npv <- rd_longdf %>%
  filter(model %in% c("reviewer1", "reviewer2", "reviewer3"), prompt == "human") %>%
  select(review, `true npv`, model)

# Filter the data for LLM models
llm_data <- rd_longdf %>%
  filter(model %in% c("GPT3", "GPT4", "GPT4o", "llama3", "geminipro", "sonnet"))

# Create a new dataframe with all combinations of human and LLM performances
rd_plot_true_npv <- llm_data %>%
  left_join(human_true_npv, by = "review", suffix = c("_llm", "_human"))

# Create the scatter plot
npv_plot <- ggplot(rd_plot_true_npv, aes(x = `true npv_llm`, y = `true npv_human`, color = model_human, shape = model_llm)) +
  geom_point(size = 1, alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
  labs(
    title = "Negative Predictive Value",
    y = "Human Performance",
    x = "LLM Performance",
    color = "Human Reviewer",
    shape = "LLM Model"
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_shape_manual(values = c(1, 0, 6, 2, 5, 3)) +
  coord_fixed(ratio = 1, xlim = c(0, 1), ylim = c(0, 1))

#### TrueF1
human_true_f1 <- rd_longdf %>%
  filter(model %in% c("reviewer1", "reviewer2", "reviewer3"), prompt == "human") %>%
  select(review, `true f1`, model)

# Filter the data for LLM models
llm_data <- rd_longdf %>%
  filter(model %in% c("GPT3", "GPT4", "GPT4o", "llama3", "geminipro", "sonnet"))

# Create a new dataframe with all combinations of human and LLM performances
rd_plot_true_f1 <- llm_data %>%
  left_join(human_true_f1, by = "review", suffix = c("_llm", "_human"))

# Create the scatter plot
f1_plot <- ggplot(rd_plot_true_f1, aes(x = `true f1_llm`, y = `true f1_human`, color = model_human, shape = model_llm)) +
  geom_point(size = 1, alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
  labs(
    title = "F1 Score",
    y = "Human Performance",
    x = "LLM Performance",
    color = "Human Reviewer",
    shape = "LLM Model"
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_shape_manual(values = c(1, 0, 6, 2, 5, 3)) +
  coord_fixed(ratio = 1, xlim = c(0, 1), ylim = c(0, 1))

grid.arrange(sens_plot, spec_plot, balacc_plot, f1_plot, ppv_plot, npv_plot, nrow=2)

#### Correlation analysis

cor.test(rd_plot_balacc$balacc_human, rd_plot_balacc$balacc_llm)
cor.test(rd_plot_true_f1$`true f1_human`, rd_plot_true_f1$`true f1_llm`)
cor.test(rd_plot_true_ppv$`true ppv_human`, rd_plot_true_ppv$`true ppv_llm`)
cor.test(rd_plot_true_npv$`true npv_human`, rd_plot_true_npv$`true npv_llm`)
cor.test(rd_plot_true_sens$`true sens_human`, rd_plot_true_sens$`true sens_llm`)
cor.test(rd_plot_true_spec$`true spec_human`, rd_plot_true_spec$`true spec_llm`)




