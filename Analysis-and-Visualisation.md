VisualisationAndAnalysis
================
NashraAhmad

# Initialise

## Libraries

``` r
library(emmeans)
library(dplyr)
library(ggplot2)
library(ggforce)
library(tidyverse)
library(ggplot2)
library(stringr)
library(lme4)
library(ggpubr)
library(ggthemes)
library(rstatix)
```

## Read data

``` r
dStudy2<-read.csv('PreprocessedData.csv',skip = 0,header = TRUE)
dWStudy1<-read.csv('Westerndata_outlierremoved.csv',skip = 0,header = TRUE)
dWStudy1 <- subset(dWStudy1, select = -c(Familiarity, name))
dIStudy1<-read.csv('Indiandata_outlierremoved.csv',skip = 0,header = TRUE)
dIStudy1<- subset(dIStudy1, select = -name)
```

## Combining data

Deleting cells from Study1

``` r
dWStudy1$Familiarity <- "Unfamiliar"
dIStudy1$Familiarity <- "Familiar"
dcomStudy1 <- rbind(dWStudy1, dIStudy1)
dcomStudy1 <- dcomStudy1 %>%filter(type != "Naturll" & type != "Naturl2"& type !="Complex" & type !=    "Basic" & type !="sp1") #remove extra stimuli
dcomStudy1$type <- ifelse(dcomStudy1$type == "sd1", "SD", ifelse(dcomStudy1$type == "Naturl", "Natural", dcomStudy1$type))

#Change this if participant groups change
dcomStudy1 <- dcomStudy1 %>%filter(!(Familiarity == "Familiar" & Musicianship == "Musician"))#Delete Indian Musicians (Familiar Musicians)
```

Combine Study 1 and Study 2 Data

``` r
dcom <- rbind(dcomStudy1, dStudy2)
```

Renaming patterns as numbers in a different column

``` r
dcom <- dcom %>%mutate(Length = pattern)
dcom <- dcom %>%mutate(Length = case_when(Length == "Rupak" ~ 7,Length == "Keherva" ~ 8,Length == "Jhaptal" ~ 10,Length == "Teental" ~ 16,))

#dcom is ready for analysis
```

A Simple Visualisation

``` r
data1 <- dcom %>%mutate(Groups = paste(Familiarity, Musicianship, sep = " ")) %>%group_by(pattern, Length, type, Groups, condition)

pd <- position_dodge(width = 0.8) 

vis1 <- data1 %>%summarise(M = mean(similarity), n=n(), SE = sd(similarity) / sqrt(n))
```

    ## `summarise()` has grouped output by 'pattern', 'Length', 'type', 'Groups'. You
    ## can override using the `.groups` argument.

``` r
g_plot1 <- ggplot(vis1, aes(x = factor(Length), y = M, color = type)) +
  geom_point(position = pd) +
  geom_errorbar(aes(ymin = M - SE, ymax = M + SE), width = 0.2, position = pd) +
  facet_wrap(. ~ condition + Groups, scales = "free") +
  scale_y_continuous(limits = c(1, 7), breaks = seq(1, 7, by = 2)) +
  scale_color_brewer(palette = "Set1") +
  ylab("Similarity Rating") +
  xlab("No. of Beats") +
  theme_bw()
print(g_plot1)
```

![](Analysis-and-Visualisation_files/figure-gfm/Visualise%201-1.png)<!-- -->

``` r
ggsave("g_plot1.png", plot = g_plot1, dpi = 300, width = 8, height = 6)
#plot for effect of length Average
data1.2 <- data1[, !names(data1) %in% 'condition']
colnames(data1.2)
```

    ##  [1] "ResponseId"   "Musicianship" "value"        "pattern"      "type"        
    ##  [6] "value_c"      "similarity"   "Familiarity"  "Length"       "Groups"

``` r
vis1.2 <- data1.2 %>%summarise(M = mean(similarity), n=n(), SE = sd(similarity) / sqrt(n))
```

    ## `summarise()` has grouped output by 'pattern', 'Length', 'type'. You can
    ## override using the `.groups` argument.

``` r
g_plot1.2 <- ggplot(vis1.2, aes(x = factor(Length), y = M, color = type)) +
  geom_point(position = pd) +
  geom_errorbar(aes(ymin = M - SE, ymax = M + SE), width = 0.2, position = pd) +
  facet_wrap(. ~ Groups, scales = "free") +
  scale_y_continuous(limits = c(1, 7), breaks = seq(1, 7, by = 2)) +
  scale_color_brewer(palette = "Set1") +
  ylab("Similarity Ratings") +
  xlab("No. of Beats") +
  theme_bw()
print(g_plot1.2)
```

![](Analysis-and-Visualisation_files/figure-gfm/Visualise%201-2.png)<!-- -->

``` r
ggsave("g_plot1.2.png", plot = g_plot1.2, dpi = 300, width = 8, height = 6)
```

# Statisitcs for similarity responses

Models and testing 1. Lmm

``` r
#dcom2 <- dcom %>% select(-Length)

# Step 1: Fit the Model
lmer_model1 <- lmer(similarity ~ pattern + Familiarity + Musicianship + condition + type + pattern:Familiarity + pattern:Musicianship + pattern:condition +Familiarity:Musicianship + Familiarity:condition + Musicianship:condition +type:pattern + type:Familiarity + type:Musicianship + type:condition  + (1 | ResponseId), data = dcom)
```

    ## fixed-effect model matrix is rank deficient so dropping 1 column / coefficient

``` r
emm1 <- emmeans(lmer_model1, ~  pattern + Familiarity + Musicianship + condition + type + pattern:Familiarity + pattern:Musicianship + pattern:condition +Familiarity:Musicianship + Familiarity:condition + Musicianship:condition +type:pattern + type:Familiarity + type:Musicianship + type:condition)
jt<-joint_tests(emm1)
print(knitr::kable(jt))
```

    ## 
    ## 
    ## |   |model term             | df1|    df2| F.ratio|   p.value|
    ## |:--|:----------------------|---:|------:|-------:|---------:|
    ## |1  |pattern                |   3| 187.47|   1.018| 0.3858237|
    ## |13 |condition              |   1| 586.30|   1.162| 0.2814787|
    ## |15 |type                   |   1| 585.10| 244.545| 0.0000000|
    ## |2  |pattern:Familiarity    |   3| 187.47|   0.718| 0.5425793|
    ## |3  |pattern:Musicianship   |   3| 187.47|   0.598| 0.6168564|
    ## |4  |pattern:condition      |   3| 585.10|   0.498| 0.6836635|
    ## |5  |pattern:type           |   3| 585.10|   4.711| 0.0029363|
    ## |8  |Familiarity:condition  |   1| 586.35|   0.025| 0.8742238|
    ## |9  |Familiarity:type       |   1| 585.10|   6.791| 0.0093938|
    ## |11 |Musicianship:condition |   1| 585.12|   0.064| 0.8000369|
    ## |12 |Musicianship:type      |   1| 585.10|  17.674| 0.0000303|
    ## |14 |condition:type         |   1| 585.10|  13.271| 0.0002935|
    ## |16 |(confounded)           |   2| 195.05|   2.162| 0.1178451|

2.  Poshoc a Overall differences in length with average of Base and test
    and b Considering effect of Learning

``` r
#a
emm1 <- emmeans(lmer_model1, ~ Familiarity*Musicianship*type * pattern)
pairwise_emm1 <- pairs(emm1, by = NULL, adjust = "bonferroni")
pairwise_summary1 <- summary(pairwise_emm1)

#b
emm2 <- emmeans(lmer_model1, ~ Familiarity*Musicianship*type * pattern*condition)
pairwise_emm2 <- pairs(emm2, by = NULL, adjust = "bonferroni")
pairwise_summary2 <- summary(pairwise_emm2)
```

\#Finalising figure

``` r
data1 <- dcom %>%
  mutate(Groups = paste(Familiarity, Musicianship, sep = " ")) %>%
  group_by(pattern, Length, type, Groups, condition) %>%
  mutate(
    Groups = ifelse(Groups == "Familiar Non-Musician", "A. Familiar Non-Musician",
                    ifelse(Groups == "Unfamiliar Musician", "B. Unfamiliar Musician", 
                           ifelse(Groups == "Unfamiliar Non-Musician", "C. Unfamiliar Non-Musician", Groups)
                    )
    ),
    type = ifelse(type == "Natural", "1. Reference", 
                  ifelse(type == "SD", "2. Altered", type))
  )


# Check dimensions of datasets
dim(dcom)
```

    ## [1] 798  10

``` r
dim(data1)
```

    ## [1] 798  11

``` r
# Position dodge for better visualization
pd <- position_dodge(width = 0.8) 

# Summarize data for visualization
vis1 <- data1 %>%
  summarise(M = mean(similarity, na.rm = TRUE), 
            n = n(), 
            SE = sd(similarity, na.rm = TRUE) / sqrt(n))
```

    ## `summarise()` has grouped output by 'pattern', 'Length', 'type', 'Groups'. You
    ## can override using the `.groups` argument.

``` r
# Perform t-test with Bonferroni correction
stat.test <- data1 %>%
  group_by(condition, pattern, Length, Groups) %>%
  t_test(similarity ~ type, p.adjust.method = "bonferroni") %>%
  add_significance()



stat.test <- stat.test %>%
  add_xy_position(x = "Length", fun = "mean_sd", dodge = 0.8)

stat.test <- stat.test %>%
  mutate(p.signif = c("ns", "**", "ns", "ns", "***", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "***", "ns", "ns", "***", "***", "ns", "*", "***", "ns", "ns", "ns", "ns"))  # Corrected the missing comma

# Adjust y.position manually to prevent overlap
stat.test <- stat.test %>%
  group_by(condition, pattern, Groups, Length) %>%
  mutate(y.position = max(vis1$M, na.rm = TRUE) + 0.3 * row_number())  # Adjust spacing


# Inspect stat.test to check y.position
print(head(stat.test))
```

    ## # A tibble: 6 × 18
    ## # Groups:   condition, pattern, Groups, Length [6]
    ##   condition pattern Length Groups      .y.   group1 group2    n1    n2 statistic
    ##   <chr>     <chr>    <dbl> <chr>       <chr> <chr>  <chr>  <int> <int>     <dbl>
    ## 1 Base      Jhaptal     10 A. Familia… simi… 1. Re… 2. Al…    14    14     1.50 
    ## 2 Base      Jhaptal     10 B. Unfamil… simi… 1. Re… 2. Al…    15    15     6.12 
    ## 3 Base      Jhaptal     10 C. Unfamil… simi… 1. Re… 2. Al…    15    15     0.783
    ## 4 Base      Keherva      8 A. Familia… simi… 1. Re… 2. Al…    23    23     3.50 
    ## 5 Base      Keherva      8 B. Unfamil… simi… 1. Re… 2. Al…    16    16     5.94 
    ## 6 Base      Keherva      8 C. Unfamil… simi… 1. Re… 2. Al…    16    16     3.01 
    ## # ℹ 8 more variables: df <dbl>, p <dbl>, p.signif <chr>, y.position <dbl>,
    ## #   groups <named list>, x <dbl>, xmin <dbl>, xmax <dbl>

``` r
# Create a plot with error bars (mean ± SE)
g_plot1 <- ggplot(vis1, aes(x = factor(Length), y = M, color = type)) +
  geom_point(position = pd) +
  geom_errorbar(aes(ymin = M - SE, ymax = M + SE), width = 0.2, position = pd) +
  facet_wrap(~condition+Groups, scales="free") +  
  scale_y_continuous(limits = c(1, 8), breaks = seq(1, 7, by = 2)) +  # Adjusted upper limit
  scale_color_brewer(name = "Stimuli-Types", palette = "Set1") +
  ylab("Similarity Rating (M ± SE)") +  
  xlab("No. of Beats")
  #theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5))

# Add p-values to the plot
stat.test <- stat.test %>% ungroup()
print(stat.test)
```

    ## # A tibble: 24 × 18
    ##    condition pattern Length Groups     .y.   group1 group2    n1    n2 statistic
    ##    <chr>     <chr>    <dbl> <chr>      <chr> <chr>  <chr>  <int> <int>     <dbl>
    ##  1 Base      Jhaptal     10 A. Famili… simi… 1. Re… 2. Al…    14    14     1.50 
    ##  2 Base      Jhaptal     10 B. Unfami… simi… 1. Re… 2. Al…    15    15     6.12 
    ##  3 Base      Jhaptal     10 C. Unfami… simi… 1. Re… 2. Al…    15    15     0.783
    ##  4 Base      Keherva      8 A. Famili… simi… 1. Re… 2. Al…    23    23     3.50 
    ##  5 Base      Keherva      8 B. Unfami… simi… 1. Re… 2. Al…    16    16     5.94 
    ##  6 Base      Keherva      8 C. Unfami… simi… 1. Re… 2. Al…    16    16     3.01 
    ##  7 Base      Rupak        7 A. Famili… simi… 1. Re… 2. Al…    21    21     0.991
    ##  8 Base      Rupak        7 B. Unfami… simi… 1. Re… 2. Al…    20    20     1.39 
    ##  9 Base      Rupak        7 C. Unfami… simi… 1. Re… 2. Al…    13    13     1.18 
    ## 10 Base      Teental     16 A. Famili… simi… 1. Re… 2. Al…    19    19     2.49 
    ## # ℹ 14 more rows
    ## # ℹ 8 more variables: df <dbl>, p <dbl>, p.signif <chr>, y.position <dbl>,
    ## #   groups <named list>, x <dbl>, xmin <dbl>, xmax <dbl>

``` r
stat.test <- stat.test %>%
  distinct(condition, pattern, Groups, Length, .keep_all = TRUE)
g_plot1 <- g_plot1 + 
  stat_pvalue_manual(stat.test, label = "p.signif", size = 4)


# Save the plot as a PNG file
#file_path <- "figuref.png"
#ggsave(filename = file_path, plot = g_plot1, device = "png", dpi = 300, height = 7, width = 11)
#print(paste("Plot saved as", file_path, "with 300 DPI"))

# Print the final plot
print(g_plot1)
```

![](Analysis-and-Visualisation_files/figure-gfm/Figure%20Final-1.png)<!-- -->
