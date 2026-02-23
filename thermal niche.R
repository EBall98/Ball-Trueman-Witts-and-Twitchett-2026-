library(tidyverse) #data namipulation and plotting
library(patchwork)
library(gridExtra)
#library(SIBER)
library(ggsci)
library(ggplot2)

data<-read.csv("UK_Otoliths_CT.csv", header = TRUE, fill=TRUE)

zscore <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

data$d13Coto_z <- zscore(data$d13Coto)
data$d18Ooto_z <- zscore(data$d18Ooto)

#Calculate Temperature in degrees C -Morissette et al 2023
data<-
  data %>%
  mutate(Temp=((d18Ooto-d18Ow)-3.465)/-0.209)

#check species IDs by depth category

lower_slope_species <- data %>%
  filter(Depth == "Lower slope") %>%
  pull(Species) %>%
  unique()

print(lower_slope_species)

slope_species <- data %>%
  filter(Depth == "Slope") %>%
  pull(Species) %>%
  unique()

print(slope_species)

upper_slope_species <- data %>%
  filter(Depth == "Upper slope") %>%
  pull(Species) %>%
  unique()

print(upper_slope_species)

mesopelagic_species <- data %>%
  filter(Depth == "Mesopelagic") %>%
  pull(Species) %>%
  unique()

print(mesopelagic_species)


shelf_species <- data %>%
  filter(Depth == "Shelf") %>%
  pull(Species) %>%
  unique()

print(shelf_species)

Modern_data <- data %>%                       
  filter(Period == "Modern") 


# Welch ANOVA with pairwise Games -Howell
library(rstatix)
library(DescTools)

# Levene test
data %>% levene_test(d18Ooto ~ Period, center="median")


# Welch ANOVA (rstatix)
welch_result <- welch_anova_test(Modern_data, d18Ooto ~ Species)
print(welch_result)

# 2. Then Games-Howell post hoc
gh_res <- Modern_data %>%
  games_howell_test(d18Ooto ~ Species, detailed = TRUE)

# extract unique depth values
species_depth <- Modern_data %>%
  select(Species, Depth) %>%
  distinct()
# append to gh results
gh_res_depth <- gh_res %>%
  left_join(species_depth, by = c("group1" = "Species")) %>%
  rename(Depth1 = Depth) %>%
  left_join(species_depth, by = c("group2" = "Species")) %>%
  rename(Depth2 = Depth)

print(gh_res_depth, n=Inf)
# Filter only p < 0.05
non_sig_pairs <- gh_res_depth %>%
  filter(p.adj > 0.05)


non_sig_pairs2 <- non_sig_pairs %>%
  mutate(
    Depth1 = as.character(Depth1),
    Depth2 = as.character(Depth2)
  )



grouped <- non_sig_pairs2 %>% group_by(Depth1)

split_tables <- grouped %>% group_split(.keep = TRUE)

# Names from group_keys() (one name per group)
names(split_tables) <- grouped %>% group_keys() %>% pull(Depth1)

# Verify
names(split_tables)

same_depth_counts <- non_sig_pairs2 %>%
  group_by(Depth1) %>%
  summarise(
    n_same = sum(Depth1 == Depth2, na.rm = TRUE),
    total = n(),
    .groups = "drop"
  ) %>%
  mutate(prop_same = n_same / total)

print(same_depth_counts)



oto_guild <- ggplot(
  Modern_data,
  aes(
    x = d18Ooto,
    y = d13Coto,
    shape = MOL,
    fill  = Depth   # map fill at top-level aes
  )
) +
  geom_point(
    size = 4,
    color = "black",    # outline
    stroke = 0.6,
    alpha=0.8
  ) +
  scale_shape_manual(values = c(21, 22, 23, 24, 25)) +
  scale_fill_npg() +
  guides(
    shape = guide_legend(override.aes = list(fill = "grey80")), # keep outline legend
    fill  = guide_legend(override.aes = list(shape = 21, color = "black")) # show filled legend
  ) +
  theme_classic(base_size = 14) +
  theme(
    legend.position = c(1.1, 0.05),  # bottom-right inside the panel
    legend.justification = c("right", "bottom"), # anchor the legend box correctly
    legend.background = element_rect(fill = "white", color = "grey70"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey85"),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5)
  ) +
  labs(
    x = expression(delta^18 * "O"),
    y = expression(delta^13 * "C"),
    fill = "Site",
    shape = "MOL"
  )+
  coord_cartesian(clip = "off") 

oto_guild



### show the distributions of d13C values by MOL for a given d18O range
# Filter data within ±0.1 of 1 in d18Ooto
subset_data1 <- Modern_data %>%
  dplyr::filter(d18Ooto >= 0.9 & d18Ooto <= 1.1)

subset_data2 <- Modern_data %>%
  dplyr::filter(d18Ooto >= 1.9 & d18Ooto <= 2.1)

subset_data3 <- Modern_data %>%
  dplyr::filter(d18Ooto >= 2.9 & d18Ooto <= 3.1)




subset_plot1 <- ggplot(
  subset_data1,
  aes(
    x = MOL,
    y = d13Coto
  )
) +
  # one violin per MOL only
  geom_violin(
    fill = "grey85",   # neutral violin fill
    color = "black",
    alpha = 0.6,
    trim = FALSE
  ) +
  # Depth still shown by color, MOL still shown by shape
  geom_jitter(
    aes(fill = Depth, shape = MOL),
    position = position_jitter(width = 0.2),
    size = 3,
    color = "black",
    stroke = 0.6,
    alpha = 0.85
  ) +
  scale_shape_manual(values = c(21, 22, 23, 24, 25)) +
  scale_fill_npg() +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey85"),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5)
  ) +
  ylim(-10,1)+
  labs(
    x = "MOL",
    y = expression(delta^13 * "C"),
    fill = "Site",
    shape = "MOL"
  )

subset_plot1




subset_plot2 <- ggplot(
  subset_data2,
  aes(
    x = MOL,
    y = d13Coto
  )
) +
  # one violin per MOL only
  geom_violin(
    fill = "grey85",   # neutral violin fill
    color = "black",
    alpha = 0.6,
    trim = FALSE
  ) +
  # Depth still shown by color, MOL still shown by shape
  geom_jitter(
    aes(fill = Depth, shape = MOL),
    position = position_jitter(width = 0.2),
    size = 3,
    color = "black",
    stroke = 0.6,
    alpha = 0.85
  ) +
  scale_shape_manual(values = c(21, 22, 23, 24, 25)) +
  scale_fill_npg() +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey85"),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5)
  ) +
  ylim(-10,1)+
  
  labs(
    x = "MOL",
    y = expression(delta^13 * "C"),
    fill = "Site",
    shape = "MOL"
  )

subset_plot2


subset_plot3 <- ggplot(
  subset_data3,
  aes(
    x = MOL,
    y = d13Coto
  )
) +
  # one violin per MOL only
  geom_violin(
    fill = "grey85",   # neutral violin fill
    color = "black",
    alpha = 0.6,
    trim = FALSE
  ) +
  # Depth still shown by color, MOL still shown by shape
  geom_jitter(
    aes(fill = Depth, shape = MOL),
    position = position_jitter(width = 0.2),
    size = 3,
    color = "black",
    stroke = 0.6,
    alpha = 0.85
  ) +
  scale_shape_manual(values = c(21, 22, 23, 24, 25)) +
  scale_fill_npg() +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey85"),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5)
  ) +
  ylim(-10,1)+
  
  labs(
    x = "MOL",
    y = expression(delta^13 * "C"),
    fill = "Site",
    shape = "MOL"
  )

subset_plot3

library(patchwork)

# Remove legends and x labels for the top two plots
subset_plot1_clean <- subset_plot1 +
  theme(legend.position = "none") +
  labs(x = NULL)

subset_plot2_clean <- subset_plot2 +
  theme(legend.position = "none") +
  labs(x = NULL)

# Keep legend and x label only in bottom plot
subset_plot3_clean <- subset_plot3

# Stack them vertically
combined_plot <- subset_plot1_clean / subset_plot2_clean / subset_plot3_clean +
  plot_layout(guides = "collect") & theme(legend.position = "none")

combined_plot


# Combine
combined_plot_full <- oto_guild +
  (
    subset_plot1_clean / subset_plot2_clean / subset_plot3 +
      plot_layout(guides = "collect") &
      theme(legend.position = "none")
  ) +
  plot_layout(widths = c(2, 1))

combined_plot_full


# Explicit factor levels (make sure these match your dataset)
all_mol_levels <- c("Shelf", "Mesopelagic", "Upper slope", "Slope", "Lower slope")  # replace with real MOL categories
all_depth_levels <- unique(data$Depth)  # or hardcode if you want fixed order

# Ensure factors are consistent
Modern_data$MOL <- factor(Modern_data$MOL, levels = all_mol_levels)
Modern_data$Depth <- factor(Modern_data$Depth, levels = all_depth_levels)

man <- manova(cbind(d13Coto_z, d18Ooto_z) ~ Species, data = Modern_data)
summary(man, test = "Pillai")

summary.aov(man)

library(effectsize)
eta_squared(man, partial = TRUE)

eta_squared(aov(d13Coto ~ Species, data=data))
eta_squared(aov(d18Ooto ~ Species, data=data))


library(MASS)
lda_res <- lda(Species ~ d13Coto + d18Ooto, data=Modern_data)
lda_res$scaling

lda_res


lda_pred <- data.frame(predict(lda_res)$x, group = Modern_data$Species)


ggplot(lda_pred, aes(x = LD1, y = LD2, color = group)) +
  geom_point(size = 3, alpha = 0.7) +
  stat_ellipse(level = 0.68, linewidth = 1) +
  theme_minimal(base_size = 14) +
  labs(
    title = "LDA Group Separation",
    x = "Linear Discriminant 1",
    y = "Linear Discriminant 2"
  ) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold")
  )

## PERMANOVA comparison between communities
data <- data %>%
  mutate(shelf_slope = if_else(Depth == "Shelf", "shelf", "slope"))

library(vegan)




#nested permanova
dist_matrix <- dist(data[, c("d13Coto_z", "d18Ooto_z")])

adonis_res<-adonis2(dist_matrix ~ Species, data = data, permutations = 999)

adonis_df <- as.data.frame(adonis_res)
adonis_df

#large difference in the distribution of species within isotope space between the two communities

# Prepare for niche metrics outside of siber
siber_data <- data %>%
  rename(
    community = Period,   
    group = Species,           
    d13C = d13Coto_z,
    d18O = d18Ooto_z
  ) %>%
 dplyr:: select(community, group, d13C, d18O, MOL, shelf_slope, Depth)



compute_SEA <- function(data){
  cov_mat <- cov(data)           # covariance matrix of δ13C and δ15N
  n <- nrow(data)
  SEA <- pi * sqrt(det(cov_mat)) # standard ellipse area
  SEAc <- SEA * (n - 1) / (n - 2) # small-sample correction
  return(list(SEA = SEA, SEAc = SEAc))
}

# 1. Compute group-level niche metrics
group_metrics <- siber_data %>%
  group_by(community, group) %>%
  summarise(
    SEA = compute_SEA(cbind(d13C, d18O))$SEA,
    SEAc = compute_SEA(cbind(d13C, d18O))$SEAc,
    mean_d13C = mean(d13C),
    mean_d18O = mean(d18O),
    n = n(),
    .groups = "drop"
  )

# 2. Compute community-level summaries
community_metrics <- group_metrics %>%
  group_by(community) %>%
  summarise(
    total_SEA = sum(SEA),       # total niche space across groups
    mean_SEA = mean(SEA),       # mean niche space per group
    total_SEAc = sum(SEAc),
    mean_SEAc = mean(SEAc),
    n_groups = n(),
    .groups = "drop"
  )



library(sp)
library(ellipse)
library(combinat)
library(sf)


# Get ellipse points for a group
get_ellipse <- function(data, level = 0.95, npoints = 100){
  cov_mat <- cov(data)
  center <- colMeans(data)
  ellipse(cov_mat, centre = center, level = level, npoints = npoints)
}



# Function to convert ellipse points to sf polygon
ellipse_to_sf <- function(ell){
  # Ensure polygon is closed
  if(!all(ell[1,] == ell[nrow(ell),])){
    ell <- rbind(ell, ell[1,])
  }
  st_sfc(st_polygon(list(as.matrix(ell))))
}

# Compute % overlap between two ellipses
compute_overlap_sf <- function(ell1, ell2){
  poly1 <- st_sfc(ellipse_to_sf(ell1))
  poly2 <- st_sfc(ellipse_to_sf(ell2))
  inter <- st_intersection(poly1, poly2)
  if (length(inter) == 0) return(0)
  
  area_overlap <- st_area(inter)
  area1 <- st_area(poly1)
  area2 <- st_area(poly2)
  
  as.numeric(area_overlap / min(area1, area2))
}

# Prepare ellipse list per group
group_ellipses <- siber_data %>%
  group_by(community, group) %>%
  summarise(ellipse_pts = list(get_ellipse(cbind(d13C, d18O))),
            .groups = "drop")


# Compute pairwise overlaps per community
within_community_overlap <- group_ellipses %>%
  group_by(community) %>%
  summarise(
    overlaps = {
      grp_names <- group
      if(length(grp_names) < 2) return(0)  # single group => no pairwise overlap
      combos <- combn(seq_along(grp_names), 2)
      sapply(1:ncol(combos), function(i){
        g1 <- combos[1,i]
        g2 <- combos[2,i]
        ell1 <- ellipse_pts[[g1]]
        ell2 <- ellipse_pts[[g2]]
        compute_overlap_sf(ell1, ell2)
      })
    },
    .groups = "drop"
  )

# ----------------------------
# 6. Summarize per community
# ----------------------------
within_community_overlap <- within_community_overlap %>%
  dplyr::mutate(
    mean_overlap = sapply(overlaps, mean),
    median_overlap = sapply(overlaps, median),
    n_pairs = sapply(overlaps, length)
  )


within_community_overlap %>%
  dplyr::select(community, n_pairs, mean_overlap, median_overlap)

#___________________




# --- 1. Functions ---

get_ellipse <- function(data, level = 0.95, npoints = 100){
  cov_mat <- cov(data)
  center <- colMeans(data)
  ellipse(cov_mat, centre = center, level = level, npoints = npoints)
}

ellipse_to_sf <- function(ell){
  # close polygon
  if(!all(ell[1,] == ell[nrow(ell),])){
    ell <- rbind(ell, ell[1,])
  }
  st_sfc(st_polygon(list(as.matrix(ell))))
}

compute_overlap_sf <- function(ell1, ell2){
  poly1 <- ellipse_to_sf(ell1)
  poly2 <- ellipse_to_sf(ell2)
  
  inter <- st_intersection(poly1, poly2)
  
  if(length(inter) == 0 || st_is_empty(inter)) return(0)
  
  area_overlap <- st_area(inter)
  area1 <- st_area(poly1)
  area2 <- st_area(poly2)
  
  as.numeric(area_overlap / min(area1, area2))
}

# --- 2. Compute ellipses per group ---
group_ellipses <- siber_data %>%
  group_by(community, group) %>%
  summarise(ellipse_pts = list(get_ellipse(cbind(d13C, d18O))),
            .groups = "drop")

# --- 3. Compute pairwise overlaps with group IDs ---
pairwise_overlaps <- group_ellipses %>%
  group_by(community) %>%
  summarise(
    overlaps_df = list({
      g <- group
      ell <- ellipse_pts
      if(length(g) < 2) return(data.frame())  # skip single-group communities
      combos <- combn(seq_along(g), 2)
      data.frame(
        group1 = g[combos[1,]],
        group2 = g[combos[2,]],
        overlap = sapply(1:ncol(combos), function(i){
          compute_overlap_sf(ell[[combos[1,i]]], ell[[combos[2,i]]])
        })
      )
    }),
    .groups = "drop"
  )

# --- 4. Unnest to long table ---
pairwise_overlaps_long <- pairwise_overlaps %>%
  tidyr::unnest(overlaps_df)

# --- 5. Optionally, compute summary per species, community ---
community_summary_species <- pairwise_overlaps_long %>%
  group_by(group1) %>%
  summarise(
    mean_overlap = mean(overlap),
    median_overlap = median(overlap, na.rm=TRUE),
    n_pairs = n(),
    .groups = "drop"
  )


community_summary <- pairwise_overlaps_long %>%
  group_by(community) %>%
  summarise(
    mean_overlap = mean(overlap),
    median_overlap = median(overlap),
    n_pairs = n(),
    .groups = "drop"
  )


# --- 6. Outputs ---
pairwise_overlaps_long      # contains community, group1, group2, overlap
community_summary           # contains summary of within-community overlap



# --- 7. NEW: Count groups with NO overlapping ellipses ---
# A group has "no overlaps" if it never appears in any pair where overlap > 0

library(stringr)
# tolerance: treat overlap <= tol as NO overlap
tol <- 0.5

# ensure types and trim whitespace
pairwise_overlaps_long <- pairwise_overlaps_long %>%
  dplyr::mutate(
    community = as.character(community),
    group1 = as.character(group1),
    group2 = as.character(group2)
  )

all_groups <- group_ellipses %>%
  dplyr::mutate(community = as.character(community),
                group = as.character(group)) %>%
  dplyr::select(community, group) %>%
  dplyr::distinct() %>%
  dplyr::mutate(group = stringr::str_trim(group))

# 1) compute per-group max overlap (each pair contributes to both groups)
per_group_stats <- pairwise_overlaps_long %>%
  tidyr::pivot_longer(
    cols = c("group1", "group2"),
    names_to = "side", values_to = "group"
  ) %>%
  mutate(group = as.character(group)) %>%
  group_by(community, group) %>%
  summarise(
    max_overlap = ifelse(n() > 0, max(overlap, na.rm = TRUE), 0),
    min_overlap = ifelse(n() > 0, min(overlap, na.rm = TRUE), NA_real_),
    mean_overlap_for_group = ifelse(n() > 0, mean(overlap, na.rm = TRUE), NA_real_),
    n_pair_rows = n(),
    .groups = "drop"
  )

# 2) include groups that had no pairs (singletons) by left-joining with all_groups
per_group_all <- all_groups %>%
  left_join(per_group_stats, by = c("community", "group")) %>%
  mutate(
    max_overlap = ifelse(is.na(max_overlap), 0, max_overlap),
    min_overlap = ifelse(is.na(min_overlap), NA_real_, min_overlap),
    n_pair_rows = ifelse(is.na(n_pair_rows), 0L, n_pair_rows),
    mean_overlap_for_group = ifelse(is.na(mean_overlap_for_group), NA_real_, mean_overlap_for_group),
    non_overlapping = (max_overlap <= tol)  # TRUE if group's max overlap <= tol
  )

# 3) summary of non-overlapping groups per community
non_overlap_summary <- per_group_all %>%
  filter(non_overlapping) %>%
  arrange(community, group) %>%
  group_by(community) %>%
  summarise(
    n_non_overlapping_groups = n(),
    non_overlapping_groups = paste(group, collapse = ", "),
    .groups = "drop"
  )

# 4) community-level totals and percent non-overlapping
community_totals <- per_group_all %>%
  group_by(community) %>%
  summarise(
    n_groups = n(),
    n_non_overlapping = sum(non_overlapping),
    pct_non_overlapping = 100 * n_non_overlapping / n_groups,
    .groups = "drop"
  )

# 5) combine with earlier community_summary if desired
community_summary_final <- community_summary %>%
  dplyr::mutate(community = as.character(community)) %>%
  dplyr:: left_join(non_overlap_summary, by = "community") %>%
  dplyr::left_join(community_totals %>%  dplyr::select(community, n_groups, n_non_overlapping, pct_non_overlapping),
            by = "community") %>%
  dplyr::mutate(overlap_tolerance = tol)

# 6) Diagnostics to print
cat("=== Pairwise overlap distribution by community (relative to tol) ===\n")
print(
  pairwise_overlaps_long %>%
    group_by(community) %>%
    summarise(
      n_pairs = n(),
      mean_overlap = mean(overlap, na.rm = TRUE),
      median_overlap = median(overlap, na.rm = TRUE),
      min_overlap = min(overlap, na.rm = TRUE),
      max_overlap = max(overlap, na.rm = TRUE),
      prop_pairs_less_eq_tol = mean(overlap <= tol),
      .groups = "drop"
    ), n = Inf
)
cat("\n=== Per-group MIN overlap sample (lowest mins first) ===\n")
print(per_group_all %>% arrange(min_overlap) %>% head(90), n = 90)


cat("\n=== Per-group overlap summary by community (with MOL & shelf_slope preserved) ===\n")


siber_group_summary <- siber_data %>%
  dplyr::group_by(community, group) %>%
  dplyr::summarise(
    MOL = dplyr::first(MOL),
    shelf_slope = dplyr::first(shelf_slope),
    Depth = dplyr::first(Depth),
    .groups = "drop"
  )


per_group_summary <- per_group_all %>%
  dplyr::left_join(siber_group_summary, by = c("community", "group"))

print(per_group_summary, n=Inf)
##### PLOT output figs mean overlap and max overlap for modern MOL and fossil 


Niche_fossil<-per_group_summary %>%
  dplyr::filter(community=="Fossil")%>%
  dplyr::mutate(mean_overlap_for_group=mean_overlap_for_group*100,
                max_overlap=max_overlap*100)
  


Niche_modern_MOL<-per_group_summary %>%
  dplyr::filter(community=="Modern")%>%
  dplyr::group_by(MOL) %>%
  dplyr::mutate(mean_overlapMOL = mean(mean_overlap_for_group, na.rm=TRUE))

Niche_modern_Depth<-per_group_summary %>%
  dplyr::filter(community=="Modern")%>%
  dplyr::mutate(mean_overlap_for_group=mean_overlap_for_group*100)
#  dplyr::group_by(Depth) %>%
#  dplyr::mutate(mean_overlapDepth = mean(mean_overlap_for_group, na.rm=TRUE))


median(Niche_modern_Depth$mean_overlap_for_group, na.rm=TRUE)
median(Niche_fossil$mean_overlap_for_group, na.rm=TRUE)


  Niche_modern_Depth$Depth <- factor(
    Niche_modern_Depth$Depth,
  levels = c("Lower slope", "Slope", "Upper slope", "Mesopelagic", "Shelf")
)


############################### FIGURE 5 ###################################
NichePlot_Depth <- ggplot(Niche_modern_Depth,
                          aes(x = Depth, y = mean_overlap_for_group, group = Depth)
) +
  
  
  # Proper boxplot with median + IQR
  geom_boxplot(
    fill = "grey85",     # neutral box color
    color = "black",
    alpha = 0.6,
    width = 0.6,
    outlier.shape = NA   # hide outlier dots (optional)
  ) +
  
  # Add jittered points, colored by Mode of Life
  geom_jitter(
    aes(fill = MOL),
    shape = 21,
    position = position_jitter(width = 0.2),
    size = 4,
    color = "black",
    stroke = 0.6,
    alpha = 0.9
  ) +
  # Palette and theme
  scale_fill_npg() +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey85"),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5)
  ) +
  ylim(0, 50) +
  labs(
    x = "Depth Habitat",
    y = "Group Mean Niche Overlap (%)",
    fill = "Mode of Life"
  )

NichePlot_Depth




############################## FIGURE 7 ###################################
#fossil plot by species

NichePlot_Fossil <- ggplot(
  Niche_fossil,
  aes(x = group)
) +
  
  # Full range bar
  geom_col(
    aes(y = max_overlap, fill = group),
    width = 0.7,
    colour = "black"
  ) +
  
  # Mean as horizontal line
  geom_errorbar(
    aes(ymin = mean_overlap_for_group,
        ymax = mean_overlap_for_group),
    width = 0.6,
    linewidth = 1.2,
    colour = "black"
  ) +
  
  scale_fill_npg() +
  scale_x_discrete(labels = function(x) gsub("_", " ", x)) +
  
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1, face = "italic"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey85")
  ) +
  
  ylim(0, 100) +
  labs(
    x = "Species",
    y = "Niche overlap (%)"
  )

NichePlot_Fossil


###########
cat("\n=== Per-group min overlap sample (lowest maxs first) ===\n")

per_group_summary %>%
  arrange(community, mean_overlap_for_group) %>%
  group_by(community) %>%
  group_walk(~{
    cat("\n--- Community:", unique(.x$community), "---\n")
    print(
      .x %>%
        dplyr::select(
          group,
          MOL, shelf_slope, Depth,
          min_overlap, max_overlap,
          mean_overlap_for_group, n_pair_rows, non_overlapping
        ) %>%
        arrange(mean_overlap_for_group),
      n = Inf
    )
  })



cat("\n=== Non-overlap summary (groups with max_overlap <= tol) ===\n")
print(non_overlap_summary, n = Inf)

cat("\n=== Community summary final ===\n")
print(community_summary_final, n = Inf)




#### resample with equal numbers of species samples

library(dplyr)
library(tidyr)
library(stringr)

# Parameters
tol <- 0.5          # tolerance for "no overlap"
n_resamples <- 500  # number of permutation iterations
n_sample <- 10      # number of species (group) to sample from Modern
n_individuals <-10  # numbers of inidividuals per species to sample from modern

# Ensure group_ellipses and pairwise_overlaps_long exist
# Namespacing dplyr/tidyr to avoid masking conflicts
all_groups <- group_ellipses %>%
  dplyr::mutate(
    community = as.character(community),
    group = as.character(group)
  ) %>%
  dplyr::select(community, group) %>%
  dplyr::distinct() %>%
  dplyr::mutate(group = stringr::str_trim(group))

# Function to compute non-overlapping groups for a given subset of data
compute_non_overlap <- function(sub_groups, pairwise_overlaps, tol) {
  
  # Filter pairwise overlaps to only these groups
  pair_subset <- pairwise_overlaps %>%
    dplyr::filter(community %in% unique(sub_groups$community),
                  group1 %in% sub_groups$group,
                  group2 %in% sub_groups$group)
  
  # Per-group maximum overlap
  per_group_max <- pair_subset %>%
    tidyr::pivot_longer(cols = c("group1", "group2"), names_to = "side", values_to = "group") %>%
    dplyr::group_by(community, group) %>%
    dplyr::summarise(
      max_overlap = ifelse(n() > 0, max(overlap, na.rm = TRUE), 0),
      .groups = "drop"
    )
  
  # Merge with all groups to include singletons
  per_group_all <- sub_groups %>%
    dplyr::left_join(per_group_max, by = c("community", "group")) %>%
    dplyr::mutate(
      max_overlap = ifelse(is.na(max_overlap), 0, max_overlap),
      non_overlapping = (max_overlap <= tol)
    )
  
  # Count non-overlapping groups per community
  summary_df <- per_group_all %>%
    dplyr::distinct(community, group, non_overlapping) %>%
    dplyr::group_by(community) %>%
    dplyr::summarise(
      n_non_overlapping_groups = sum(non_overlapping),
      .groups = "drop"
    )
  
  return(summary_df)
}

# ---------- Permutation/resampling ----------
set.seed(123)  # for reproducibility
resample_results <- lapply(1:n_resamples, function(i) {
  
  # Step 1: Sample n_sample species from Modern
  modern_species <- all_groups %>%
    dplyr::filter(community == "Modern") %>%
    dplyr::pull(group) %>%
    unique() %>%
    sample(n_sample)
  
  # Step 2: For each sampled species, sample 10 individuals
  modern_groups <- all_groups %>%
    dplyr::filter(community == "Modern", group %in% modern_species) %>%
    dplyr::group_by(group) %>%
    dplyr::slice_sample(n=n_individuals, replace = TRUE) %>%  # replace = TRUE if some species < 10 individuals
    dplyr::ungroup()
  
  # Keep all Fossil groups
  fossil_groups <- all_groups %>%
    dplyr::filter(community == "Fossil")
  
  # Combine
  sub_groups <- dplyr::bind_rows(modern_groups, fossil_groups)
  
  # Compute non-overlapping counts
  compute_non_overlap(sub_groups, pairwise_overlaps_long, tol) %>%
    dplyr::mutate(iteration = i)
})

# Combine all iterations
resample_results_df <- dplyr::bind_rows(resample_results)

# Summary statistics across 500 iterations
resample_summary <- resample_results_df %>%
  dplyr::group_by(community) %>%
  dplyr::summarise(
    mean_non_overlapping = mean(n_non_overlapping_groups),
    sd_non_overlapping = sd(n_non_overlapping_groups),
    min_non_overlapping = min(n_non_overlapping_groups),
    max_non_overlapping = max(n_non_overlapping_groups),
    .groups = "drop"
  )

# View summary
print(resample_summary, n = Inf)

#25% non overlapping = fossil -0 Modern  - 2.5%
#50% non overlapping = fossil -2% Modern - 6%


