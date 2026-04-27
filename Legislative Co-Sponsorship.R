# ================================================================
# Part 1: W-NOMINATE Estimation & Ideological Gap Calculation
# ================================================================
# This script:
#   1. Reads roll-call voting data for 4 sub-terms (S201, S202, S211, S212)
#   2. Runs W-NOMINATE to estimate each legislator's ideal point
#   3. Calculates pairwise ideological gaps
#   4. Outputs: score CSVs + combined gap CSV
# ================================================================

rm(list = ls())
getwd()
setwd("/Users/pero1507/Desktop/BERLIN")

# ---------------------------
# 0. Load packages
# ---------------------------
library(readxl)
library(dplyr)
library(pscl)
library(wnominate)

# ---------------------------
# 1. Configuration
# ---------------------------
DATA_FILE    <- "test_KOREA.xlsx"
POLARITY_LEG <- "PPL_182"  # Reference legislator (Woo Won Shik) for polarity fix

# Sheet names for each year of roll-call data
SHEET_NAMES <- list(
  S20Y1 = "nominateS20Y1", S20Y2 = "nominateS20Y2",
  S20Y3 = "nominateS20Y3", S20Y4 = "nominateS20Y4",
  S21Y1 = "nominateS21Y1", S21Y2 = "nominateS21Y2",
  S21Y3 = "nominateS21Y3", S21Y4 = "nominateS21Y4"
)

# Sub-term definitions: each sub-term combines two consecutive years
SUBTERMS <- list(
  S201 = c("S20Y1", "S20Y2"),  # 20th Assembly, first half
  S202 = c("S20Y3", "S20Y4"),  # 20th Assembly, second half
  S211 = c("S21Y1", "S21Y2"),  # 21st Assembly, first half
  S212 = c("S21Y3", "S21Y4")   # 21st Assembly, second half
)

# ---------------------------
# 2. Helper functions
# ---------------------------

#' Combine two years of roll-call data into one sub-term dataset
#' Prefixes vote columns with Y1_ and Y2_ to avoid name collisions
combine_years <- function(data1, data2) {
  # Identify vote columns (everything except ID and name)
  id_cols <- c("legislator_id", "legislator_name")
  vote_cols1 <- setdiff(colnames(data1), id_cols)
  vote_cols2 <- setdiff(colnames(data2), id_cols)
  
  # Prefix vote columns
  colnames(data1)[colnames(data1) %in% vote_cols1] <- paste0("Y1_", vote_cols1)
  colnames(data2)[colnames(data2) %in% vote_cols2] <- paste0("Y2_", vote_cols2)
  
  # Full join on legislator_id (handles legislators present in only one year)
  combined <- full_join(data1, data2, by = "legislator_id", suffix = c(".y1", ".y2"))
  combined <- combined %>%
    mutate(legislator_name = coalesce(legislator_name.y1, legislator_name.y2)) %>%
    select(legislator_id, legislator_name, starts_with("Y1_"), starts_with("Y2_"))
  
  return(combined)
}

#' Run W-NOMINATE on a sub-term dataset and return scores + pairwise gaps
run_wnominate <- function(session_data, session_id) {
  cat("\n=== Processing", session_id, "===\n")
  
  # --- Prepare vote matrix ---
  vote_cols <- grep("^Y1_|^Y2_", colnames(session_data), value = TRUE)
  vote_matrix <- as.data.frame(lapply(session_data[, vote_cols], as.numeric))
  
  # Recode: 1 = Yea, 6 -> -1 (Nay), 0/9 -> NA (missing/abstain)
  vote_matrix[vote_matrix == 6]  <- -1
  vote_matrix[vote_matrix == 0]  <- NA
  vote_matrix[vote_matrix == 9]  <- NA
  
  cat("  Legislators:", nrow(vote_matrix), " | Votes:", ncol(vote_matrix), "\n")
  
  # --- Create rollcall object ---
  rc <- rollcall(
    data        = vote_matrix,
    legis.names = session_data$legislator_name,
    yea = 1, nay = -1, missing = NA
  )
  
  # --- Set polarity ---
  polarity_idx <- which(session_data$legislator_id == POLARITY_LEG)
  if (length(polarity_idx) == 0) {
    stop("Polarity legislator ", POLARITY_LEG, " not found in ", session_id)
  }
  
  # --- Run W-NOMINATE (1 dimension) ---
  result <- wnominate(rc, dims = 1, polarity = polarity_idx)
  scores <- result$legislators$coord1D
  
  # Flip sign so that conservative = positive (consistent with paper convention)
  scores <- -scores
  
  # --- Build scores dataframe ---
  score_df <- data.frame(
    legislator_id   = session_data$legislator_id,
    legislator_name = session_data$legislator_name,
    session_id      = session_id,
    score           = scores,
    stringsAsFactors = FALSE
  )
  
  cat("  Score range: [", round(min(scores, na.rm = TRUE), 3), ",",
      round(max(scores, na.rm = TRUE), 3), "]\n")
  cat("  NAs:", sum(is.na(scores)), "\n")
  
  # --- Calculate pairwise ideological gaps ---
  valid <- score_df %>% filter(!is.na(score))
  n <- nrow(valid)
  
  # Use combn for efficiency instead of nested loops
  pairs_idx <- combn(seq_len(n), 2)
  gap_df <- data.frame(
    from       = valid$legislator_id[pairs_idx[1, ]],
    to         = valid$legislator_id[pairs_idx[2, ]],
    gap        = abs(valid$score[pairs_idx[1, ]] - valid$score[pairs_idx[2, ]]),
    session_id = session_id,
    stringsAsFactors = FALSE
  )
  
  cat("  Dyadic pairs:", nrow(gap_df), "\n")
  
  return(list(scores = score_df, gaps = gap_df, wnominate_result = result))
}

# ---------------------------
# 3. Read data
# ---------------------------
cat("Reading roll-call data...\n")
raw_data <- lapply(SHEET_NAMES, function(sheet) {
  read_excel(DATA_FILE, sheet = sheet)
})

# ---------------------------
# 4. Combine years into sub-terms
# ---------------------------
cat("Combining years into sub-terms...\n")
subterm_data <- lapply(SUBTERMS, function(years) {
  combine_years(raw_data[[years[1]]], raw_data[[years[2]]])
})

# ---------------------------
# 5. Run W-NOMINATE for each sub-term
# ---------------------------
cat("\nRunning W-NOMINATE...\n")
results <- mapply(
  run_wnominate,
  session_data = subterm_data,
  session_id   = names(SUBTERMS),
  SIMPLIFY = FALSE
)

# ---------------------------
# 6. Collect and save outputs
# ---------------------------

# 6a. Individual score files (per sub-term)
for (sid in names(results)) {
  outfile <- paste0("w_nominate_scores_", sid, ".csv")
  write.csv(results[[sid]]$scores, outfile, row.names = FALSE)
  cat("Saved:", outfile, "\n")
}

# 6b. Combined scores (all sub-terms)
all_scores <- bind_rows(lapply(results, function(x) x$scores))
write.csv(all_scores, "w_nominate_scores_all.csv", row.names = FALSE)
cat("Saved: w_nominate_scores_all.csv\n")

# 6c. Combined ideological gaps (all sub-terms)
all_gaps <- bind_rows(lapply(results, function(x) x$gaps))
write.csv(all_gaps, "w_nominate_ideology_gaps.csv", row.names = FALSE)
cat("Saved: w_nominate_ideology_gaps.csv\n")

# ---------------------------
# 7. Summary statistics
# ---------------------------
cat("\n=== Summary ===\n")
all_scores %>%
  filter(!is.na(score)) %>%
  group_by(session_id) %>%
  summarise(
    n          = n(),
    mean_score = round(mean(score), 3),
    sd_score   = round(sd(score), 3),
    min_score  = round(min(score), 3),
    max_score  = round(max(score), 3),
    .groups    = "drop"
  ) %>%
  print()

cat("\n=== Gap Summary ===\n")
all_gaps %>%
  group_by(session_id) %>%
  summarise(
    n_pairs  = n(),
    mean_gap = round(mean(gap), 3),
    sd_gap   = round(sd(gap), 3),
    .groups  = "drop"
  ) %>%
  print()

cat("\nPart 1 complete.\n")





# ================================================================
# Part 2: Co-Sponsorship Network Distance Calculation
# ================================================================
# This script:
#   1. Reads bill co-sponsorship data
#   2. Constructs weighted directed networks for each sub-term
#   3. Calculates shortest-path network distance for all dyads
#   4. Outputs: weight matrices, distance matrices, combined relations CSV
#
# Weight formula (from paper):
#   - Co-sponsor -> Main sponsor:  alpha * (9 / n_cosponsors)
#   - Co-sponsor <-> Co-sponsor:   beta  * (9 / n_cosponsors)
#   where 9 = mandatory minimum co-sponsors in Korean National Assembly
# ================================================================

rm(list = ls())

# ---------------------------
# 0. Load packages
# ---------------------------
library(readxl)
library(dplyr)
library(igraph)

# ---------------------------
# 1. Configuration
# ---------------------------
DATA_FILE <- "test_KOREA.xlsx"
SHEET_SPONSORSHIP <- "bill_sponsorship"

ALPHA   <- 1      # Weight for co-sponsor -> main sponsor edges
BETA    <- 0      # Main model: no co-sponsor <-> co-sponsor edges
# Set to 0.5 for robustness check (see Supplementary Information)
EPSILON <- 1e-6   # Small constant for disconnected pairs (distance = 1/epsilon)

TARGET_SESSIONS <- c("S201", "S202", "S211", "S212")

# ---------------------------
# 2. Read and clean data
# ---------------------------
cat("Reading co-sponsorship data...\n")
sponsorship_raw <- read_xlsx(DATA_FILE, sheet = SHEET_SPONSORSHIP) %>%
  select(bill_id, main_sponsor_id, co_sponsor_id, co_sponsor_no, session_id) %>%
  filter(
    !is.na(main_sponsor_id), !is.na(co_sponsor_id),
    main_sponsor_id != "NA", co_sponsor_id != "NA",
    session_id %in% TARGET_SESSIONS
  )

cat("Total sponsorship rows:", nrow(sponsorship_raw), "\n")
cat("Sessions:", paste(unique(sponsorship_raw$session_id), collapse = ", "), "\n")

# ---------------------------
# 3. Helper functions
# ---------------------------

#' Build weight matrix for one sub-term
#' Returns a named matrix: weight_matrix[i, j] = total weight from i to j
build_weight_matrix <- function(session_data, legislators) {
  n <- length(legislators)
  W <- matrix(0, n, n, dimnames = list(legislators, legislators))
  
  bills <- unique(session_data$bill_id)
  
  for (bill in bills) {
    bill_data <- session_data %>% filter(bill_id == bill)
    
    main_sponsors <- unique(bill_data$main_sponsor_id)
    co_sponsors   <- unique(bill_data$co_sponsor_id)
    n_cosponsors  <- length(co_sponsors)
    
    weight_direct   <- ALPHA * (9 / n_cosponsors)
    weight_indirect <- BETA  * (9 / n_cosponsors)
    
    # Part A: Directed edges from co-sponsors to main sponsors
    for (cs in co_sponsors) {
      for (ms in main_sponsors) {
        if (cs != ms) {
          W[cs, ms] <- W[cs, ms] + weight_direct
        }
      }
    }
    
    # Part B: Undirected edges between co-sponsors (exclude main sponsors)
    non_main <- setdiff(co_sponsors, main_sponsors)
    if (length(non_main) > 1) {
      pairs <- combn(non_main, 2)
      for (k in seq_len(ncol(pairs))) {
        i <- pairs[1, k]
        j <- pairs[2, k]
        W[i, j] <- W[i, j] + weight_indirect
        W[j, i] <- W[j, i] + weight_indirect
      }
    }
  }
  
  return(W)
}

#' Calculate shortest-path distance matrix from weight matrix
#' Uses Johnson's algorithm on a directed graph
calc_distance_matrix <- function(W, legislators) {
  n <- length(legislators)
  
  # Convert weights to edge distances (reciprocal)
  # No edge (weight = 0) gets distance = 1/EPSILON (very large)
  edge_dist <- matrix(1 / EPSILON, n, n, dimnames = list(legislators, legislators))
  has_edge <- W > 0
  edge_dist[has_edge] <- 1 / W[has_edge]
  diag(edge_dist) <- 0
  
  # Build directed graph and compute all-pairs shortest paths
  g <- graph_from_adjacency_matrix(edge_dist, mode = "directed", weighted = TRUE)
  D <- distances(g, v = V(g), to = V(g), mode = "out", algorithm = "johnson")
  
  rownames(D) <- legislators
  colnames(D) <- legislators
  
  return(D)
}

#' Process one sub-term: build network, compute distances, return long-format relations
process_session_network <- function(session_data, session_id) {
  cat("\n=== Processing", session_id, "===\n")
  
  # Identify all legislators in this sub-term
  legislators <- sort(unique(c(session_data$main_sponsor_id, session_data$co_sponsor_id)))
  n_bills <- length(unique(session_data$bill_id))
  cat("  Legislators:", length(legislators), "| Bills:", n_bills, "\n")
  
  # Build weight matrix
  W <- build_weight_matrix(session_data, legislators)
  n_edges <- sum(W > 0)
  cat("  Non-zero edges:", n_edges, "\n")
  
  # Calculate distance matrix
  D <- calc_distance_matrix(W, legislators)
  n_inf <- sum(is.infinite(D)) - length(legislators)  # exclude diagonal
  cat("  Infinite distances (disconnected):", n_inf, "\n")
  
  # Save matrices
  write.csv(W, paste0("network_weight_", session_id, ".csv"))
  
  D_export <- D
  D_export[is.infinite(D_export)] <- 999999
  write.csv(D_export, paste0("network_distance_", session_id, ".csv"))
  
  # Convert to long format (all directed pairs, excluding self)
  relations <- expand.grid(from = legislators, to = legislators, stringsAsFactors = FALSE) %>%
    filter(from != to) %>%
    mutate(
      session_id = session_id,
      weight     = mapply(function(f, t) W[f, t], from, to),
      distance   = mapply(function(f, t) D[f, t], from, to)
    )
  
  # Replace Inf with 999999 for export
  relations$distance[is.infinite(relations$distance)] <- 999999
  
  cat("  Total directed pairs:", nrow(relations), "\n")
  
  return(relations)
}

# ---------------------------
# 4. Process each sub-term
# ---------------------------
all_relations <- list()

for (sid in TARGET_SESSIONS) {
  session_data <- sponsorship_raw %>% filter(session_id == sid)
  all_relations[[sid]] <- process_session_network(session_data, sid)
}

# ---------------------------
# 5. Combine and save
# ---------------------------
all_relations_df <- bind_rows(all_relations)
write.csv(all_relations_df, "all_network_relations.csv", row.names = FALSE)
cat("\nSaved: all_network_relations.csv\n")

# ---------------------------
# 6. Summary statistics
# ---------------------------
cat("\n=== Summary ===\n")
all_relations_df %>%
  mutate(connected = distance < 999999) %>%
  group_by(session_id) %>%
  summarise(
    n_pairs       = n(),
    n_connected   = sum(connected),
    pct_connected = round(mean(connected) * 100, 1),
    mean_dist     = round(mean(distance[connected], na.rm = TRUE), 3),
    mean_weight   = round(mean(weight[weight > 0], na.rm = TRUE), 3),
    .groups       = "drop"
  ) %>%
  print()

cat("\nPart 2 complete.\n")














# ================================================================
# Part 3: Regression Analysis — Ideological Gap, Network Distance,
#         and Electoral Proximity
# ================================================================
# This script:
#   1. Merges network distance, ideological gap, legislator attributes
#   2. Constructs camp/party classification variables
#   3. Runs the main regression models (H1a, H1b):
#        Distance ~ Gap + controls + session FE
#   4. Runs the interaction model (H2):
#        Distance ~ Gap * SecondHalf + controls + term FE
#        (uses term FE instead of session FE to avoid collinearity)
#   5. Split-sample models + coefficient plot (Figure 4)
#   6. All models use dyad-clustered standard errors
#
# Prerequisites: Run 01_wnominate.R and 02_network_distance.R first
# ================================================================

rm(list = ls())

# ---------------------------
# 0. Load packages
# ---------------------------
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stargazer)
library(sandwich)    # for robust/clustered SEs
library(lmtest)      # for coeftest()

# ---------------------------
# 1. Configuration
# ---------------------------
DATA_FILE <- "test_KOREA.xlsx"

# Output files from Part 1 and Part 2
GAPS_FILE      <- "w_nominate_ideology_gaps.csv"
RELATIONS_FILE <- "all_network_relations.csv"

TARGET_SESSIONS <- c("S201", "S202", "S211", "S212")

# ---------------------------
# 2. Read data
# ---------------------------
cat("Reading data...\n")

# 2a. Network relations (from Part 2)
network <- read.csv(RELATIONS_FILE, stringsAsFactors = FALSE) %>%
  filter(session_id %in% TARGET_SESSIONS)

# 2b. Ideological gaps (from Part 1)
gaps <- read.csv(GAPS_FILE, stringsAsFactors = FALSE) %>%
  filter(session_id %in% TARGET_SESSIONS)

# 2c. Legislator attributes
legislator_attr <- read_excel(DATA_FILE, sheet = "legislator_attributes")

cat("  Network relations:", nrow(network), "\n")
cat("  Ideology gaps:", nrow(gaps), "\n")
cat("  Legislators:", nrow(legislator_attr), "\n")

# ---------------------------
# 3. Add camp/party information
# ---------------------------
camp_lookup <- legislator_attr %>%
  select(legislator_id, session_id, camp_id, party_id) %>%
  distinct()

# Add camp/party info to ideology gaps
gaps <- gaps %>%
  left_join(camp_lookup, by = c("from" = "legislator_id", "session_id")) %>%
  rename(from_camp_id = camp_id, from_party_id = party_id) %>%
  left_join(camp_lookup, by = c("to" = "legislator_id", "session_id")) %>%
  rename(to_camp_id = camp_id, to_party_id = party_id)

# Add camp/party info to network relations
network <- network %>%
  left_join(camp_lookup, by = c("from" = "legislator_id", "session_id")) %>%
  rename(from_camp_id = camp_id, from_party_id = party_id) %>%
  left_join(camp_lookup, by = c("to" = "legislator_id", "session_id")) %>%
  rename(to_camp_id = camp_id, to_party_id = party_id)

# ---------------------------
# 4. Merge network distance with ideological gap
# ---------------------------
# Network is directed (from -> to); gaps are undirected (stored as from < to).
# Match both directions.
merged <- network %>%
  left_join(gaps %>% select(from, to, session_id, gap),
            by = c("from", "to", "session_id")) %>%
  left_join(gaps %>% select(from, to, session_id, gap),
            by = c("from" = "to", "to" = "from", "session_id" = "session_id"),
            suffix = c("", "_rev")) %>%
  mutate(gap = coalesce(gap, gap_rev)) %>%
  select(-gap_rev)

cat("Merged rows:", nrow(merged), "\n")
cat("Gap NA after merge:", sum(is.na(merged$gap)), "\n")

# ---------------------------
# 5. Add legislator control variables
# ---------------------------
leg_controls <- legislator_attr %>%
  select(legislator_id, session_id,
         legislator_female, birth_year, electoral_type,
         terms_served, region_id) %>%
  distinct()

merged <- merged %>%
  left_join(leg_controls, by = c("from" = "legislator_id", "session_id")) %>%
  rename(from_female = legislator_female, from_birth_year = birth_year,
         from_electoral_type = electoral_type, from_terms_served = terms_served,
         from_region_id = region_id) %>%
  left_join(leg_controls, by = c("to" = "legislator_id", "session_id")) %>%
  rename(to_female = legislator_female, to_birth_year = birth_year,
         to_electoral_type = electoral_type, to_terms_served = terms_served,
         to_region_id = region_id)

# ---------------------------
# 6. Construct analytical variables
# ---------------------------
merged <- merged %>%
  mutate(
    # Camp type: opposing vs same (excludes independents = camp 0)
    camp_type = case_when(
      from_camp_id %in% c(1, 2) & to_camp_id %in% c(1, 2) &
        from_camp_id != to_camp_id ~ "opposing_camp",
      from_camp_id %in% c(1, 2) & to_camp_id %in% c(1, 2) &
        from_camp_id == to_camp_id ~ "same_camp",
      TRUE ~ "other"
    ),
    
    # Party type (for robustness / supplementary analysis)
    party_type = case_when(
      from_party_id == to_party_id ~ "same_party",
      camp_type == "opposing_camp" ~ "opposing_party",
      camp_type == "same_camp" & from_party_id != to_party_id ~ "same_camp_diff_party",
      TRUE ~ "other"
    ),
    
    # Same region dummy
    same_region = as.integer(
      !is.na(from_region_id) & !is.na(to_region_id) &
        from_region_id == to_region_id
    ),
    
    # Second half dummy: S202, S212 = 1; S201, S211 = 0
    second_half = as.integer(session_id %in% c("S202", "S212")),
    
    # Term dummy: 20th = 0, 21st = 1
    term21 = as.integer(session_id %in% c("S211", "S212")),
    
    # Dyad cluster ID for clustered SEs
    # Sorted pair + session ensures same cluster regardless of direction
    dyad_id = paste(pmin(from, to), pmax(from, to), session_id, sep = "_")
  )

# ---------------------------
# 7. Filter to analysis sample
# ---------------------------
cat("\n=== Pre-filter ===\n")
cat("Total rows:", nrow(merged), "\n")
cat("Disconnected (distance = 999999):", sum(merged$distance >= 999999, na.rm = TRUE), "\n")
cat("Missing gap:", sum(is.na(merged$gap)), "\n")

analysis_data <- merged %>%
  filter(
    distance < 999999,       # Remove disconnected pairs
    !is.na(gap),             # Remove pairs without ideology scores
    camp_type != "other"     # Keep only camp 1 and 2
  )

cat("\n=== Post-filter ===\n")
cat("Analysis rows:", nrow(analysis_data), "\n")
cat("By camp type x session:\n")
print(table(analysis_data$camp_type, analysis_data$session_id))

# ---------------------------
# 8. Descriptive statistics
# ---------------------------
cat("\n=== Descriptive Statistics ===\n")

desc_stats <- analysis_data %>%
  group_by(session_id, camp_type) %>%
  summarise(
    n         = n(),
    mean_dist = round(mean(distance, na.rm = TRUE), 3),
    sd_dist   = round(sd(distance, na.rm = TRUE), 3),
    mean_gap  = round(mean(gap, na.rm = TRUE), 3),
    sd_gap    = round(sd(gap, na.rm = TRUE), 3),
    .groups   = "drop"
  )

print(desc_stats)
write.csv(desc_stats, "descriptive_stats_by_session_camp.csv", row.names = FALSE)

# ---------------------------
# 9. Helper: OLS with clustered SEs
# ---------------------------
# Dyadic data violates independence: each legislator appears in many dyads.
# We cluster on dyad_id (unique undirected pair x session).

lm_clustered <- function(formula, data, cluster_var = "dyad_id") {
  model <- lm(formula, data = data)
  cl_vcov <- vcovCL(model, cluster = data[[cluster_var]])
  ct <- coeftest(model, vcov = cl_vcov)
  return(list(model = model, coeftest = ct, vcov_cl = cl_vcov))
}

# ============================================================
# 10. MAIN MODELS — H1: Distance ~ Gap + controls + session FE
# ============================================================
cat("\n=== Main Models (H1) ===\n")

main_formula <- distance ~ gap +
  from_female + to_female +
  from_electoral_type + to_electoral_type +
  from_terms_served + to_terms_served +
  from_birth_year + to_birth_year +
  same_region +
  factor(session_id)

# All pairs
h1_all <- lm_clustered(main_formula, analysis_data)
cat("\n--- H1: All Pairs ---\n")
print(h1_all$coeftest)

# Opposing camp
h1_opp <- lm_clustered(main_formula,
                       analysis_data %>% filter(camp_type == "opposing_camp"))
cat("\n--- H1: Opposing Camps ---\n")
print(h1_opp$coeftest)

# Same camp
h1_same <- lm_clustered(main_formula,
                        analysis_data %>% filter(camp_type == "same_camp"))
cat("\n--- H1: Same Camps ---\n")
print(h1_same$coeftest)

# Stargazer table with clustered SEs
stargazer(h1_all$model, h1_opp$model, h1_same$model,
          type = "text",
          se = list(sqrt(diag(h1_all$vcov_cl)),
                    sqrt(diag(h1_opp$vcov_cl)),
                    sqrt(diag(h1_same$vcov_cl))),
          title = "Table 3: Main Models (H1) - Clustered SEs",
          column.labels = c("All Pairs", "Opposing Camps", "Same Camps"),
          dep.var.labels = "Network Distance",
          out = "table3_main_models.txt")

cat("Saved: table3_main_models.txt\n")

# ============================================================
# 11. INTERACTION MODELS — H2: Gap x SecondHalf
# ============================================================
# COLLINEARITY NOTE:
# session_id has 4 levels: S201, S202, S211, S212
# second_half = 1 for S202, S212; = 0 for S201, S211
# term21      = 1 for S211, S212; = 0 for S201, S202
#
# second_half + term21 spans the same space as session FE
# (they are two orthogonal binary contrasts of the 4 sessions)
# but neither is collinear with gap:second_half.
# This is the correct specification for the interaction model.
# ============================================================
cat("\n=== Interaction Models (H2) ===\n")

interaction_formula <- distance ~ gap * second_half +
  from_female + to_female +
  from_electoral_type + to_electoral_type +
  from_terms_served + to_terms_served +
  from_birth_year + to_birth_year +
  same_region +
  term21

# All pairs
h2_all <- lm_clustered(interaction_formula, analysis_data)
cat("\n--- H2: All Pairs ---\n")
print(h2_all$coeftest)

# Opposing camp
h2_opp <- lm_clustered(interaction_formula,
                       analysis_data %>% filter(camp_type == "opposing_camp"))
cat("\n--- H2: Opposing Camps ---\n")
print(h2_opp$coeftest)

# Same camp
h2_same <- lm_clustered(interaction_formula,
                        analysis_data %>% filter(camp_type == "same_camp"))
cat("\n--- H2: Same Camps ---\n")
print(h2_same$coeftest)

# Stargazer table
stargazer(h2_all$model, h2_opp$model, h2_same$model,
          type = "text",
          se = list(sqrt(diag(h2_all$vcov_cl)),
                    sqrt(diag(h2_opp$vcov_cl)),
                    sqrt(diag(h2_same$vcov_cl))),
          title = "Table 4: Interaction Models (H2) - Gap x SecondHalf",
          column.labels = c("All Pairs", "Opposing Camps", "Same Camps"),
          dep.var.labels = "Network Distance",
          out = "table4_interaction_models.txt")

cat("Saved: table4_interaction_models.txt\n")

# ---------------------------
# 11b. Interpret interaction
# ---------------------------
cat("\n=== Interaction Coefficient Interpretation ===\n")

interpret_interaction <- function(ct, label) {
  gap_est <- ct["gap", "Estimate"]
  gap_p   <- ct["gap", "Pr(>|t|)"]
  int_est <- ct["gap:second_half", "Estimate"]
  int_p   <- ct["gap:second_half", "Pr(>|t|)"]
  
  cat("\n---", label, "---\n")
  cat("  Gap effect (first half):   ", round(gap_est, 4),
      " (p =", formatC(gap_p, format = "f", digits = 4), ")\n")
  cat("  Gap x SecondHalf:          ", round(int_est, 4),
      " (p =", formatC(int_p, format = "f", digits = 4), ")\n")
  cat("  Gap effect (second half):  ", round(gap_est + int_est, 4), "\n")
  cat("  Direction: gap effect",
      ifelse(int_est > 0, "STRENGTHENS", "WEAKENS"),
      "in second half\n")
}

interpret_interaction(h2_all$coeftest,  "All Pairs")
interpret_interaction(h2_opp$coeftest,  "Opposing Camps")
interpret_interaction(h2_same$coeftest, "Same Camps")

# ============================================================
# 12. SPLIT-SAMPLE MODELS (for Figure 4 coefficient plot)
# ============================================================
cat("\n=== Split-Sample Models ===\n")

split_formula <- distance ~ gap +
  from_female + to_female +
  from_electoral_type + to_electoral_type +
  from_terms_served + to_terms_served +
  from_birth_year + to_birth_year +
  same_region

gap_coefs <- data.frame()

for (sid in TARGET_SESSIONS) {
  for (ctype in c("opposing_camp", "same_camp")) {
    sub <- analysis_data %>% filter(session_id == sid, camp_type == ctype)
    
    if (nrow(sub) < 50) {
      cat("  Skipping", sid, ctype, "- too few obs\n")
      next
    }
    
    res <- lm_clustered(split_formula, sub)
    ct <- res$coeftest
    
    gap_coefs <- rbind(gap_coefs, data.frame(
      session_id = sid,
      camp_type  = ctype,
      estimate   = ct["gap", "Estimate"],
      std_error  = ct["gap", "Std. Error"],
      p_value    = ct["gap", "Pr(>|t|)"],
      n_obs      = nrow(sub),
      stringsAsFactors = FALSE
    ))
  }
}

gap_coefs <- gap_coefs %>%
  mutate(
    group    = ifelse(camp_type == "same_camp", "Same", "Opposing"),
    ci_lower = estimate - 1.96 * std_error,
    ci_upper = estimate + 1.96 * std_error
  )

print(gap_coefs)
write.csv(gap_coefs, "gap_coefficients_by_session_camp.csv", row.names = FALSE)

# ============================================================
# 13. COEFFICIENT PLOT (Figure 4)
# ============================================================
cat("\n=== Generating Figure 4 ===\n")

gap_coefs$session_id <- factor(gap_coefs$session_id,
                               levels = c("S201", "S202", "S211", "S212"))

fig4 <- ggplot(gap_coefs, aes(x = session_id, y = estimate,
                              group = group, color = group, shape = group)) +
  geom_point(size = 3, position = position_dodge(0.2)) +
  geom_line(position = position_dodge(0.2)) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                width = 0.15, position = position_dodge(0.2)) +
  geom_vline(xintercept = 2.5, linetype = "dashed", color = "grey50") +
  annotate("text", x = 1.5, y = max(gap_coefs$ci_upper) * 0.95,
           label = "20th Assembly", size = 3, color = "grey40") +
  annotate("text", x = 3.5, y = max(gap_coefs$ci_upper) * 0.95,
           label = "21st Assembly", size = 3, color = "grey40") +
  scale_color_manual(values = c("Same" = "black", "Opposing" = "grey40")) +
  labs(
    title = "Effect of Ideological Gap on Network Distance",
    subtitle = "By Camp Type and Session (clustered SEs, 95% CI)",
    x = "Session", y = "Coefficient Estimate",
    color = "Group", shape = "Group"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(fig4)
ggsave("figure4_gap_coefficients.png", fig4, width = 8, height = 6, dpi = 300)
cat("Saved: figure4_gap_coefficients.png\n")

# ============================================================
# 14. PARTY-LEVEL ANALYSIS (Robustness Check for H2)
# ============================================================
# Factionalism is conceptually an intra-party phenomenon.
# Camp-level classification may be too coarse to detect 
# bottom-up (legislator avoidance) effects.
# Here we restrict to major parties and use party_type.
# ============================================================
cat("\n=== Party-Level Analysis (Robustness) ===\n")

MAJOR_PARTIES <- c("PT03", "PT05", "PT06", "PT07", "PT08", "PT15")

# Filter to dyads where BOTH legislators belong to a major party
party_data <- analysis_data %>%
  filter(
    from_party_id %in% MAJOR_PARTIES,
    to_party_id %in% MAJOR_PARTIES
  ) %>%
  mutate(
    party_type2 = case_when(
      from_party_id == to_party_id ~ "same_party",
      from_camp_id != to_camp_id   ~ "opposing_party",
      from_camp_id == to_camp_id & from_party_id != to_party_id ~ "same_camp_diff_party",
      TRUE ~ "other"
    )
  )

cat("Party-level analysis sample:", nrow(party_data), "\n")
cat("By party_type2:\n")
print(table(party_data$party_type2, party_data$session_id))

# --- 14a. Interaction: Opposing party ---
cat("\n--- H2 Party-Level: Opposing Party ---\n")
h2_opp_party <- lm_clustered(interaction_formula,
                             party_data %>% filter(party_type2 == "opposing_party"))
print(h2_opp_party$coeftest)

# --- 14b. Interaction: Same party ---
cat("\n--- H2 Party-Level: Same Party ---\n")
h2_same_party <- lm_clustered(interaction_formula,
                              party_data %>% filter(party_type2 == "same_party"))
print(h2_same_party$coeftest)

# --- 14c. Interpret ---
cat("\n=== Party-Level Interaction Interpretation ===\n")
interpret_interaction(h2_opp_party$coeftest,  "Opposing Party (major parties only)")
interpret_interaction(h2_same_party$coeftest, "Same Party (major parties only)")

# --- 14d. Stargazer table ---
stargazer(h2_opp_party$model, h2_same_party$model,
          type = "text",
          se = list(sqrt(diag(h2_opp_party$vcov_cl)),
                    sqrt(diag(h2_same_party$vcov_cl))),
          title = "Table A: Party-Level Interaction (Robustness) - Gap x SecondHalf",
          column.labels = c("Opposing Party", "Same Party"),
          dep.var.labels = "Network Distance",
          out = "tableA_party_interaction.txt")

cat("Saved: tableA_party_interaction.txt\n")

# --- 14e. Split-sample by session for party-level (Figure A) ---
gap_coefs_party <- data.frame()

for (sid in TARGET_SESSIONS) {
  for (ptype in c("opposing_party", "same_party")) {
    sub <- party_data %>% filter(session_id == sid, party_type2 == ptype)
    
    if (nrow(sub) < 50) {
      cat("  Skipping", sid, ptype, "- too few obs\n")
      next
    }
    
    res <- lm_clustered(split_formula, sub)
    ct <- res$coeftest
    
    gap_coefs_party <- rbind(gap_coefs_party, data.frame(
      session_id = sid,
      party_type = ptype,
      estimate   = ct["gap", "Estimate"],
      std_error  = ct["gap", "Std. Error"],
      p_value    = ct["gap", "Pr(>|t|)"],
      n_obs      = nrow(sub),
      stringsAsFactors = FALSE
    ))
  }
}

gap_coefs_party <- gap_coefs_party %>%
  mutate(
    group    = ifelse(party_type == "same_party", "Same Party", "Opposing Party"),
    ci_lower = estimate - 1.96 * std_error,
    ci_upper = estimate + 1.96 * std_error
  )

print(gap_coefs_party)
write.csv(gap_coefs_party, "gap_coefficients_by_session_party.csv", row.names = FALSE)

# --- 14f. Coefficient plot (Figure A: party-level) ---
gap_coefs_party$session_id <- factor(gap_coefs_party$session_id,
                                     levels = c("S201", "S202", "S211", "S212"))

figA <- ggplot(gap_coefs_party, aes(x = session_id, y = estimate,
                                    group = group, color = group, shape = group)) +
  geom_point(size = 3, position = position_dodge(0.2)) +
  geom_line(position = position_dodge(0.2)) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                width = 0.15, position = position_dodge(0.2)) +
  geom_vline(xintercept = 2.5, linetype = "dashed", color = "grey50") +
  annotate("text", x = 1.5, y = max(gap_coefs_party$ci_upper) * 0.95,
           label = "20th Assembly", size = 3, color = "grey40") +
  annotate("text", x = 3.5, y = max(gap_coefs_party$ci_upper) * 0.95,
           label = "21st Assembly", size = 3, color = "grey40") +
  scale_color_manual(values = c("Same Party" = "black", "Opposing Party" = "grey40")) +
  labs(
    title = "Effect of Ideological Gap on Network Distance (Major Parties)",
    subtitle = "By Party Type and Session (clustered SEs, 95% CI)",
    x = "Session", y = "Coefficient Estimate",
    color = "Group", shape = "Group"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(figA)
ggsave("figureA_party_gap_coefficients.png", figA, width = 8, height = 6, dpi = 300)
cat("Saved: figureA_party_gap_coefficients.png\n")

# ============================================================
# 15. Summary
# ============================================================
cat("\n=== Part 3 Complete ===\n")
cat("Output files:\n")
cat("  descriptive_stats_by_session_camp.csv\n")
cat("  table3_main_models.txt           (H1: pooled, clustered SEs)\n")
cat("  table4_interaction_models.txt     (H2: camp-level interaction)\n")
cat("  tableA_party_interaction.txt      (H2: party-level robustness)\n")
cat("  gap_coefficients_by_session_camp.csv\n")
cat("  gap_coefficients_by_session_party.csv\n")
cat("  figure4_gap_coefficients.png\n")
cat("  figureA_party_gap_coefficients.png\n")


