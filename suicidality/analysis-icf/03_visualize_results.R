# 03_visualize_results.R
# Visualize iCF results
#
# This script creates visualizations of the iCF subgroup analysis results.

library(tidyverse)
library(ggplot2)
library(here)
here::i_am("suicidality/analysis-icf/03_visualize_results.R")

# Compact partykit-based tree renderer (shared with analysis-hdicf)
source(here("suicidality", "analysis-icf", "icf", "tree_render.R"))

# =============================================================================
# LOAD RESULTS
# =============================================================================

cat("Loading iCF results...\n")

output_dir <- here("suicidality", "analysis-icf", "output")
results <- readRDS(file.path(output_dir, "icf_results.rds"))
config <- readRDS(file.path(output_dir, "config.rds"))

# Load variable importance
vi_df <- read.csv(file.path(output_dir, "variable_importance.csv"))

# =============================================================================
# PLOT 1: VARIABLE IMPORTANCE (raw + human-readable versions)
# =============================================================================

cat("\nCreating variable importance plots...\n")

# Load human-readable variable labels
var_labels <- jsonlite::fromJSON(
  here("suicidality", "analysis-icf", "variable_labels.json")
)

# Mark variables that survived the iCF variable-selection step. The threshold
# line is drawn at the importance of the lowest-ranked selected variable.
selected_set <- if (!is.null(results$var_names)) results$var_names else character(0)
vi_threshold <- if (length(selected_set) > 0) {
  min(vi_df$importance[vi_df$variable %in% selected_set])
} else mean(vi_df$importance)

# Select top 20 variables; flag whether each is in the selected set
vi_top <- vi_df %>%
  head(20) %>%
  mutate(
    variable = factor(variable, levels = rev(variable)),
    selected = as.character(variable) %in% selected_set
  )

p_vi_raw <- ggplot(vi_top, aes(x = variable, y = importance, fill = selected)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = vi_threshold, linetype = "dashed", color = "gray30") +
  scale_fill_manual(
    values = c(`TRUE` = "steelblue", `FALSE` = "gray70"),
    labels = c(`TRUE` = "Selected (above mean)", `FALSE` = "Not selected"),
    breaks = c("TRUE", "FALSE"),
    name = NULL
  ) +
  coord_flip() +
  labs(
    title = "Variable Importance from Raw Causal Forest",
    subtitle = sprintf("Top 20 variables; %d selected by above-mean VI (dashed line at threshold)",
                       length(selected_set)),
    x = "Variable",
    y = "Importance (weighted split frequency)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 10),
    legend.position = "bottom"
  )

ggsave(
  file.path(output_dir, "variable_importance_raw.pdf"),
  p_vi_raw, width = 8, height = 6
)
cat("Saved: variable_importance_raw.pdf\n")

# Human-readable version
vi_top_pretty <- vi_top %>%
  mutate(
    label = ifelse(as.character(variable) %in% names(var_labels),
                   var_labels[as.character(variable)],
                   as.character(variable)),
    label = factor(label, levels = rev(label))
  )

p_vi <- ggplot(vi_top_pretty, aes(x = label, y = importance, fill = selected)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = vi_threshold, linetype = "dashed", color = "gray30") +
  scale_fill_manual(
    values = c(`TRUE` = "steelblue", `FALSE` = "gray70"),
    labels = c(`TRUE` = "Selected (above mean)", `FALSE` = "Not selected"),
    breaks = c("TRUE", "FALSE"),
    name = NULL
  ) +
  coord_flip() +
  labs(
    title = "Variable Importance from Raw Causal Forest",
    subtitle = sprintf("Top 20 variables; %d selected by above-mean VI (dashed line at threshold)",
                       length(selected_set)),
    x = NULL,
    y = "Importance (weighted split frequency)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 10),
    legend.position = "bottom"
  )

ggsave(
  file.path(output_dir, "variable_importance.pdf"),
  p_vi, width = 8, height = 6
)
cat("Saved: variable_importance.pdf\n")

# =============================================================================
# PLOT 2: SUBGROUP CATE FOREST PLOT
# =============================================================================

cat("\nCreating CATE forest plot...\n")

if (!is.null(results$cate) && nrow(results$cate) > 0) {

  cate_df <- results$cate %>%
    dplyr::mutate(
      plot_label = paste0("SG", subgroup_id, "\n(n=", n_total, ")"),
      # Ensure ci columns exist even if no bootstrap was run
      ci_lower = ifelse(is.na(ci_lower), iptw_cate, ci_lower),
      ci_upper = ifelse(is.na(ci_upper), iptw_cate, ci_upper)
    ) %>%
    dplyr::arrange(iptw_cate) %>%
    dplyr::mutate(y_label = factor(plot_label, levels = plot_label))

  p_cate <- ggplot(cate_df, aes(x = iptw_cate, y = y_label)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), height = 0.2,
                   color = "steelblue") +
    geom_point(shape = 15, size = 3, color = "steelblue") +
    labs(
      title = "Conditional Average Treatment Effects by Subgroup",
      subtitle = "SSRI vs. No SSRI initiation on suicidal behavior risk",
      x = "IPTW-weighted Risk Difference (percentage points)",
      y = NULL
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.text.y = element_text(size = 12)
    )

  ggsave(
    file.path(output_dir, "cate_forest_plot.pdf"),
    p_cate, width = 10, height = max(6, nrow(cate_df) * 0.8)
  )

  cat("Saved: cate_forest_plot.pdf\n")
}

# =============================================================================
# PLOT 2b: PER-DEPTH SUBGROUP CATE FOREST PLOTS
# =============================================================================

if (!is.null(results$all_depth_results)) {
  cat("\nCreating per-depth CATE forest plots...\n")
  for (dname in names(results$all_depth_results)) {
    ar <- results$all_depth_results[[dname]]
    if (is.null(ar$cate) || nrow(ar$cate) == 0) next
    depth_cate <- ar$cate %>%
      dplyr::mutate(
        plot_label = paste0("SG", subgroup_id, "\n(n=", n_total, ")"),
        ci_lower = ifelse(is.na(ci_lower), iptw_cate, ci_lower),
        ci_upper = ifelse(is.na(ci_upper), iptw_cate, ci_upper)
      ) %>%
      dplyr::arrange(iptw_cate) %>%
      dplyr::mutate(y_label = factor(plot_label, levels = plot_label))

    p <- ggplot(depth_cate, aes(x = iptw_cate, y = y_label)) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
      geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), height = 0.2,
                     color = "steelblue") +
      geom_point(shape = 15, size = 3, color = "steelblue") +
      labs(
        title = sprintf("Conditional Average Treatment Effects by Subgroup (%s)", dname),
        subtitle = "SSRI vs. No SSRI initiation on suicidal behavior risk",
        x = "IPTW-weighted Risk Difference (percentage points)",
        y = NULL
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        axis.text.y = element_text(size = 12)
      )

    fname <- sprintf("cate_forest_plot_%s.pdf", dname)
    ggsave(file.path(output_dir, fname), p,
           width = 10, height = max(4, nrow(depth_cate) * 0.8))
    cat("Saved:", fname, "\n")
  }
}

# =============================================================================
# PLOT 3: DECISION TREE VISUALIZATION
# =============================================================================

cat("\nExtracting decision tree structure...\n")

# Create text summary of subgroup decision
if (!is.null(results$subgroup_labels)) {

  # Save as text file
  sink(file.path(output_dir, "subgroup_decision.txt"))
  cat("==============================================\n")
  cat("iCF SUBGROUP DECISION\n")
  cat("==============================================\n\n")
  cat("Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")
  cat("Configuration:\n")
  cat("  - K-fold CV:", config$K, "\n")
  cat("  - Trees per forest:", config$n_trees, "\n")
  cat("  - CV iterations:", config$n_iterations, "\n")
  cat("  - Final iterations:", config$n_iterations_final %||% config$n_iterations, "\n")
  cat("  - Best depth:", results$best_depth, "\n\n")
  cat("Heterogeneity test p-value:", results$het_p_value, "\n\n")
  cat("Selected Variables:\n")
  cat("----------------------------------------------\n")
  cat(paste(results$var_names, collapse = ", "), "\n\n")
  cat("Subgroup Labels:\n")
  cat("----------------------------------------------\n")
  for (i in seq_along(results$subgroup_labels)) {
    cat("  ", i, ":", results$subgroup_labels[i], "\n")
  }
  cat("\n")

  if (!is.null(results$cate) && nrow(results$cate) > 0) {
    cat("\nCATE Estimates:\n")
    cat("----------------------------------------------\n")
    print(results$cate)
  }
  sink()

  cat("Saved: subgroup_decision.txt\n")
}

# Create decision tree visualization
cat("\nCreating decision tree plot...\n")

if (!is.null(results$final_result$synthetic_splits) && !is.null(results$cate) && nrow(results$cate) > 0) {

  splits <- results$final_result$synthetic_splits
  cate_df <- results$cate

  # Helper function to get CATE by subgroup label (exact match)
  get_cate <- function(label) {
    row <- which(cate_df$label == label)
    if (length(row) > 0) {
      round(cate_df$iptw_cate[row[1]], 2)
    } else {
      NA
    }
  }

  # Helper function to get full CATE row by subgroup label (exact match)
  get_cate_row <- function(label) {
    row <- which(cate_df$label == label)
    if (length(row) > 0) {
      cate_df[row[1], ]
    } else {
      NULL
    }
  }

  # Helper to format decision node labels in human-readable form
  format_split_label <- function(var_name, split_val) {
    # Binary indicators (0/1): split "var <= 0" means Yes = 0 (absent), No = 1 (present)
    # Labels should match the <= 0 condition, i.e., the "no" / "absent" interpretation
    binary_labels <- c(
      female = "Male?",
      hosp = "No prior hospitalization?",
      diag_mdd = "No prior MDD?",
      diag_bipolar = "No prior bipolar disorder?",
      diag_psychotic = "No prior psychotic disorder?",
      diag_alcohol = "No prior alcohol use disorder?",
      diag_sud = "No prior substance use disorder?",
      diag_suicidal = "No prior suicidal behavior?",
      diag_overdose = "No prior overdose?",
      diag_stress = "No prior stress disorder?",
      diag_anxiety_other = "No prior anxiety disorder?",
      diag_phobic = "No prior phobic disorder?",
      diag_sleep = "No prior sleep disorder?",
      diag_organic = "No prior organic disorder?",
      diag_anorexia = "No prior anorexia?",
      diag_bulimia = "No prior bulimia?",
      diag_ocd = "No prior OCD?",
      diag_conduct = "No prior conduct disorder?",
      diag_intellectual_disability = "No prior intellectual disability?",
      diag_personality_cluster_b = "No prior cluster B personality disorder?",
      diag_adhd = "No prior ADHD?",
      diag_autism = "No prior autism?",
      med_antipsychotic = "No prior antipsychotic use?",
      med_hypnotic = "No prior hypnotic use?",
      med_benzodiazepine = "No prior benzodiazepine use?",
      med_antiepileptic = "No prior antiepileptic use?",
      med_stimulant = "No prior stimulant use?",
      med_opioid = "No prior opioid use?",
      med_mood_stabilizer = "No prior mood stabilizer use?",
      med_addiction = "No prior addiction medication?"
    )

    # Categorical variables: rephrase with readable thresholds.
    # Note: missing values for edufam_cat / inc_cat / fh_suicidal / fh_depr are
    # encoded as 9 (sentinel above any real category; see 01_prepare_data.R).
    # Splits with thresholds above the highest real category separate
    # "non-missing" from "missing" rather than ordinal-style category cuts.
    categorical_labels <- list(
      age_cat = function(sv) {
        if (sv < 0.5) "Child (6-11)?"
        else if (sv < 1.5) "Child/Adolescent (\u226417)?"
        else "Child/Adolescent/Young adult?"
      },
      edufam_cat = function(sv) {
        if (sv > 2.5) "Parental education recorded (vs.\u00a0missing)?"
        else sprintf("Parental education \u2264 %g?", sv)
      },
      inc_cat = function(sv) {
        if (sv > 5.5) "Income quintile recorded (vs.\u00a0missing)?"
        else sprintf("Income quintile \u2264 %g?", sv)
      },
      fh_suicidal = function(sv) {
        if (sv > 1.5) "Family history of suicidality recorded (vs.\u00a0missing)?"
        else if (sv < 0.5) "No family history of suicidality?"
        else "Family history of suicidality known?"
      },
      fh_depr = function(sv) {
        if (sv > 1.5) "Family history of depression recorded (vs.\u00a0missing)?"
        else if (sv < 0.5) "No family history of depression?"
        else "Family history of depression known?"
      },
      source = function(sv) sprintf("Care setting \u2264 %g?", sv)
    )

    # Binary 0/1 variables: grf typically splits at the midpoint (0.5), so
    # accept any threshold strictly less than 1 as a binary-style split.
    # Previously gated on `split_val <= 0`, which never fired for grf splits.
    if (var_name %in% names(binary_labels) && split_val < 1) {
      return(binary_labels[[var_name]])
    }
    if (var_name %in% names(categorical_labels)) {
      return(categorical_labels[[var_name]](split_val))
    }
    # Fallback
    paste0(var_name, " \u2264 ", split_val, "?")
  }

  # Helper to format CATE value (handle NA and negative)
  format_cate <- function(cate_val) {
    if (is.na(cate_val)) return("N/A")
    if (cate_val >= 0) paste0("+", cate_val) else as.character(cate_val)
  }

  # Helper to format CATE with CI
  format_cate_ci <- function(cate_val, ci_lo, ci_hi) {
    point <- format_cate(cate_val)
    if (!is.na(ci_lo) && !is.na(ci_hi)) {
      paste0(point, " (", round(ci_lo, 2), ", ", round(ci_hi, 2), ")")
    } else {
      point
    }
  }

  # ============================================================================
  # DYNAMIC TREE BUILDING (Fix #14)
  # Build tree structure from synthetic_splits instead of hard-coding
  # ============================================================================

  # Build node list dynamically from splits
  # label_fn: function(var_name, split_val) returning a string for decision nodes
  build_tree_layout <- function(splits, cate_df, label_fn) {
    if (is.null(splits) || nrow(splits) == 0) {
      return(list(nodes = NULL, edges = NULL))
    }

    # Local CATE lookup functions using THIS cate_df, not the outer scope
    get_cate_local <- function(label) {
      row <- which(cate_df$label == label)
      if (length(row) > 0) round(cate_df$iptw_cate[row[1]], 2) else NA
    }
    get_cate_row_local <- function(label) {
      row <- which(cate_df$label == label)
      if (length(row) > 0) cate_df[row[1], ] else NULL
    }

    nodes_list <- list()
    edges_list <- list()
    node_counter <- 1
    leaf_spacing <- 1  # horizontal space per leaf

    # Count leaves in subtree rooted at node_id
    count_leaves <- function(node_id) {
      split_row <- splits[splits$node_id == node_id, ]
      if (nrow(split_row) == 0) return(1)  # leaf
      count_leaves(split_row$left_child[1]) + count_leaves(split_row$right_child[1])
    }

    # Recursive function to build tree
    # x_left = left edge of allocated x-range, x_right = right edge
    build_node <- function(node_id, depth, x_left, x_right, path = "SG") {
      x_pos <- (x_left + x_right) / 2
      split_row <- splits[splits$node_id == node_id, ]

      if (nrow(split_row) == 0) {
        # This is a leaf node — Wang et al. style label
        cate_row <- get_cate_row_local(path)
        if (!is.null(cate_row)) {
          sg_id <- cate_row$subgroup_id
          cate_val <- round(cate_row$iptw_cate, 2)
          ci_lo <- if ("ci_lower" %in% names(cate_row)) cate_row$ci_lower else NA
          ci_hi <- if ("ci_upper" %in% names(cate_row)) cate_row$ci_upper else NA
          label_text <- paste0(
            sg_id, ": SSRI vs. No SSRI\n",
            "Size: ", format(cate_row$n_treated, big.mark = ","),
            " vs. ", format(cate_row$n_control, big.mark = ","), "\n",
            "Events: ", format(cate_row$events_treated, big.mark = ","),
            " vs. ", format(cate_row$events_control, big.mark = ","), "\n",
            "aRD: ", format_cate_ci(cate_val, ci_lo, ci_hi), " pp"
          )
        } else {
          cate_val <- get_cate_local(path)
          label_text <- paste0(path, "\nCATE: ", format_cate(cate_val))
        }

        nodes_list[[length(nodes_list) + 1]] <<- data.frame(
          id = node_counter,
          x = x_pos,
          y = -(depth - 1),
          label = label_text,
          is_leaf = TRUE,
          stringsAsFactors = FALSE
        )
        node_counter <<- node_counter + 1
        return()
      }

      # Decision node
      var_name <- split_row$variable[1]
      split_val <- round(split_row$split_value[1], 2)
      node_label <- label_fn(var_name, split_val)

      current_id <- node_counter
      node_counter <<- node_counter + 1

      nodes_list[[length(nodes_list) + 1]] <<- data.frame(
        id = current_id,
        x = x_pos,
        y = -(depth - 1),
        label = node_label,
        is_leaf = FALSE,
        stringsAsFactors = FALSE
      )

      # Allocate x-space proportional to number of leaves in each subtree
      left_leaves <- count_leaves(split_row$left_child[1])
      right_leaves <- count_leaves(split_row$right_child[1])
      total_leaves <- left_leaves + right_leaves
      left_frac <- left_leaves / total_leaves
      x_mid <- x_left + (x_right - x_left) * left_frac

      left_x <- (x_left + x_mid) / 2
      right_x <- (x_mid + x_right) / 2
      child_y <- -depth

      # Add edges
      edges_list[[length(edges_list) + 1]] <<- data.frame(
        from_x = x_pos, from_y = -(depth - 1),
        to_x = left_x, to_y = child_y,
        edge_label = "Yes"
      )
      edges_list[[length(edges_list) + 1]] <<- data.frame(
        from_x = x_pos, from_y = -(depth - 1),
        to_x = right_x, to_y = child_y,
        edge_label = "No"
      )

      # Recurse to children
      left_path <- paste0(path, "_", var_name, "<=", split_val)
      right_path <- paste0(path, "_", var_name, ">", split_val)

      build_node(split_row$left_child[1], depth + 1, x_left, x_mid, left_path)
      build_node(split_row$right_child[1], depth + 1, x_mid, x_right, right_path)
    }

    # Total leaves determines the total x-range
    total_leaves <- count_leaves(1)

    # Start building from root
    build_node(1, 1, 0, total_leaves * leaf_spacing, "SG")

    nodes <- do.call(rbind, nodes_list)
    edges <- if (length(edges_list) > 0) do.call(rbind, edges_list) else NULL

    return(list(nodes = nodes, edges = edges))
  }

  # Helper: raw label function (e.g., "female <= 0?")
  raw_label_fn <- function(var_name, split_val) {
    paste0(var_name, " \u2264 ", split_val, "?")
  }

  # Helper: render tree plot from layout
  render_tree <- function(nodes, edges) {
    x_range <- diff(range(nodes$x))
    x_margin <- max(x_range * 0.1, 0.5)

    ggplot() +
      geom_segment(
        data = edges,
        aes(x = from_x, y = from_y, xend = to_x, yend = to_y),
        color = "gray40", linewidth = 0.8
      ) +
      geom_text(
        data = edges,
        aes(x = (from_x + to_x) / 2, y = (from_y + to_y) / 2, label = edge_label),
        size = 5, color = "gray30", nudge_x = x_range * 0.01
      ) +
      geom_label(
        data = nodes %>% filter(!is_leaf),
        aes(x = x, y = y, label = label),
        fill = "lightblue", color = "black", size = 6,
        label.padding = unit(0.4, "lines")
      ) +
      geom_label(
        data = nodes %>% filter(is_leaf),
        aes(x = x, y = y, label = label),
        fill = "lightgreen", color = "black", size = 4,
        label.padding = unit(0.5, "lines")
      ) +
      theme_void() +
      theme(plot.margin = margin(10, 10, 10, 10)) +
      coord_cartesian(
        xlim = range(nodes$x) + c(-x_margin, x_margin),
        ylim = range(nodes$y) + c(-0.3, 0.3),
        clip = "off"
      )
  }

  # Helper: save tree with pdfcrop, scaling dimensions to tree size
  save_tree <- function(p, filename, nodes = NULL) {
    out_path <- file.path(output_dir, filename)
    # Scale dimensions based on tree size
    if (!is.null(nodes)) {
      n_leaves <- sum(nodes$is_leaf)
      tree_depth <- length(unique(round(nodes$y, 2)))
      w <- max(14, n_leaves * 4)
      h <- max(9, tree_depth * 3)
    } else {
      w <- 14
      h <- 9
    }
    ggsave(out_path, p, width = w, height = h)
    if (nzchar(Sys.which("pdfcrop"))) {
      system2("pdfcrop", c(out_path, out_path))
    } else {
      warning("pdfcrop not found — saving uncropped PDF for ", filename)
    }
    cat("Saved:", filename, "\n")
  }

  # Render both pretty and raw versions via the shared partykit renderer.
  var_names_for_tree <- colnames(results$X)[results$selected_vars]

  ok_pretty <- save_partykit_tree(
    splits    = splits,
    cate_df   = cate_df,
    var_names = var_names_for_tree,
    out_path  = file.path(output_dir, "decision_tree.pdf")
    # label_fn omitted → tree_render's default_split_label (topic style)
  )
  ok_raw <- save_partykit_tree(
    splits    = splits,
    cate_df   = cate_df,
    var_names = var_names_for_tree,
    out_path  = file.path(output_dir, "decision_tree_raw.pdf"),
    label_fn  = raw_label_fn
  )
  if (isTRUE(ok_pretty)) cat("Saved: decision_tree.pdf\n")
  if (isTRUE(ok_raw))    cat("Saved: decision_tree_raw.pdf\n")

  # Save split-label mapping as JSON for easy verification (raw -> pretty).
  if (isTRUE(ok_pretty)) {
    raw_labels    <- vapply(seq_len(nrow(splits)),
                             function(i) raw_label_fn(splits$variable[i], round(splits$split_value[i], 2)),
                             character(1))
    pretty_labels <- vapply(seq_len(nrow(splits)),
                             function(i) format_split_label(splits$variable[i], round(splits$split_value[i], 2)),
                             character(1))
    label_map <- setNames(as.list(pretty_labels), raw_labels)
    writeLines(
      jsonlite::toJSON(label_map, auto_unbox = TRUE, pretty = TRUE),
      file.path(output_dir, "decision_tree_labels.json")
    )
    cat("Saved: decision_tree_labels.json\n")
  }
}

# Per-depth decision trees
if (!is.null(results$all_depth_results)) {
  cat("\nGenerating per-depth decision tree plots...\n")
  var_names_for_tree <- colnames(results$X)[results$selected_vars]
  for (dname in names(results$all_depth_results)) {
    dr <- results$all_depth_results[[dname]]
    if (is.null(dr) || !isTRUE(dr$final_result$success)) next

    depth_splits <- dr$final_result$synthetic_splits
    depth_cate   <- dr$cate
    if (is.null(depth_splits) || is.null(depth_cate)) next

    filename <- sprintf("decision_tree_%s.pdf", dname)
    ok <- save_partykit_tree(
      splits    = depth_splits,
      cate_df   = depth_cate,
      var_names = var_names_for_tree,
      out_path  = file.path(output_dir, filename)
      # label_fn omitted → tree_render's default_split_label (topic style)
    )
    if (isTRUE(ok)) cat(sprintf("Saved: %s\n", filename))
  }
}

# =============================================================================
# PLOT 4: SUBGROUP EVENT RATES
# =============================================================================

cat("\nCreating event rate comparison plot...\n")

if (!is.null(results$cate) && nrow(results$cate) > 0) {

  # Prepare data for event rate plot
  event_data <- results$cate %>%
    tidyr::pivot_longer(
      cols = c(rate_treated, rate_control),
      names_to = "group",
      values_to = "event_rate"
    ) %>%
    dplyr::mutate(
      group = ifelse(group == "rate_treated", "SSRI", "No SSRI")
    )

  # Raw labels version
  p_rates_raw <- ggplot(event_data, aes(x = label, y = event_rate, fill = group)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
    scale_fill_manual(values = c("SSRI" = "steelblue", "No SSRI" = "coral")) +
    labs(
      title = "Suicidal Behavior Event Rates by Subgroup",
      subtitle = "Comparing SSRI initiators vs. non-initiators",
      x = "Subgroup",
      y = "Event Rate (%)",
      fill = "Treatment"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
      legend.position = "top"
    )

  ggsave(
    file.path(output_dir, "crude_event_rates_by_subgroup_raw.pdf"),
    p_rates_raw, width = 10, height = 6
  )

  cat("Saved: crude_event_rates_by_subgroup_raw.pdf\n")

  # Human-readable labels version
  # Parse subgroup path labels (e.g. "SG_female<=0_age_cat<=1") into readable form
  prettify_subgroup_label <- function(sg_label) {
    # Binary variable labels: "var<=0" means the condition is true (e.g., "not female" = Male)
    binary_yes <- c(
      female = "Male", hosp = "Not hospitalized",
      diag_mdd = "No prior MDD", diag_bipolar = "No prior bipolar",
      diag_alcohol = "No prior alcohol use disorder",
      diag_sud = "No prior SUD", diag_suicidal = "No prior suicidal behavior",
      diag_overdose = "No prior overdose", diag_stress = "No prior stress disorder",
      diag_anxiety_other = "No prior anxiety", diag_phobic = "No prior phobia",
      diag_sleep = "No prior sleep disorder", diag_ocd = "No prior OCD",
      diag_conduct = "No prior conduct disorder", diag_adhd = "No prior ADHD",
      diag_autism = "No prior autism",
      med_antipsychotic = "No antipsychotic", med_hypnotic = "No hypnotic",
      med_benzodiazepine = "No benzodiazepine", med_antiepileptic = "No antiepileptic",
      med_stimulant = "No stimulant", med_opioid = "No opioid",
      med_mood_stabilizer = "No mood stabilizer", med_addiction = "No addiction med",
      fh_suicidal = "No family hx suicidality", fh_depr = "No family hx depression"
    )
    binary_no <- c(
      female = "Female", hosp = "Prior hospitalization",
      diag_mdd = "Prior MDD", diag_bipolar = "Prior bipolar",
      diag_alcohol = "Prior alcohol use disorder",
      diag_sud = "Prior SUD", diag_suicidal = "Prior suicidal behavior",
      diag_overdose = "Prior overdose", diag_stress = "Prior stress disorder",
      diag_anxiety_other = "Prior anxiety", diag_phobic = "Prior phobia",
      diag_sleep = "Prior sleep disorder", diag_ocd = "Prior OCD",
      diag_conduct = "Prior conduct disorder", diag_adhd = "Prior ADHD",
      diag_autism = "Prior autism",
      med_antipsychotic = "Antipsychotic use", med_hypnotic = "Hypnotic use",
      med_benzodiazepine = "Benzodiazepine use", med_antiepileptic = "Antiepileptic use",
      med_stimulant = "Stimulant use", med_opioid = "Opioid use",
      med_mood_stabilizer = "Mood stabilizer use", med_addiction = "Addiction med use",
      fh_suicidal = "Family hx suicidality", fh_depr = "Family hx depression"
    )
    # age_cat: 0=Children 6-11, 1=Adolescents 12-17, 2=Young adults 18-24
    # Upper age bound for each category
    age_cat_upper <- c(11, 17, 24)

    # Strip "SG_" prefix and split on "_" that follows a digit (condition boundary)
    path <- sub("^SG_", "", sg_label)
    conditions <- strsplit(path, "(?<=[0-9])_", perl = TRUE)[[1]]

    parts <- character(0)
    for (cond in conditions) {
      if (grepl("<=", cond)) {
        m <- regmatches(cond, regexec("^(.+)<=(.+)$", cond))[[1]]
        var_name <- m[2]; val <- as.numeric(m[3])
        if (var_name %in% names(binary_yes) && val <= 0) {
          parts <- c(parts, binary_yes[[var_name]])
        } else if (var_name == "age_cat") {
          upper <- age_cat_upper[min(val + 1, length(age_cat_upper))]
          parts <- c(parts, paste0("\u2264", upper, " years"))
        } else if (var_name == "edufam_cat" && val == 1) {
          parts <- c(parts, "Primary/secondary parental edu.")
        } else {
          parts <- c(parts, cond)
        }
      } else {
        m <- regmatches(cond, regexec("^(.+)>(.+)$", cond))[[1]]
        var_name <- m[2]; val <- as.numeric(m[3])
        if (var_name %in% names(binary_no) && val <= 0) {
          parts <- c(parts, binary_no[[var_name]])
        } else if (var_name == "age_cat") {
          lower <- age_cat_upper[min(val + 1, length(age_cat_upper))] + 1
          parts <- c(parts, paste0(lower, "\u201324 years"))
        } else if (var_name == "edufam_cat" && val == 1) {
          parts <- c(parts, "Post-secondary parental edu.")
        } else {
          parts <- c(parts, cond)
        }
      }
    }
    paste(parts, sep = ", ", collapse = ", ")
  }

  event_data_pretty <- event_data %>%
    dplyr::mutate(
      label = sapply(label, prettify_subgroup_label)
    )

  p_rates <- ggplot(event_data_pretty, aes(x = label, y = event_rate, fill = group)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
    scale_fill_manual(values = c("SSRI" = "steelblue", "No SSRI" = "coral")) +
    labs(
      title = "Suicidal Behavior Event Rates by Subgroup",
      subtitle = "Comparing SSRI initiators vs. non-initiators",
      x = "Subgroup",
      y = "Event Rate (%)",
      fill = "Treatment"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
      legend.position = "top"
    )

  ggsave(
    file.path(output_dir, "crude_event_rates_by_subgroup.pdf"),
    p_rates, width = 10, height = 6
  )

  cat("Saved: crude_event_rates_by_subgroup.pdf\n")
}

# =============================================================================
# PLOT 5: INDIVIDUAL-LEVEL CATE DISTRIBUTION
# =============================================================================

cat("\nCreating individual CATE distribution plot...\n")

if (!is.null(results$cate_individual)) {
  cate_ind <- results$cate_individual
  ate <- mean(cate_ind, na.rm = TRUE)

  # Sort from largest to smallest
  cate_sorted <- sort(cate_ind, decreasing = TRUE)
  # Convert to percentage points for readability
  cate_pp <- cate_sorted * 100

  cate_df_ind <- data.frame(
    rank = seq_along(cate_pp) / length(cate_pp),  # percentile
    cate = cate_pp
  )

  p_cate_dist <- ggplot(cate_df_ind, aes(x = rank, y = cate)) +
    geom_line(color = "steelblue", linewidth = 0.4) +
    geom_hline(yintercept = ate * 100, linetype = "dashed", color = "red",
               linewidth = 0.8) +
    annotate("text", x = 0.02, y = ate * 100, label = sprintf("ATE = %.2f pp", ate * 100),
             vjust = -0.8, hjust = 0, color = "red", size = 3.5) +
    scale_x_continuous(labels = scales::percent_format()) +
    labs(
      title = "Distribution of Individual-Level CATE Estimates",
      subtitle = "From raw causal forest (sorted from highest to lowest)",
      x = "Percentile of patients (ranked by CATE)",
      y = "CATE (percentage points)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold")
    )

  ggsave(
    file.path(output_dir, "cate_distribution.pdf"),
    p_cate_dist, width = 10, height = 6
  )
  cat("Saved: cate_distribution.pdf\n")

  # Histogram + kernel density version
  # x-axis clipped to [-5, 5] pp for readability; raw CF predictions in this
  # cohort extend to roughly [-10, +15] pp but the bulk of mass sits inside
  # [-5, +5]. coord_cartesian clips the view without dropping data, so the
  # kernel density curve remains valid at the boundaries.
  pct_outside <- 100 * mean(cate_df_ind$cate < -5 | cate_df_ind$cate > 5)
  p_cate_hist <- ggplot(cate_df_ind, aes(x = cate)) +
    geom_histogram(aes(y = after_stat(density)), binwidth = 0.25,
                   fill = "steelblue", color = "white", alpha = 0.7) +
    geom_density(color = "darkblue", linewidth = 0.8) +
    geom_vline(xintercept = ate * 100, linetype = "dashed", color = "red",
               linewidth = 0.8) +
    annotate("text", x = ate * 100, y = Inf, label = sprintf("ATE = %.2f pp", ate * 100),
             vjust = 2, hjust = -0.1, color = "red", size = 3.5) +
    coord_cartesian(xlim = c(-5, 5)) +
    labs(
      title = "Distribution of Individual-Level CATE Estimates",
      subtitle = sprintf("From raw causal forest (x-axis clipped to [-5, +5]. %.2f%% of patients have CATE outside this range)",
                         pct_outside),
      x = "CATE (percentage points)",
      y = "Density"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold")
    )

  ggsave(
    file.path(output_dir, "cate_distribution_histogram.pdf"),
    p_cate_hist, width = 10, height = 6
  )
  cat("Saved: cate_distribution_histogram.pdf\n")

} else {
  cat("No individual CATE predictions available (cate_individual not in results).\n")
}

# =============================================================================
# PLOT 6: CATE vs BASELINE RISK
# =============================================================================

cat("\nCreating CATE vs baseline risk plot...\n")

if (!is.null(results$cate_individual) && !is.null(results$Y.hat)) {
  cate_ind <- results$cate_individual
  y_hat <- results$Y.hat

  cate_risk_df <- data.frame(
    baseline_risk = y_hat * 100,       # percentage points
    cate = cate_ind * 100              # percentage points
  )

  p_cate_risk <- ggplot(cate_risk_df, aes(x = baseline_risk, y = cate)) +
    geom_point(alpha = 0.05, size = 0.3, color = "steelblue") +
    geom_smooth(method = "loess", color = "red", linewidth = 0.8, se = FALSE) +
    geom_hline(yintercept = mean(cate_ind, na.rm = TRUE) * 100,
               linetype = "dashed", color = "grey40") +
    labs(
      title = "Treatment Effect Heterogeneity by Baseline Risk",
      subtitle = "Individual CATE vs predicted outcome risk (from raw causal forest)",
      x = "Baseline risk (predicted outcome probability, pp)",
      y = "CATE (percentage points)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold")
    )

  ggsave(
    file.path(output_dir, "cate_vs_baseline_risk.pdf"),
    p_cate_risk, width = 10, height = 6
  )
  cat("Saved: cate_vs_baseline_risk.pdf\n")
} else {
  cat("Missing cate_individual or Y.hat — skipping CATE vs baseline risk plot.\n")
}

# =============================================================================
# PLOT 7: VOTED-STRUCTURE DISTRIBUTION PER DEPTH
# =============================================================================
# For each candidate depth, show the top-K most-frequent voted tree structures
# across iCF iterations. Replaces the previous depth-attrition diagnostic
# (depth_distribution_D{2..5}.pdf), which was uninformative when the
# min_leaf_size heuristic hit its target depth in 100% of iterations.
# This figure instead shows STRUCTURAL fragmentation: at D2 the algorithm
# picks one tree unanimously; at D4 it splits across many distinct structures.

cat("\nCreating voted-structure distribution plot...\n")

if (!requireNamespace("patchwork", quietly = TRUE)) {
  cat("patchwork not available - skipping structure-distribution plot\n")
} else {
  library(patchwork)

  TOP_K <- 5

  short_labels <- c(
    female = "Sex", age_cat = "Age", edufam_cat = "Education",
    inc_cat = "Income", fh_depr = "FH-Depr", fh_suicidal = "FH-Suicidal",
    source = "Care setting", hosp = "Prior psych hosp",
    diag_suicidal = "Prior suicidal", diag_overdose = "Prior overdose",
    diag_alcohol = "Prior alcohol", diag_sud = "Prior SUD",
    diag_stress = "Prior stress", diag_anxiety_other = "Prior anxiety",
    diag_mdd = "Prior MDD", med_hypnotic = "Prior hypnotic",
    med_benzodiazepine = "Prior benzo", med_antipsychotic = "Prior antipsych"
  )

  relabel_structure <- function(s) {
    parts <- strsplit(s, "\\|")[[1]]
    out <- vapply(parts, function(p) {
      m <- regmatches(p, regexec("^(D\\d+):(.+)$", p))[[1]]
      if (length(m) == 3) {
        v <- m[3]
        friendly <- if (v %in% names(short_labels)) short_labels[[v]] else v
        paste0(m[2], ":", friendly)
      } else p
    }, character(1), USE.NAMES = FALSE)
    paste(out, collapse = " | ")
  }

  make_structure_panel <- function(dname, vote_df) {
    df <- head(vote_df, TOP_K)
    df$structure_pretty <- vapply(as.character(df$structure), relabel_structure,
                                  character(1), USE.NAMES = FALSE)
    df$structure_pretty <- factor(df$structure_pretty,
                                  levels = rev(df$structure_pretty))
    df$is_top <- df$frequency == max(df$frequency)

    total_iter <- sum(vote_df$count)
    n_distinct <- nrow(vote_df)

    ggplot(df, aes(x = frequency, y = structure_pretty, fill = is_top)) +
      geom_col() +
      scale_fill_manual(
        values = c(`TRUE` = "steelblue", `FALSE` = "gray70"),
        guide = "none"
      ) +
      geom_text(
        aes(label = sprintf("%.0f%%", 100 * frequency)),
        hjust = -0.2, size = 3
      ) +
      scale_x_continuous(
        labels = scales::percent_format(),
        limits = c(0, 1.15),
        expand = expansion(mult = c(0, 0.02))
      ) +
      labs(
        title = sprintf("%s (%d iterations, %d distinct structure%s)",
                        dname, total_iter, n_distinct,
                        ifelse(n_distinct == 1, "", "s")),
        x = "Frequency",
        y = NULL
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size = 8)
      )
  }

  panels <- list()
  for (dname in names(results$all_depth_results)) {
    vd <- results$all_depth_results[[dname]]$final_result$vote_distribution
    if (!is.null(vd) && nrow(vd) > 0) {
      panels[[dname]] <- make_structure_panel(dname, vd)
    }
  }

  if (length(panels) > 0) {
    p_structure <- Reduce(`/`, panels) +
      plot_annotation(
        title = "Voted-tree structure distribution by candidate depth",
        subtitle = "Top 5 most frequent tree structures per depth. Voted (most popular) structure in blue.",
        theme = theme(
          plot.title = element_text(size = 14, face = "bold"),
          plot.subtitle = element_text(size = 10)
        )
      )

    ggsave(
      file.path(output_dir, "structure_distribution.pdf"),
      p_structure, width = 7.5, height = 10
    )
    cat("Saved: structure_distribution.pdf\n")
  }
}

# =============================================================================
# SUMMARY REPORT
# =============================================================================

cat("\n==============================================\n")
cat("VISUALIZATION COMPLETE\n")
cat("==============================================\n")
cat("\nOutput files saved to:", output_dir, "\n")
cat("\nGenerated files:\n")
list.files(output_dir, pattern = "\\.(pdf|txt|csv)$") %>%
  paste(" -", .) %>%
  cat(sep = "\n")
