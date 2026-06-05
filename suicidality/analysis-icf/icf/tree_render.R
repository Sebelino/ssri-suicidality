# tree_render.R
# Compact partykit-based decision tree rendering for iCF and hdiCF output.
#
# Shared between analysis-icf/03_visualize_results.R and
# analysis-hdicf/04_visualize_results.R. The earlier ggplot2-based renderer
# (build_tree_layout + render_tree + save_tree) produced large multi-line
# leaf cards that dominated the page; this module renders the same tree
# structure via partykit::plot.party() with small rounded-rectangle nodes.

if (!requireNamespace("partykit", quietly = TRUE)) {
  install.packages("partykit", repos = "https://cloud.r-project.org")
}
suppressPackageStartupMessages({
  library(partykit)
  library(grid)
})

#' Build a partykit `party` object from an iCF synthetic-splits data frame
#'
#' @param splits Data frame produced by `core.R::build_synthetic_tree` with
#'   columns `node_id, depth, variable, var_idx, split_value, left_child,
#'   right_child`. Leaves are node ids that don't appear in `node_id`.
#' @param cate_df Data frame of leaf-level CATE estimates (the `cate` slot
#'   of results / all_depth_results[[Dx]]).
#' @param var_names Character vector of covariate names corresponding to
#'   the split variable column.
#' @return A `party` object whose terminal-node info slots carry
#'   `sg_id, cate, ci_lo, ci_hi, n_treated, n_control, events_treated, events_control`.
build_party_from_splits <- function(splits, cate_df, var_names) {
  # Build a placeholder data frame with one row per variable to satisfy
  # partykit's data-validation step. The actual values aren't used in
  # rendering -- partysplit only looks up variable index via varid.
  data <- as.data.frame(matrix(0, nrow = 1, ncol = length(var_names)))
  names(data) <- var_names

  next_id <- 0L
  get_next_id <- function() { next_id <<- next_id + 1L; next_id }

  build_node <- function(node_id, path) {
    sr <- splits[splits$node_id == node_id, , drop = FALSE]

    if (nrow(sr) == 0L) {
      row <- cate_df[cate_df$label == path, , drop = FALSE]
      info <- if (nrow(row) == 1L) list(
        sg_id          = row$subgroup_id[1],
        label_path     = row$label[1],
        cate           = row$iptw_cate[1],
        ci_lo          = if ("ci_lower" %in% names(row)) row$ci_lower[1] else NA_real_,
        ci_hi          = if ("ci_upper" %in% names(row)) row$ci_upper[1] else NA_real_,
        n_treated      = if ("n_treated" %in% names(row)) row$n_treated[1] else NA_integer_,
        n_control      = if ("n_control" %in% names(row)) row$n_control[1] else NA_integer_,
        events_treated = if ("events_treated" %in% names(row)) row$events_treated[1] else NA_integer_,
        events_control = if ("events_control" %in% names(row)) row$events_control[1] else NA_integer_
      ) else list(label_path = path)
      return(partynode(id = get_next_id(), info = info))
    }

    var_name  <- sr$variable[1]
    split_val <- sr$split_value[1]
    var_idx   <- which(var_names == var_name)
    if (length(var_idx) != 1L) {
      stop(sprintf("Variable %s not found in var_names (or duplicated).", var_name))
    }

    # Attach human-readable edge labels in the split's info so the renderer
    # can replace partykit's default "≤ x / > x" with e.g. "Male / Female".
    el <- edge_labels_for(var_name, split_val)
    sp <- partysplit(
      varid  = as.integer(var_idx),
      breaks = split_val,
      info   = list(var_name = var_name, split_value = split_val,
                    edge_left = el$left, edge_right = el$right)
    )
    me_id <- get_next_id()

    left_path  <- paste0(path, "_", var_name, "<=", round(split_val, 2))
    right_path <- paste0(path, "_", var_name, ">",  round(split_val, 2))

    partynode(
      id    = me_id,
      split = sp,
      kids  = list(build_node(sr$left_child[1], left_path),
                   build_node(sr$right_child[1], right_path)),
      info  = list(var_name = var_name, split_value = split_val)
    )
  }

  tree <- build_node(1L, "SG")
  party(tree, data = data)
}

#' Topic-style edge labels for a split
#'
#' Returns the text shown on the left and right branch out of an inner node.
#' For binary clinical indicators these are "Male / Female", "No / Yes" etc.;
#' for ordinal / continuous splits they default to "≤ v / > v". The inner
#' node itself shows the topic question (`default_split_label` below), so the
#' edge label and the node label together read e.g. "Sex? → Male" / "Female".
edge_labels_for <- function(var_name, split_val) {
  # Pure 0/1 indicators with no missing-value sentinel (derived from medical
  # records: missing = "no record" = 0). For these, "No / Yes" is exact.
  binary_pure <- c("hosp", "diag_mdd", "diag_bipolar", "diag_psychotic",
                   "diag_alcohol", "diag_sud", "diag_suicidal", "diag_overdose",
                   "diag_stress", "diag_anxiety_other", "diag_phobic",
                   "diag_sleep", "diag_anorexia", "diag_bulimia",
                   "diag_ocd", "diag_conduct", "diag_intellectual_disability",
                   "diag_personality_cluster_b", "diag_adhd", "diag_autism",
                   "med_antipsychotic", "med_hypnotic", "med_benzodiazepine",
                   "med_antiepileptic", "med_stimulant", "med_opioid",
                   "med_mood_stabilizer", "med_addiction")
  # The analysis cohort is complete-case (no sentinel-9 missing values for
  # inc_cat / edufam_cat / fh_depr / fh_suicidal), so all ordinal splits are
  # between legitimate category boundaries. Older /NA-suffixed labels are
  # retained as the >sentinel branches in case a stale tree is rendered, but
  # they should never fire on the production cohort.

  if (var_name == "female" && split_val < 1) {
    return(list(left = "Male", right = "Female"))
  }
  if (var_name %in% binary_pure && split_val < 1) {
    return(list(left = "No", right = "Yes"))
  }
  if (var_name == "age_cat") {
    if (split_val < 0.5)      return(list(left = "6–11",  right = "12–24"))
    else if (split_val < 1.5) return(list(left = "6–17",  right = "18–24"))
    else                      return(list(left = "≤ 17",  right = "Adult"))
  }
  # Family-income quintiles: 1=negative, 2=zero, 3=1st–20th pct,
  # 4=20th–80th pct, 5=top 20 %. (No sentinel-9: complete-case cohort.)
  if (var_name == "inc_cat") {
    if (split_val > 5.5)      return(list(left = "Recorded", right = "Missing"))
    else if (split_val < 3.5) return(list(left = "Bottom 20%",     right = "Above 20th pct"))
    else if (split_val < 4.5) return(list(left = "Below top 20%",  right = "Top 20%"))
  }
  # Parental education: 0=primary, 1=secondary, 2=post-secondary.
  if (var_name == "edufam_cat") {
    if (split_val > 2.5)      return(list(left = "Recorded", right = "Missing"))
    else if (split_val < 0.5) return(list(left = "Primary",     right = "Above primary"))
    else if (split_val < 1.5) return(list(left = "≤ Secondary", right = "> Secondary"))
  }
  # Family history of depression / suicidality (binary 0/1; complete-case).
  if (var_name %in% c("fh_depr", "fh_suicidal")) {
    if (split_val > 1.5)      return(list(left = "Recorded", right = "Missing"))
    else if (split_val < 0.5) return(list(left = "No", right = "Yes"))
    else                      return(list(left = "Non-missing", right = "Missing"))
  }
  list(left  = sprintf("≤ %g", split_val),
       right = sprintf("> %g", split_val))
}

#' Topic-style split-label formatter
#'
#' Inner nodes show just the topic being asked about (e.g. "Sex?",
#' "Income quintile?", "Parental education?"); the specific answer
#' selecting each child branch is shown on the edge instead via
#' `edge_labels_for()`. The previous "Male? ≤ 0 / > 0" style was
#' confusing because the inner-node question and the branch labels
#' were redundant for binary indicators.
default_split_label <- function(var_name, split_val) {
  topics <- c(
    female                       = "Sex",
    age_cat                      = "Age group",
    edufam_cat                   = "Parental education",
    inc_cat                      = "Income quintile",
    fh_depr                      = "Family history of depression",
    fh_suicidal                  = "Family history of suicidality",
    source                       = "Care setting",
    hosp                         = "Prior psychiatric hospitalisation",
    diag_mdd                     = "Prior MDD",
    diag_bipolar                 = "Prior bipolar disorder",
    diag_psychotic               = "Prior psychotic disorder",
    diag_alcohol                 = "Prior alcohol use disorder",
    diag_sud                     = "Prior substance use disorder",
    diag_suicidal                = "Prior suicidal behavior",
    diag_overdose                = "Prior overdose",
    diag_stress                  = "Prior stress disorder",
    diag_anxiety_other           = "Prior anxiety disorder",
    diag_phobic                  = "Prior phobic disorder",
    diag_sleep                   = "Prior sleep disorder",
    diag_organic                 = "Prior organic disorder",
    diag_anorexia                = "Prior anorexia",
    diag_bulimia                 = "Prior bulimia",
    diag_ocd                     = "Prior OCD",
    diag_conduct                 = "Prior conduct disorder",
    diag_intellectual_disability = "Prior intellectual disability",
    diag_personality_cluster_b   = "Prior cluster B personality disorder",
    diag_adhd                    = "Prior ADHD",
    diag_autism                  = "Prior autism",
    med_antipsychotic            = "Prior antipsychotic use",
    med_hypnotic                 = "Prior hypnotic use",
    med_benzodiazepine           = "Prior benzodiazepine use",
    med_antiepileptic            = "Prior antiepileptic use",
    med_stimulant                = "Prior stimulant use",
    med_opioid                   = "Prior opioid use",
    med_mood_stabilizer          = "Prior mood stabilizer use",
    med_addiction                = "Prior addiction medication"
  )
  if (var_name %in% names(topics)) return(topics[[var_name]])
  var_name
}

# Legacy formatter retained for the decision_tree_labels.json mapping.
legacy_split_label <- function(var_name, split_val) {
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
  categorical_labels <- list(
    age_cat = function(sv) {
      if (sv < 0.5) "Child (6-11)?"
      else if (sv < 1.5) "Child/Adolescent (≤17)?"
      else "Child/Adolescent/Young adult?"
    },
    edufam_cat = function(sv) {
      if (sv > 2.5) "Parental education recorded (vs. missing)?"
      else sprintf("Parental education ≤ %g?", sv)
    },
    inc_cat = function(sv) {
      if (sv > 5.5) "Income quintile recorded (vs. missing)?"
      else sprintf("Income quintile ≤ %g?", sv)
    },
    fh_suicidal = function(sv) {
      if (sv > 1.5) "Family history of suicidality recorded (vs. missing)?"
      else if (sv < 0.5) "No family history of suicidality?"
      else "Family history of suicidality known?"
    },
    fh_depr = function(sv) {
      if (sv > 1.5) "Family history of depression recorded (vs. missing)?"
      else if (sv < 0.5) "No family history of depression?"
      else "Family history of depression known?"
    },
    source = function(sv) sprintf("Care setting ≤ %g?", sv)
  )
  if (var_name %in% names(binary_labels) && split_val < 1) {
    return(binary_labels[[var_name]])
  }
  if (var_name %in% names(categorical_labels)) {
    return(categorical_labels[[var_name]](split_val))
  }
  paste0(var_name, " ≤ ", split_val)
}

#' Render an iCF synthetic tree to a compact PDF via partykit
#'
#' @param splits  iCF synthetic-splits data frame (or NULL → no-op).
#' @param cate_df Leaf-level CATE data frame.
#' @param var_names Covariate names matching split$variable values.
#' @param out_path Destination PDF path.
#' @param label_fn Function `(var_name, split_val) -> character` for inner
#'   nodes. Defaults to `default_split_label` (the iCF pipeline's pretty
#'   labels). Pass a raw-label function (e.g. `paste0(var, " <= ", val)`)
#'   for the `decision_tree_raw.pdf` variant.
#' @param width,height PDF dimensions in inches. Auto-scaled if NULL.
#' @param fontsize_inner Font size (pt) for inner-node labels (default 12).
#' @param fontsize_leaf  Font size (pt) for terminal-node text (default 11).
#' @param fontsize_edge  Font size (pt) for branch labels (default 11).
save_partykit_tree <- function(splits, cate_df, var_names, out_path,
                               label_fn = default_split_label,
                               width = NULL, height = NULL,
                               fontsize_inner = 14,
                               fontsize_leaf  = 12,
                               fontsize_edge  = 13) {
  if (is.null(splits) || nrow(splits) == 0L) {
    message("save_partykit_tree: empty splits; skipping ", basename(out_path))
    return(invisible(FALSE))
  }
  if (is.null(cate_df) || nrow(cate_df) == 0L) {
    message("save_partykit_tree: empty cate_df; skipping ", basename(out_path))
    return(invisible(FALSE))
  }

  py <- build_party_from_splits(splits, cate_df, var_names)
  n_leaves <- length(nodeids(py, terminal = TRUE))
  tree_depth <- depth(py)
  # Each leaf card needs ~2.5 in for the widest line ("Size: NN,NNN vs. MM,MMM");
  # leaves take 3x the height of an inner node (tnex below). Auto-size so the
  # final cropped PDF stays roughly proportional to the tree content.
  #
  # The per-leaf width must accommodate the wider of the fonts cairo_pdf may
  # fall back to. macOS resolves "sans" -> Helvetica (narrow), Linux without
  # Microsoft/Apple fonts resolves "sans" -> DejaVu Sans (~15% wider). 2.5 in
  # leaves a small visible gap between leaf cards on both platforms without
  # the wasted whitespace a wider slot would introduce.
  if (is.null(width))  width  <- n_leaves * 2.5 + 0.5
  if (is.null(height)) height <- tree_depth * 0.7 + 2.2

  # Leaf-card text mirrors the prior ggplot2 renderer (size + events + aRD).
  format_leaf_text <- function(inf) {
    lines <- character()
    if (!is.null(inf$sg_id) && !is.na(inf$sg_id)) {
      lines <- c(lines, sprintf("SG%s", inf$sg_id))
    }
    if (!is.null(inf$n_treated) && !is.na(inf$n_treated)) {
      lines <- c(lines, sprintf("Size: %s vs. %s",
                                format(inf$n_treated, big.mark = ","),
                                format(inf$n_control, big.mark = ",")))
    }
    if (!is.null(inf$events_treated) && !is.na(inf$events_treated)) {
      lines <- c(lines, sprintf("Events: %s vs. %s",
                                format(inf$events_treated, big.mark = ","),
                                format(inf$events_control, big.mark = ",")))
    }
    if (!is.null(inf$cate) && !is.na(inf$cate)) {
      point <- if (inf$cate >= 0) sprintf("+%.2f", inf$cate) else sprintf("%.2f", inf$cate)
      if (!is.null(inf$ci_lo) && !is.na(inf$ci_lo) && !is.na(inf$ci_hi)) {
        lines <- c(lines, sprintf("aRD: %s (%.2f, %.2f) pp",
                                  point, inf$ci_lo, inf$ci_hi))
      } else {
        lines <- c(lines, sprintf("aRD: %s pp", point))
      }
    }
    paste(lines, collapse = "\n")
  }

  # Draw a rounded box sized to a textGrob with configurable inner padding
  # (extra horizontal / vertical space, in points, distributed evenly on
  # both sides of the text).
  draw_text_box <- function(text, fill, fontsize, pad_x = 12, pad_y = 8) {
    g <- textGrob(text, gp = gpar(fontsize = fontsize))
    grid.roundrect(width  = unit(pad_x, "pt") + grobWidth(g),
                   height = unit(pad_y, "pt") + grobHeight(g),
                   gp = gpar(fill = fill, col = "black", lwd = 1))
    grid.draw(g)
  }

  inner_panel <- function(party_obj, ...) {
    var_names_local <- names(party_obj$data)
    function(node) {
      sp <- split_node(node)
      v  <- var_names_local[varid_split(sp)]
      br <- breaks_split(sp)
      # Inner nodes get a touch more padding than leaves so the single-line
      # question reads as airier than the multi-line leaf cards.
      draw_text_box(label_fn(v, br), fill = "lightblue",
                    fontsize = fontsize_inner, pad_x = 18, pad_y = 12)
    }
  }
  class(inner_panel) <- "grapcon_generator"

  terminal_panel <- function(party_obj, ...) {
    function(node) {
      inf <- info_node(node)
      draw_text_box(format_leaf_text(inf), fill = "lightgreen",
                    fontsize = fontsize_leaf, pad_x = 12, pad_y = 8)
    }
  }
  class(terminal_panel) <- "grapcon_generator"

  # Custom edge labels: reuse the human-readable left/right strings the
  # build_party_from_splits step attached to each partysplit's info slot.
  edge_panel <- function(party_obj, ...) {
    function(node, i) {
      sp <- split_node(node)
      inf <- info_split(sp)
      label <- if (i == 1L) inf$edge_left else inf$edge_right
      if (is.null(label) || !nzchar(label)) return(invisible(NULL))
      grid.rect(width  = grobWidth(textGrob(label, gp = gpar(fontsize = fontsize_edge))) + unit(2, "pt"),
                height = unit(1.2, "lines"),
                gp = gpar(fill = "white", col = NA))
      grid.text(label, gp = gpar(fontsize = fontsize_edge))
    }
  }
  class(edge_panel) <- "grapcon_generator"

  cairo_pdf(out_path, width = width, height = height)
  plot(py,
       inner_panel    = inner_panel,
       terminal_panel = terminal_panel,
       edge_panel     = edge_panel,
       drop_terminal  = TRUE,
       tnex           = 3,                # taller leaves for 4-line cards
       margins        = c(1, 1, 1, 1),
       gp             = gpar(fontsize = 10))
  dev.off()

  if (nzchar(Sys.which("pdfcrop"))) {
    system2("pdfcrop", c(out_path, out_path), stdout = FALSE)
  }
  invisible(TRUE)
}
