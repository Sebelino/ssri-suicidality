# Makefile for thesis compilation

MAIN = thesis
LATEX = pdflatex
BIBTEX = bibtex

# Default source directories (overridden by config.mk if present).
# The three output dirs point at the 2026-05-22 backup snapshots so that
# in-flight reruns (iCF/hdiCF step-1 missind triage, analysis script reruns)
# cannot silently change the figures/values that ship with the thesis.
# Reconfigure with ./configure --icf-dir DIR --analysis-dir DIR to point at
# live outputs when ready to refresh.
ANALYSIS_DIR = ../analysis
ANALYSIS_OUTPUT_DIR = ../analysis/gitignore/output-backup-2026-05-22
ICF_DIR = ../analysis-icf/gitignore/output-backup-2026-05-22
HDICF_DIR = ../analysis-hdicf/gitignore/output-backup-2026-05-22
DOCUMENTS_DIR = ../Documents

-include config.mk

# Local directories for copied files
FIGURES_DIR = figures
GENERATED_DIR = generated

# Files from main analysis
ANALYSIS_FIGURES = survplot_itt_main.pdf survplot_age_strata.pdf \
                   predi_diff_distribution.pdf

# Files from Documents
DOCUMENT_FIGURES = flowchart_cohort_inclusion.pdf flowchart_cohort_inclusion_bw.pdf

# Files from iCF analysis
ICF_FIGURES = variable_importance.pdf decision_tree.pdf \
              decision_tree_D2.pdf decision_tree_D3.pdf \
              decision_tree_D4.pdf decision_tree_D5.pdf \
              depth_distribution_D2.pdf depth_distribution_D3.pdf \
              depth_distribution_D4.pdf depth_distribution_D5.pdf \
              cate_forest_plot.pdf \
              cate_forest_plot_D2.pdf cate_forest_plot_D3.pdf \
              cate_forest_plot_D4.pdf cate_forest_plot_D5.pdf \
              crude_event_rates_by_subgroup.pdf \
              cate_distribution.pdf cate_distribution_histogram.pdf \
              cate_vs_baseline_risk.pdf \
              calibration_plot.pdf structure_distribution.pdf \
              cate_vi_comparison.pdf cate_distribution_comparison.pdf
ICF_TEX = icf_values.tex icf_varimp_list.tex icf_subgroups_table.tex icf_vi_stability_table.tex thesis_values.tex baseline_table.tex excluded_baseline_table.tex validation_values.tex calibration_values.tex missind_triage_values.tex

# Files from hdiCF analysis (renamed with _hdicf suffix)
HDICF_FIGURES_SRC = decision_tree.pdf variable_importance.pdf cate_forest_plot.pdf cate_forest_plot_D2.pdf cate_forest_plot_D3.pdf cate_forest_plot_D4.pdf cate_forest_plot_D5.pdf depth_distribution_D2.pdf depth_distribution_D3.pdf depth_distribution_D4.pdf depth_distribution_D5.pdf decision_tree_D2.pdf decision_tree_D3.pdf decision_tree_D4.pdf decision_tree_D5.pdf structure_distribution.pdf
HDICF_FIGURES = $(patsubst %.pdf,%_hdicf.pdf,$(HDICF_FIGURES_SRC))
HDICF_TEX = hdicf_values.tex hdicf_varimp_list.tex hdicf_subgroups_table.tex hd_features_table.tex

# Files from analysis/output (Qini curves, supplementary)
ANALYSIS_OUTPUT_FIGURES = qini_12wks.pdf qini_12wks_hdicf.pdf

# Generated tex from analysis/output
ANALYSIS_OUTPUT_TEX = cate_by_source_values.tex qini_values.tex qini_values_hdicf.tex missingness_patterns.tex missind_values.tex stratified_prior_suicidal_values.tex

# Local copies of all figures and generated tex
LOCAL_FIGURES = $(addprefix $(FIGURES_DIR)/,$(ANALYSIS_FIGURES) $(DOCUMENT_FIGURES) $(ICF_FIGURES) $(HDICF_FIGURES) $(ANALYSIS_OUTPUT_FIGURES))
LOCAL_TEX = $(addprefix $(GENERATED_DIR)/,$(ICF_TEX) $(HDICF_TEX) $(ANALYSIS_OUTPUT_TEX))

.PHONY: all press_release clean view

all: $(MAIN).pdf

press_release: press_release.pdf

# Copy individual analysis figures
$(FIGURES_DIR)/%: $(ANALYSIS_DIR)/% | $(FIGURES_DIR)
	cp $< $@

# Copy individual document figures
$(FIGURES_DIR)/%: $(DOCUMENTS_DIR)/% | $(FIGURES_DIR)
	cp $< $@

# Copy individual iCF figures
$(FIGURES_DIR)/%: $(ICF_DIR)/% | $(FIGURES_DIR)
	cp $< $@

# Copy analysis output figures (Qini)
$(FIGURES_DIR)/%: $(ANALYSIS_OUTPUT_DIR)/% | $(FIGURES_DIR)
	cp $< $@

# Copy hdiCF figures (renamed with _hdicf suffix)
$(FIGURES_DIR)/%_hdicf.pdf: $(HDICF_DIR)/%.pdf | $(FIGURES_DIR)
	cp $< $@

# Copy hdiCF generated tex files
$(GENERATED_DIR)/%: $(HDICF_DIR)/% | $(GENERATED_DIR)
	cp $< $@

# Copy individual generated tex files
$(GENERATED_DIR)/%: $(ICF_DIR)/% | $(GENERATED_DIR)
	cp $< $@

# Copy generated tex from analysis/output
$(GENERATED_DIR)/%: $(ANALYSIS_OUTPUT_DIR)/% | $(GENERATED_DIR)
	cp $< $@

# Create output directories
$(FIGURES_DIR) $(GENERATED_DIR):
	mkdir -p $@

$(MAIN).pdf: $(MAIN).tex references.bib $(LOCAL_FIGURES) $(LOCAL_TEX)
	$(LATEX) $(MAIN)
	$(BIBTEX) $(MAIN)
	$(LATEX) $(MAIN)
	$(LATEX) $(MAIN)

# Press release: depends only on the few generated/*.tex macro files it
# \input{}s. No bibliography, so a single pdflatex pass is enough.
press_release.pdf: press_release.tex $(GENERATED_DIR)/thesis_values.tex $(GENERATED_DIR)/icf_values.tex
	$(LATEX) press_release

clean:
	rm -f $(MAIN).aux $(MAIN).bbl $(MAIN).blg $(MAIN).log $(MAIN).out $(MAIN).toc $(MAIN).pdf
	rm -f press_release.aux press_release.log press_release.out press_release.pdf
	rm -rf $(FIGURES_DIR) $(GENERATED_DIR)

view: $(MAIN).pdf
	open $(MAIN).pdf
