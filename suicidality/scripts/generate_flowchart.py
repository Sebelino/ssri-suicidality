#!/usr/bin/env python3
"""
Generate cohort inclusion flow chart for the suicidality study.

This script generates a PNG image showing the cohort inclusion/exclusion
flow following the CONSORT-style format used in academic papers.

Usage:
    python generate_flowchart.py [--output-path PATH] [--rds-dir PATH]

Requirements:
    - graphviz (mamba install -n thesis graphviz)
"""

import argparse
import textwrap
from pathlib import Path

from graphviz import Digraph


# Cohort flow definition
COHORT_FLOW = [
    {
        "text": "{n:,} individuals received a first recorded depression diagnosis (ICD-10: F32, F33, excluding F33.4) at ages 6 to 24 years from inpatient or outpatient care, 1st July 2006 to 31st December 2019",
        "n_var": "n_after_age_filter",
        "exclusion": {
            "text": "{n:,} were excluded",
            "n_var": "n_excluded_washout",
            "bullets": [
                "{n:,} received a first recorded depression diagnosis when they had dispensed an antidepressant medication (ATC=N06A) on any of the 364 days preceding the diagnosis date",
            ],
            "bullet_vars": ["n_excluded_washout"],
        },
    },
    {
        "text": "{n:,} received a first recorded depression diagnosis with no antidepressant (ATC=N06A) dispensation on any of the 364 days preceding the diagnosis date",
        "n_var": "n_after_washout",
        "exclusion": {
            "text": "{n:,} were excluded",
            "n_var": "n_excluded_prefu",
            "bullets": [
                "{n1:,} emigrated between depression diagnosis and follow-up start",
                "{n2:,} died between depression diagnosis and follow-up start",
            ],
            "bullet_vars": ["n_emigrated_prefu", "n_died_prefu"],
        },
    },
    {
        "text": "{n:,} received a first recorded depression diagnosis with no antidepressant (ATC=N06A) dispensation on any of the 364 days preceding the diagnosis date, and did not die or emigrate before follow-up start",
        "n_var": "n_analysis_cohort_raw",
        "exclusion": {
            "text": "{n:,} were excluded",
            "n_var": "n_excluded_missing_cov",
            "bullets": [
                "{n:,} had missing parental education, family income, or family-history covariates (complete-case analysis)",
            ],
            "bullet_vars": ["n_excluded_missing_cov"],
        },
    },
    {
        "text": "{n:,} were included in the complete-case analysis cohort (no missing parental education, family income, or family history)",
        "n_var": "n_analysis_cohort",
    },
]

TREATMENT_SPLIT = {
    "initiators": {
        "text": "{n:,} initiated SSRI treatment (ATC=N06AB) within 28 days after their first recorded depression diagnosis",
        "n_var": "n_initiators",
    },
    "non_initiators": {
        "text": "{n:,} did not initiate SSRI treatment within 28 days after their first recorded depression diagnosis",
        "n_var": "n_non_initiators",
    },
}


def read_rds_numbers(rds_dir: Path) -> dict:
    """Read cohort numbers directly from RDS files using R."""
    import subprocess

    numbers = {}

    if not rds_dir or not rds_dir.exists():
        print(f"RDS directory not found: {rds_dir}")
        return numbers

    print(f"Reading RDS files from: {rds_dir}")

    r_script = f'''
    rds_dir <- "{rds_dir}"
    `%||%` <- function(a, b) if (!is.null(a)) a else b

    safe_read <- function(file) {{
        path <- file.path(rds_dir, file)
        if (file.exists(path)) readRDS(path) else NULL
    }}

    summary <- safe_read("cohort_flow_summary.rds")
    if (!is.null(summary)) {{
        # Map variable names from summary to what the flowchart expects
        n_initial <- summary$n_unique_initial %||% summary$n_initial_diagnoses
        if (!is.null(n_initial))
            cat("n_initial_diagnoses=", n_initial, "\\n", sep="")
        if (!is.null(summary$n_after_age_filter))
            cat("n_after_age_filter=", summary$n_after_age_filter, "\\n", sep="")
    }}

    # Compute age exclusion breakdown (young vs old)
    raw_diag <- safe_read("raw_diagnoses_index.rds")
    raw_indiv <- safe_read("raw_individual.rds")
    if (!is.null(raw_diag) && !is.null(raw_indiv) && !is.null(summary$n_after_age_filter)) {{
        # Compute age at each diagnosis
        diag_age <- merge(raw_diag, raw_indiv[, c("lopnr", "bdate")], by = "lopnr")
        diag_age$year_diff <- as.integer(format(diag_age$diagn_date, "%Y")) -
                              as.integer(format(diag_age$bdate, "%Y"))
        diag_age$bday_occurred <- format(diag_age$diagn_date, "%m%d") >=
                                  format(diag_age$bdate, "%m%d")
        diag_age$age <- as.integer(diag_age$year_diff - ifelse(diag_age$bday_occurred, 0L, 1L))

        # Individuals included: have at least one diagnosis at age 6-24
        included <- unique(diag_age$lopnr[diag_age$age >= 6 & diag_age$age <= 24])

        # Excluded individuals
        excluded_diag <- diag_age[!(diag_age$lopnr %in% included), ]

        # Per excluded individual: max and min age
        max_age <- aggregate(age ~ lopnr, data = excluded_diag, FUN = max)
        min_age <- aggregate(age ~ lopnr, data = excluded_diag, FUN = min)

        n_young <- sum(max_age$age < 6)
        n_old <- sum(min_age$age > 24)

        cat("n_excluded_age_young=", n_young, "\\n", sep="")
        cat("n_excluded_age_old=", n_old, "\\n", sep="")
    }}

    if (!is.null(summary$n_eligible_after_washout)) {{
        cat("n_after_washout=", summary$n_eligible_after_washout, "\\n", sep="")
    }}

    base <- safe_read("base_28.rds")
    main <- safe_read("main_12wks_28.rds")

    if (!is.null(main) && "cc" %in% names(main)) {{
        # Raw analysis cohort (pre-CCA: after excluding deaths during initiation window)
        cat("n_analysis_cohort_raw=", nrow(main), "\\n", sep="")

        # Complete-case cohort: drop rows with sentinel-99 in any of the four
        # partially-observed covariates.
        cca_miss <- (main$edufam_cat == 99) | (main$inc_cat == 99) |
                    (main$fh_suicidal == 99) | (main$fh_depr == 99)
        main_cca <- main[!cca_miss, , drop = FALSE]
        cat("n_excluded_missing_cov=", sum(cca_miss), "\\n", sep="")
        cat("n_analysis_cohort=", nrow(main_cca), "\\n", sep="")
        cat("n_initiators=", sum(main_cca$cc == 1, na.rm = TRUE), "\\n", sep="")
        cat("n_non_initiators=", sum(main_cca$cc == 0, na.rm = TRUE), "\\n", sep="")

        # Calculate exclusions between base and main (died/emigrated during initiation window)
        if (!is.null(base)) {{
            missing_lopnrs <- setdiff(base$lopnr, main$lopnr)
            if (length(missing_lopnrs) > 0) {{
                missing <- base[base$lopnr %in% missing_lopnrs, ]
                n_died <- sum(!is.na(missing$date_death))
                n_emig <- sum(!is.na(missing$date_emig) & is.na(missing$date_death))
                cat("n_died_prefu=", n_died, "\\n", sep="")
                cat("n_emigrated_prefu=", n_emig, "\\n", sep="")
            }} else {{
                cat("n_died_prefu=0\\n")
                cat("n_emigrated_prefu=0\\n")
            }}
        }}

        # SSRI type breakdown among initiators in the final (CCA) cohort
        cb <- safe_read("cohort_base.rds")
        if (!is.null(cb) && "atc" %in% names(cb)) {{
            initiator_lopnrs <- main_cca$lopnr[main_cca$cc == 1]
            init_cb <- cb[cb$lopnr %in% initiator_lopnrs & !is.na(cb$atc), ]
            atc5 <- substr(init_cb$atc, 1, 7)
            ssri_map <- c(N06AB04="n_citalopram", N06AB10="n_escitalopram",
                          N06AB03="n_fluoxetine", N06AB08="n_fluvoxamine",
                          N06AB05="n_paroxetine", N06AB06="n_sertraline")
            counts <- table(atc5)
            for (code in names(ssri_map)) {{
                n <- if (code %in% names(counts)) as.integer(counts[code]) else 0L
                cat(ssri_map[code], "=", n, "\\n", sep="")
            }}
        }}
    }}
    '''

    try:
        result = subprocess.run(
            ['Rscript', '-e', r_script],
            capture_output=True,
            text=True,
            timeout=60
        )

        if result.returncode != 0:
            print(f"  R script error: {result.stderr}")
            return numbers

        for line in result.stdout.strip().split('\n'):
            if '=' in line:
                key, value = line.split('=', 1)
                try:
                    numbers[key.strip()] = int(value.strip())
                except ValueError:
                    pass

        if 'n_initial_diagnoses' in numbers and 'n_after_age_filter' in numbers:
            numbers['n_excluded_age'] = numbers['n_initial_diagnoses'] - numbers['n_after_age_filter']

        if 'n_after_age_filter' in numbers and 'n_after_washout' in numbers:
            numbers['n_excluded_washout'] = numbers['n_after_age_filter'] - numbers['n_after_washout']

        if 'n_after_washout' in numbers and 'n_analysis_cohort_raw' in numbers:
            numbers['n_excluded_prefu'] = numbers['n_after_washout'] - numbers['n_analysis_cohort_raw']

    except subprocess.TimeoutExpired:
        print("  R script timed out")
    except Exception as e:
        print(f"  Error: {e}")

    return numbers


def wrap_text(text: str, width: int = 35) -> str:
    """Wrap text for display in graph nodes."""
    return '\n'.join(textwrap.wrap(text, width=width))


def format_text(template: str, numbers: dict, n_var: str) -> str:
    """Format a text template with numbers."""
    n = numbers.get(n_var)
    if n is not None:
        return template.format(n=n)
    return template.replace("{n:,}", "[N]")


def format_bullet(template: str, numbers: dict, bullet_vars: list, idx: int) -> str:
    """Format a bullet point template."""
    if "{n:,}" in template:
        var = bullet_vars[idx] if idx < len(bullet_vars) else None
        n = numbers.get(var) if var else None
        if n is not None:
            return template.format(n=n)
        return template.replace("{n:,}", "[N]")
    elif "{n1:,}" in template or "{n2:,}" in template:
        n1 = numbers.get(bullet_vars[0]) if len(bullet_vars) > 0 else None
        n2 = numbers.get(bullet_vars[1]) if len(bullet_vars) > 1 else None
        text = template
        text = text.replace("{n1:,}", f"{n1:,}" if n1 is not None else "[N]")
        text = text.replace("{n2:,}", f"{n2:,}" if n2 is not None else "[N]")
        return text
    return template


def create_flowchart(output_path: Path, numbers: dict = None, bw: bool = False):
    """Create the CONSORT flow chart using Graphviz.

    Args:
        output_path: Output file path (without extension for bw variant)
        numbers: Cohort numbers dict
        bw: If True, generate black-and-white version suitable for publishing
    """
    if numbers is None:
        numbers = {}

    dot = Digraph(comment='CONSORT Flow Diagram')
    dot.attr(rankdir='TB', splines='ortho', nodesep='0.4', ranksep='0.5')

    if bw:
        dot.attr('node', shape='box', fontname='Helvetica', fontsize='10')
        dot.attr('edge', color='black', penwidth='1.2')
        main_style = {'fillcolor': 'white', 'color': 'black', 'penwidth': '1.5'}
        excl_style = {'fillcolor': 'white', 'color': 'black', 'penwidth': '1', 'style': 'dashed'}
        split_style = {'fillcolor': 'white', 'color': 'black', 'penwidth': '1.5'}
        excl_edge_color = 'black'
    else:
        dot.attr('node', shape='box', style='filled,rounded', fontname='Helvetica', fontsize='10')
        dot.attr('edge', color='#2E86AB', penwidth='1.5')
        main_style = {'fillcolor': '#E8F4FD', 'color': '#2E86AB', 'penwidth': '2'}
        excl_style = {'fillcolor': '#FEF3E8', 'color': '#D4782C', 'penwidth': '1.5'}
        split_style = {'fillcolor': '#E8F4FD', 'color': '#2E86AB', 'penwidth': '2'}
        excl_edge_color = '#D4782C'

    # Create main flow nodes and exclusion nodes
    for i, step in enumerate(COHORT_FLOW):
        node_id = f"main_{i}"
        text = format_text(step["text"], numbers, step["n_var"])
        label = wrap_text(text, width=40)
        dot.node(node_id, label, **main_style)

        # Add exclusion node if present
        if "exclusion" in step:
            excl = step["exclusion"]
            excl_id = f"excl_{i}"

            header = format_text(excl["text"], numbers, excl["n_var"])
            bullets = []
            for j, bullet_template in enumerate(excl["bullets"]):
                bullet_text = format_bullet(bullet_template, numbers, excl["bullet_vars"], j)
                bullets.append(f"• {bullet_text}")

            excl_label = wrap_text(header, width=32) + '\n' + '\n'.join(wrap_text(b, width=32) for b in bullets)
            dot.node(excl_id, excl_label, **excl_style)

            # Edge to exclusion (dashed)
            dot.edge(node_id, excl_id, style='dashed', color=excl_edge_color)

    # Connect main flow nodes
    for i in range(len(COHORT_FLOW) - 1):
        dot.edge(f"main_{i}", f"main_{i+1}")

    # Treatment split - use invisible node for branching
    dot.node('branch', '', shape='point', width='0', height='0')
    dot.edge(f"main_{len(COHORT_FLOW)-1}", 'branch')

    # Initiators
    init = TREATMENT_SPLIT["initiators"]
    init_text = format_text(init["text"], numbers, init["n_var"])
    dot.node('initiators', wrap_text(init_text, width=32), **split_style)

    # Non-initiators
    non_init = TREATMENT_SPLIT["non_initiators"]
    non_init_text = format_text(non_init["text"], numbers, non_init["n_var"])
    dot.node('non_initiators', wrap_text(non_init_text, width=32), **split_style)

    # Connect branch to both
    dot.edge('branch', 'initiators')
    dot.edge('branch', 'non_initiators')

    # Force layout: exclusion nodes on same rank as their main nodes
    for i, step in enumerate(COHORT_FLOW):
        if "exclusion" in step:
            with dot.subgraph() as s:
                s.attr(rank='same')
                s.node(f"main_{i}")
                s.node(f"excl_{i}")

    # Force initiators and non-initiators on same rank
    with dot.subgraph() as s:
        s.attr(rank='same')
        s.node('initiators')
        s.node('non_initiators')

    # Render PNG and PDF
    output_stem = str(output_path.with_suffix(''))
    dot.render(output_stem, format='png', cleanup=False)
    dot.render(output_stem, format='pdf', cleanup=True)

    print(f"Created: {output_path}")
    print(f"Created: {output_path.with_suffix('.pdf')}")


def main():
    parser = argparse.ArgumentParser(description='Generate cohort inclusion flow chart')
    parser.add_argument('--output-path', type=Path,
                        default=Path('suicidality/Documents/flowchart_cohort_inclusion.png'),
                        help='Output path for the flow chart image')
    parser.add_argument('--rds-dir', type=Path,
                        default=Path('suicidality/extraction/output/rds'),
                        help='Directory containing RDS files')

    args = parser.parse_args()

    args.output_path.parent.mkdir(parents=True, exist_ok=True)

    numbers = {}
    if args.rds_dir and args.rds_dir.exists():
        numbers = read_rds_numbers(args.rds_dir)
        if numbers:
            print(f"Found {len(numbers)} cohort numbers:")
            for k, v in sorted(numbers.items()):
                print(f"  {k}: {v:,}")
        else:
            print("No cohort numbers found in RDS files")
    else:
        print(f"RDS directory not found: {args.rds_dir}")

    # Generate colored version
    create_flowchart(args.output_path, numbers, bw=False)

    # Generate black-and-white version for publishing
    bw_path = args.output_path.with_stem(args.output_path.stem + '_bw')
    create_flowchart(bw_path, numbers, bw=True)

    print(f"\nFlow chart (color): {args.output_path}")
    print(f"Flow chart (B&W):   {bw_path}")


if __name__ == '__main__':
    main()
