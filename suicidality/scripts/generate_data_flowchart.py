#!/usr/bin/env python3
"""
Generate a data flow diagram showing RDS files with data previews.

Each node shows the first 3 rows of the RDS file. R scripts are not shown;
edges connect RDS files directly based on data dependencies.

Usage:
    python generate_data_flowchart.py [--output PATH]

Requirements:
    - pyreadr (mamba install -n thesis pyreadr)
    - graphviz (mamba install -n thesis graphviz)
"""

import argparse
import subprocess
import tempfile
from pathlib import Path
import pyreadr
import html

# Data flow: (input_rds_files, output_rds_files)
# Derived from the extraction pipeline, omitting scripts and databases
RDS_DEPENDENCIES = [
    # 03: raw_diagnoses_index -> raw_individual
    (["raw_diagnoses_index.rds"], ["raw_individual.rds"]),
    # 04: raw_diagnoses_index, raw_prescriptions, raw_individual -> cohort_*
    (["raw_diagnoses_index.rds", "raw_prescriptions.rds", "raw_individual.rds"],
     ["cohort_lopnrs.rds", "parent_lopnrs.rds", "cohort_base.rds"]),
    # 05: cohort_lopnrs -> raw_migration
    (["cohort_lopnrs.rds"], ["raw_migration.rds"]),
    # 06: cohort_lopnrs -> raw_dor
    (["cohort_lopnrs.rds"], ["raw_dor.rds"]),
    # 07: cohort_lopnrs -> raw_diagnoses_cohort
    (["cohort_lopnrs.rds"], ["raw_diagnoses_cohort.rds"]),
    # 08: parent_lopnrs -> raw_diagnoses_parents
    (["parent_lopnrs.rds"], ["raw_diagnoses_parents.rds"]),
    # 09: parent_lopnrs -> raw_lisa
    (["parent_lopnrs.rds"], ["raw_lisa.rds"]),
    # 10: cohort_lopnrs -> raw_hospitalization
    (["cohort_lopnrs.rds"], ["raw_hospitalization.rds"]),
    # 11: cohort_lopnrs -> raw_prescriptions_cohort
    (["cohort_lopnrs.rds"], ["raw_prescriptions_cohort.rds"]),
    # 12: cohort_base, raw_migration, raw_dor -> base_28
    (["cohort_base.rds", "raw_migration.rds", "raw_dor.rds"], ["base_28.rds"]),
    # 13: raw_diagnoses_cohort, raw_dor -> dia_all_28
    (["raw_diagnoses_cohort.rds", "raw_dor.rds"], ["dia_all_28.rds"]),
    # 14: raw_hospitalization -> cens_hosp_28
    (["raw_hospitalization.rds"], ["cens_hosp_28.rds"]),
    # 15: base_28, dia_all_28, cens_hosp_28 -> main_12wks_28_tmp
    (["base_28.rds", "dia_all_28.rds"], ["main_12wks_28_tmp.rds"]),
    # 16: cohort_base, raw_diagnoses_parents -> cov_family_history
    (["cohort_base.rds", "raw_diagnoses_parents.rds"], ["cov_family_history.rds"]),
    # 17: cohort_base, raw_lisa -> cov_education
    (["cohort_base.rds", "raw_lisa.rds"], ["cov_education.rds"]),
    # 18: cohort_base, raw_lisa -> cov_income
    (["cohort_base.rds", "raw_lisa.rds"], ["cov_income.rds"]),
    # 19: main_12wks_28_tmp, raw_diagnoses_cohort -> cov_diagnoses
    (["main_12wks_28_tmp.rds", "raw_diagnoses_cohort.rds"], ["cov_diagnoses.rds"]),
    # 20: main_12wks_28_tmp, raw_prescriptions_cohort -> cov_medications, othermeds_28
    (["main_12wks_28_tmp.rds", "raw_prescriptions_cohort.rds"], ["cov_medications.rds", "othermeds_28.rds"]),
    # 21: main_12wks_28_tmp, raw_hospitalization -> cov_hospitalizations
    (["main_12wks_28_tmp.rds", "raw_hospitalization.rds"], ["cov_hospitalizations.rds"]),
    # 22: base_28, cov_* -> base_cov_28
    (["base_28.rds", "cov_family_history.rds", "cov_education.rds", "cov_income.rds",
      "cov_diagnoses.rds", "cov_medications.rds", "cov_hospitalizations.rds"], ["base_cov_28.rds"]),
    # 23: main_12wks_28_tmp, othermeds_28, raw_prescriptions_cohort -> pp_12wks_max_tmp
    (["main_12wks_28_tmp.rds", "othermeds_28.rds", "raw_prescriptions_cohort.rds"], ["pp_12wks_max_tmp.rds"]),
    # 24: main_12wks_28_tmp, pp_12wks_max_tmp, base_cov_28, raw_individual, raw_diagnoses_index -> final
    (["main_12wks_28_tmp.rds", "pp_12wks_max_tmp.rds", "base_cov_28.rds", "raw_individual.rds", "raw_diagnoses_index.rds"],
     ["main_12wks_28.rds", "pp_12wks_max.rds"]),
]

FINAL_DATASETS = {"main_12wks_28.rds", "pp_12wks_max.rds"}

# Database to RDS file mappings
DATABASE_TO_RDS = {
    "v_npr_dia": ["raw_diagnoses_index.rds", "raw_diagnoses_cohort.rds", "raw_diagnoses_parents.rds", "raw_hospitalization.rds"],
    "v_lmr": ["raw_prescriptions.rds", "raw_prescriptions_cohort.rds"],
    "v_individual": ["raw_individual.rds"],
    "v_parent": ["cohort_base.rds"],
    "v_migration": ["raw_migration.rds"],
    "v_dor": ["raw_dor.rds"],
    "v_lisa": ["raw_lisa.rds"],
}

# Files that come directly from database (shown with different color)
DATABASE_OUTPUTS = {"raw_diagnoses_index.rds", "raw_prescriptions.rds"}


def format_value(val, max_len=12):
    """Format a value for display, truncating if needed."""
    if val is None:
        return "NA"
    s = str(val)
    if len(s) > max_len:
        return s[:max_len-2] + ".."
    return s


def format_dataframe_preview(df, max_rows=3, max_cols=6):
    """Format first few rows of a dataframe as HTML table."""
    if df is None or len(df) == 0:
        return "<i>empty</i>"

    cols = list(df.columns)[:max_cols]
    rows = min(len(df), max_rows)

    # Build HTML table
    lines = ['<table border="0" cellspacing="0" cellpadding="2">']

    # Header
    lines.append('<tr>')
    for col in cols:
        lines.append(f'<td><b>{html.escape(str(col)[:10])}</b></td>')
    if len(df.columns) > max_cols:
        lines.append('<td><b>...</b></td>')
    lines.append('</tr>')

    # Data rows
    for i in range(rows):
        lines.append('<tr>')
        for col in cols:
            val = format_value(df.iloc[i][col])
            lines.append(f'<td>{html.escape(val)}</td>')
        if len(df.columns) > max_cols:
            lines.append('<td>...</td>')
        lines.append('</tr>')

    if len(df) > max_rows:
        lines.append(f'<tr><td colspan="{len(cols) + (1 if len(df.columns) > max_cols else 0)}"><i>... ({len(df)} rows)</i></td></tr>')

    lines.append('</table>')
    return ''.join(lines)


def read_rds_preview(file_path):
    """Read an RDS file and return a preview string."""
    try:
        result = pyreadr.read_r(str(file_path))
        if result is None or len(result) == 0:
            return "<i>empty</i>", 0

        # pyreadr returns a dict; get the first (usually only) dataframe
        df = list(result.values())[0]
        nrows = len(df)
        preview = format_dataframe_preview(df)
        return preview, nrows
    except Exception as e:
        return f"<i>Error: {html.escape(str(e)[:30])}</i>", 0


def generate_flowchart(rds_dir: Path, output_path: Path) -> bool:
    """Generate the data flow diagram."""

    # Collect all RDS files
    all_rds = set()
    for inputs, outputs in RDS_DEPENDENCIES:
        all_rds.update(inputs)
        all_rds.update(outputs)
    all_rds.update(DATABASE_OUTPUTS)

    # Read previews
    previews = {}
    for rds_file in all_rds:
        file_path = rds_dir / rds_file
        if file_path.exists():
            preview, nrows = read_rds_preview(file_path)
            previews[rds_file] = (preview, nrows)
        else:
            previews[rds_file] = ("<i>not found</i>", 0)

    # Build DOT graph
    dot_lines = [
        'digraph DataFlow {',
        '    rankdir=TB;',
        '    node [shape=none, fontname="Helvetica", fontsize=9];',
        '    edge [fontname="Helvetica", fontsize=8];',
        '',
        '    // Database nodes',
    ]

    # Create database nodes (cylinder shape, name only)
    for db_name in sorted(DATABASE_TO_RDS.keys()):
        node_id = db_name.replace(".", "_").replace("-", "_")
        dot_lines.append(f'    {node_id} [shape=cylinder, style=filled, fillcolor="#E8E8E8", label="{db_name}", fontsize=10];')

    # Put all databases on the same rank (top layer)
    db_ids = ' '.join(db.replace(".", "_").replace("-", "_") for db in DATABASE_TO_RDS.keys())
    dot_lines.append(f'    {{ rank=same; {db_ids}; }}')
    dot_lines.append('')

    # Create RDS nodes
    for rds_file in sorted(all_rds):
        preview, nrows = previews.get(rds_file, ("<i>?</i>", 0))
        name = rds_file.replace(".rds", "")

        # Determine fill color
        # Collect RDS files that have direct database input
        db_outputs = set()
        for rds_list in DATABASE_TO_RDS.values():
            db_outputs.update(rds_list)

        if rds_file in FINAL_DATASETS:
            bgcolor = "#90EE90"  # Light green for final
        elif rds_file in db_outputs:
            bgcolor = "#E8E8E8"  # Gray for database extracts
        else:
            bgcolor = "#FFFACD"  # Light yellow for intermediate

        # Create HTML label with table
        label = f'''<
<table border="1" cellborder="0" cellspacing="0" cellpadding="4" bgcolor="{bgcolor}">
<tr><td><b>{html.escape(name)}</b></td></tr>
<tr><td>{preview}</td></tr>
</table>>'''

        node_id = rds_file.replace(".", "_").replace("-", "_")
        dot_lines.append(f'    {node_id} [label={label}];')

    dot_lines.append('')
    dot_lines.append('    // Edges from databases to RDS files')
    for db_name, rds_files in DATABASE_TO_RDS.items():
        db_id = db_name.replace(".", "_").replace("-", "_")
        for rds_file in rds_files:
            rds_id = rds_file.replace(".", "_").replace("-", "_")
            dot_lines.append(f'    {db_id} -> {rds_id};')

    dot_lines.append('')
    dot_lines.append('    // Edges between RDS files')

    # Create edges (RDS to RDS)
    for inputs, outputs in RDS_DEPENDENCIES:
        for inp in inputs:
            for out in outputs:
                inp_id = inp.replace(".", "_").replace("-", "_")
                out_id = out.replace(".", "_").replace("-", "_")
                dot_lines.append(f'    {inp_id} -> {out_id};')

    dot_lines.append('}')

    dot_content = '\n'.join(dot_lines)

    # Write DOT file and generate PNG
    try:
        with tempfile.NamedTemporaryFile(mode='w', suffix='.dot', delete=False) as f:
            f.write(dot_content)
            dot_path = f.name

        # Also save DOT file for debugging
        dot_output = output_path.with_suffix('.dot')
        with open(dot_output, 'w') as f:
            f.write(dot_content)
        print(f"DOT file: {dot_output}")

        # Run graphviz
        result = subprocess.run(
            ['dot', '-Tpng', '-Gdpi=150', dot_path, '-o', str(output_path)],
            capture_output=True,
            text=True
        )

        if result.returncode != 0:
            print(f"Graphviz error: {result.stderr}")
            return False

        Path(dot_path).unlink()
        return True

    except FileNotFoundError:
        print("Error: Graphviz 'dot' command not found. Install with: mamba install -n thesis graphviz")
        return False
    except Exception as e:
        print(f"Error generating flow chart: {e}")
        return False


def main():
    parser = argparse.ArgumentParser(description='Generate data flow diagram with RDS previews')
    parser.add_argument('--output', type=Path,
                        default=Path('suicidality/Documents/data_flowchart.png'),
                        help='Output path for the PNG image')
    parser.add_argument('--rds-dir', type=Path,
                        default=Path('suicidality/extraction/output/rds'),
                        help='Directory containing RDS files')

    args = parser.parse_args()

    # Ensure output directory exists
    args.output.parent.mkdir(parents=True, exist_ok=True)

    if generate_flowchart(args.rds_dir, args.output):
        print(f"Generated: {args.output}")
    else:
        print("Failed to generate flowchart")
        exit(1)


if __name__ == '__main__':
    main()
