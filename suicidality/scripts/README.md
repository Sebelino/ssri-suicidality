# Scripts

Python scripts for generating documentation and figures.

## Scripts

| Script | Description | Output |
|--------|-------------|--------|
| `generate_codebook.py` | Generates codebook documents for RDS dataset files | Word documents (.docx) |
| `generate_logbook.py` | Generates MEB logbook documenting the data extraction pipeline | Word document (.docx), `pipeline_flowchart.png` |
| `generate_flowchart.py` | Generates CONSORT-style cohort inclusion flow chart | PNG image |

## Requirements

```bash
mamba install -n thesis python-docx graphviz r-base
```

## Usage

```bash
# Generate codebooks for all RDS files
python generate_codebook.py --input-dir ../Data\ extraction/output/rds --output-dir ../Documents/codebooks

# Generate codebook for a single file
python generate_codebook.py --single main_12wks_28.rds

# Generate logbook
python generate_logbook.py --output ../Documents/logbook.docx

# Generate flow chart
python generate_flowchart.py --output-path ../Documents/flowchart_cohort_inclusion.png --rds-dir ../Data\ extraction/output/rds
```
