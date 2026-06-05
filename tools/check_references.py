#!/usr/bin/env python3
"""
check_references.py

Validate every @article entry in a BibTeX file against the Crossref metadata
for its DOI. Reports mismatches for: title, first author, journal, year,
volume, issue, pages.

Usage:
    python tools/check_references.py [path/to/references.bib]
    (default: suicidality/report/references.bib)

Exit code 0 if every entry checks out, 1 if any discrepancies are found.
Requires only the Python standard library + curl on PATH.
"""

from __future__ import annotations
import json
import re
import subprocess
import sys
import unicodedata
from pathlib import Path


DEFAULT_BIB = Path(__file__).resolve().parent.parent / "suicidality" / "report" / "references.bib"

# ANSI colour for terminal output. Auto-off when not a TTY.
USE_COLOR = sys.stdout.isatty()
def c(s, code):
    return f"\033[{code}m{s}\033[0m" if USE_COLOR else s
def green(s):   return c(s, "32")
def red(s):     return c(s, "31")
def yellow(s):  return c(s, "33")
def dim(s):     return c(s, "2")


# ---------- BibTeX parsing (light, just enough for @article entries) ----------

ENTRY_RE = re.compile(r"^@(\w+)\s*\{\s*([^,\s]+)\s*,", re.MULTILINE)

def parse_bib(path: Path) -> list[dict]:
    """Return a list of entry dicts: {bibkey, type, fields:{...}}."""
    text = path.read_text(encoding="utf-8")
    entries = []
    starts = [(m.start(), m.group(1), m.group(2)) for m in ENTRY_RE.finditer(text)]
    for i, (start, etype, bibkey) in enumerate(starts):
        end = starts[i + 1][0] if i + 1 < len(starts) else len(text)
        body = text[start:end]
        # Extract fields. Tolerate {value}, "value", and trailing-comma-or-end.
        fields = {}
        for m in re.finditer(
            r"\b(\w+)\s*=\s*(?:\{((?:[^{}]|\{[^{}]*\})*)\}|\"([^\"]*)\")",
            body,
        ):
            key = m.group(1).lower()
            val = m.group(2) if m.group(2) is not None else m.group(3)
            fields[key] = val.strip()
        entries.append({"bibkey": bibkey, "type": etype.lower(), "fields": fields})
    return entries


# ---------- Crossref lookup ----------

def fetch_crossref(doi: str) -> dict | None:
    """Return the Crossref 'message' object for a DOI, or None on failure."""
    try:
        out = subprocess.check_output(
            ["curl", "-sL", "--max-time", "15",
             f"https://api.crossref.org/works/{doi}"],
            stderr=subprocess.DEVNULL,
        )
        d = json.loads(out)
        return d.get("message")
    except Exception:
        return None


# ---------- Field normalisation ----------

def deaccent(s: str) -> str:
    return "".join(
        ch for ch in unicodedata.normalize("NFKD", s) if not unicodedata.combining(ch)
    )

def strip_tex(s: str) -> str:
    r"""Strip the common BibTeX-isms: \"o, {\'a}, \&, --, ~, etc."""
    s = re.sub(r"\\[\'`\"^~]\{?(\w)\}?", r"\1", s)   # \'a → a, \"o → o
    s = re.sub(r"\\&", "&", s)
    s = re.sub(r"--", "–", s)                        # en-dash
    s = re.sub(r"\{|\}", "", s)
    s = re.sub(r"\s+", " ", s).strip()
    return s

def decode_html_entities(s: str) -> str:
    return s.replace("&amp;", "&").replace("&lt;", "<").replace("&gt;", ">")

def normalise_title(s: str) -> str:
    s = strip_tex(s)
    s = decode_html_entities(s)
    s = deaccent(s).lower()
    s = re.sub(r"[^a-z0-9]+", " ", s).strip()
    return s

def normalise_journal(s: str) -> str:
    """Like normalise_title but also strips a leading 'the '."""
    s = normalise_title(s)
    if s.startswith("the "):
        s = s[4:]
    return s

def normalise_pages(s: str) -> str:
    return re.sub(r"[\s–-]+", "-", strip_tex(s)).strip()

def pages_match(bib: str, cr: str) -> bool:
    """Pages match if equal after normalisation, or if Crossref only carries
    the start page (advance access) and bib's start page matches it."""
    if not cr:
        return True  # Crossref-side metadata gap
    b, c = normalise_pages(bib), normalise_pages(cr)
    if b == c:
        return True
    # Crossref gave only "1683" but bib has "1683-1696" — accept if start matches
    if "-" in b and "-" not in c and b.split("-")[0] == c:
        return True
    return False

def title_match(bib: str, cr: str) -> bool:
    """Titles match after normalisation, or if Crossref truncated on punctuation."""
    nb, nc = normalise_title(bib), normalise_title(cr)
    if nb == nc:
        return True
    # Crossref title is sometimes truncated mid-sentence at semicolons/colons
    shorter, longer = sorted([nb, nc], key=len)
    return longer.startswith(shorter) and len(shorter) > 20

def first_author_family(bib_authors: str) -> str:
    first = bib_authors.split(" and ")[0]
    if "," in first:
        family = first.split(",")[0].strip()
    else:
        # "Given Family" form
        family = first.strip().split()[-1]
    return deaccent(strip_tex(family)).lower()

def crossref_first_author_family(cr_authors: list[dict]) -> str:
    if not cr_authors:
        return ""
    return deaccent(cr_authors[0].get("family", "")).lower()


# ---------- Comparison ----------

CHECK_FIELDS = ["title", "first_author", "journal", "year", "volume", "issue", "pages"]

def compare(entry: dict, cr: dict) -> list[tuple[str, str, str, bool]]:
    """Return list of (field, bib_value, crossref_value, ok)."""
    f = entry["fields"]
    cr_title = cr["title"][0] if cr.get("title") else ""
    cr_journal = cr["container-title"][0] if cr.get("container-title") else ""
    cr_year = str(cr.get("issued", {}).get("date-parts", [[""]])[0][0])
    cr_volume = cr.get("volume", "")
    cr_issue = cr.get("issue", "")
    cr_pages = cr.get("page", "") or cr.get("article-number", "")
    cr_first = crossref_first_author_family(cr.get("author", []))

    bib_title = f.get("title", "")
    bib_journal = f.get("journal", "")
    bib_year = f.get("year", "")
    bib_volume = f.get("volume", "")
    bib_issue = f.get("number", "")
    bib_pages = f.get("pages", "")
    bib_first = first_author_family(f.get("author", ""))

    # Year mismatches are often "Crossref shows online-publication year,
    # bib uses print-publication year" — flag only if they differ by more
    # than one (or if there's no reason to suspect advance access).
    year_ok = (bib_year == cr_year) or (
        bib_year.isdigit() and cr_year.isdigit()
        and abs(int(bib_year) - int(cr_year)) <= 1
    )

    return [
        ("title",        bib_title,   cr_title,   title_match(bib_title, cr_title)),
        ("first_author", bib_first,   cr_first,   bib_first == cr_first),
        ("journal",      bib_journal, cr_journal, normalise_journal(bib_journal) == normalise_journal(cr_journal)),
        ("year",         bib_year,    cr_year,    year_ok),
        # Volume / pages / issue: Crossref metadata is frequently incomplete.
        # Only fail when Crossref has a value and it disagrees with the bib.
        ("volume",       bib_volume,  cr_volume,  (not cr_volume) or (bib_volume == cr_volume)),
        ("issue",        bib_issue,   cr_issue,   (not cr_issue)  or (bib_issue  == cr_issue)),
        ("pages",        bib_pages,   cr_pages,   pages_match(bib_pages, cr_pages)),
    ]


# ---------- Reporting ----------

def main(bib_path: Path) -> int:
    if not bib_path.exists():
        print(f"ERROR: bib file not found: {bib_path}", file=sys.stderr)
        return 2

    entries = [e for e in parse_bib(bib_path) if e["type"] == "article"]
    if not entries:
        print(f"No @article entries found in {bib_path}")
        return 0

    print(f"Checking {len(entries)} @article entries in {bib_path}\n")

    n_fail = 0
    n_doi_missing = 0
    n_doi_invalid = 0

    for e in entries:
        bibkey = e["bibkey"]
        doi = e["fields"].get("doi", "").strip()
        if not doi:
            print(f"{yellow('?')} {bibkey:<25} (no DOI in bib)")
            n_doi_missing += 1
            continue

        cr = fetch_crossref(doi)
        if cr is None:
            print(f"{red('!')} {bibkey:<25} DOI not resolvable: {doi}")
            n_doi_invalid += 1
            n_fail += 1
            continue

        results = compare(e, cr)
        bad = [r for r in results if not r[3]]
        if not bad:
            print(f"{green('OK')} {bibkey:<25} {dim(doi)}")
        else:
            print(f"{red('FAIL')} {bibkey:<25} {dim(doi)}")
            for field, bib_v, cr_v, _ in bad:
                print(f"   {red(field)}:")
                print(f"      bib:      {bib_v!r}")
                print(f"      crossref: {cr_v!r}")
            n_fail += 1

    print()
    print(f"Summary: {len(entries) - n_fail} OK, {n_fail} with mismatches, "
          f"{n_doi_missing} without DOI, {n_doi_invalid} unresolvable")
    return 0 if n_fail == 0 else 1


if __name__ == "__main__":
    path = Path(sys.argv[1]) if len(sys.argv) > 1 else DEFAULT_BIB
    sys.exit(main(path))
