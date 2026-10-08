#!/usr/bin/env python3
"""Add the shared header of the publication website to one rendered page.

Every page of a website version (start page, preregistration, Code Browser,
results report) gets the same slim bar: the project title, links to the other
pages and to the source code, the current page marked. The bar stays at the
top while the page scrolls. The Code Browser fills the window, so its columns
shrink by the bar's height; on the Quarto pages the table of contents and the
targets of section links start below the bar.

    python3 add_publication_header.py PAGE CURRENT [--repository URL]

CURRENT is one of: overview, preregistration, code-browser, results-report.
"""
import argparse
from pathlib import Path

PAGES = [
    ("overview", "index.html", "Overview"),
    ("preregistration", "preregistration.html", "Preregistration"),
    ("code-browser", "code-browser.html", "Code Browser"),
    ("results-report", "results-report.html", "Results report"),
]
HEIGHT = 46

STYLE = f"""<style id="publication-header-style">
.publication-header{{position:sticky;top:0;z-index:2000;height:{HEIGHT}px;box-sizing:border-box;display:flex;align-items:center;gap:1.4rem;padding:0 1.2rem;background:#1d2733;border-bottom:1px solid #0f151c;font:500 14px/1 system-ui,-apple-system,"Segoe UI",sans-serif}}
.publication-header .publication-title{{color:#fff;font-weight:650;text-decoration:none;margin-right:.6rem;white-space:nowrap}}
.publication-header nav{{display:flex;gap:.2rem;overflow-x:auto}}
.publication-header nav a{{color:#c9d3de;text-decoration:none;padding:.45rem .7rem;border-radius:.35rem;white-space:nowrap}}
.publication-header nav a:hover{{color:#fff;background:#2c3a4a}}
.publication-header nav a[aria-current="page"]{{color:#fff;background:#2780e3}}
.publication-header .publication-source{{margin-left:auto;color:#c9d3de;text-decoration:none;white-space:nowrap}}
.publication-header .publication-source:hover{{color:#fff}}
html{{scroll-padding-top:{HEIGHT + 12}px}}
#quarto-margin-sidebar,#quarto-sidebar,.sidebar.toc-left,.sidebar.margin-sidebar{{top:{HEIGHT + 8}px !important;max-height:calc(100vh - {HEIGHT + 8}px) !important}}
.shell,.shell>.side,.shell>main{{height:calc(100vh - {HEIGHT}px) !important}}
@media (max-width:760px){{.publication-header .publication-title{{display:none}}.shell,.shell>.side,.shell>main{{height:auto !important}}}}
</style>"""


def build_header(current, repository):
    links = "".join(
        f'<a href="{href}"{" aria-current=\"page\"" if key == current else ""}>{label}</a>'
        for key, href, label in PAGES
    )
    source = f'<a class="publication-source" href="{repository}">Source code ↗</a>' if repository else ""
    return (f'<header class="publication-header"><a class="publication-title" href="index.html">'
            f'Motives and Ideology</a><nav aria-label="Website">{links}</nav>{source}</header>')


def add_publication_header(page, current, repository=""):
    text = page.read_text(encoding="utf-8")
    # The source renders live in separate directories; publication pages are siblings.
    for source, published in (
        ("../../preregistration/renders/preregistration.html", "preregistration.html"),
        ("../../analysis/report/results_draft.html", "results-report.html"),
    ):
        text = text.replace(f'href="{source}"', f'href="{published}"')
    if 'id="publication-header-style"' in text:
        raise ValueError(f"{page} already has the header.")
    if "</head>" not in text or "<body" not in text:
        raise ValueError(f"{page} is not a complete HTML document.")
    text = text.replace("</head>", STYLE + "\n</head>", 1)
    body_start = text.index("<body")
    body_end = text.index(">", body_start) + 1
    text = text[:body_end] + "\n" + build_header(current, repository) + text[body_end:]
    page.write_text(text, encoding="utf-8")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("page", type=Path)
    parser.add_argument("current", choices=[key for key, _, _ in PAGES])
    parser.add_argument("--repository", default="")
    arguments = parser.parse_args()
    add_publication_header(arguments.page, arguments.current, arguments.repository)
