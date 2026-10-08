#!/usr/bin/env python3
"""Build linked HTML copies without changing the preregistration or report source."""
import argparse
import html
import re
from html.parser import HTMLParser
import json
from pathlib import Path

class ReadIdentifiers(HTMLParser):
    def __init__(self):
        super().__init__()
        self.identifiers = set()
    def handle_starttag(self, tag, attrs):
        self.identifiers.update(value for key, value in attrs if key == 'id')

def read_identifiers(text):
    parser = ReadIdentifiers()
    parser.feed(text)
    return parser.identifiers

def read_section_headings(text):
    """The heading text of every section, by section id, as the rendered preregistration shows it."""
    headings = {}
    for match in re.finditer(r'<section id="([^"]+)"[^>]*>\s*<h[1-6][^>]*>(.*?)</h[1-6]>', text, re.S):
        title = re.sub(r'<span class="header-section-number">.*?</span>', '', match.group(2))
        title = html.unescape(re.sub(r'<[^>]+>', '', title))
        headings[match.group(1)] = ' '.join(title.replace(' — ', ': ').split())
    return headings

def encode_script_data(value):
    return json.dumps(value, ensure_ascii=False).replace('&', '\\u0026').replace('<', '\\u003c').replace('>', '\\u003e')

def append_navigation(text, mappings, code_titles, kind):
    if 'id="publication-section-links"' in text:
        raise ValueError('Use the original rendered input, not an already linked HTML copy.')
    if '</body>' not in text:
        raise ValueError('Input is not a complete HTML document.')
    payload = encode_script_data({'sections': mappings, 'titles': code_titles, 'kind': kind})
    script = r'''
<style id="publication-section-links">
.publication-links{margin:.65rem 0;padding:.55rem .8rem;border-left:3px solid #3988ab;font:14px/1.5 system-ui,sans-serif;white-space:normal}
.publication-links a{color:#267391;text-decoration:underline;margin-right:1rem}
body:has(.look1) .publication-links a{color:#62d7ff}
</style>
<script>
(() => {
const data = __PUBLICATION_DATA__;
const add = (anchor, links, label) => {
  if (!anchor) throw new Error('Missing publication section after HTML rendering');
  const nav = document.createElement('nav');
  nav.className = 'publication-links'; nav.setAttribute('aria-label', label);
  nav.append(document.createTextNode(label + ': '));
  for (const [text, href] of links) {
    const link = document.createElement('a'); link.textContent = text; link.href = href;
    nav.append(link);
  }
  anchor.after(nav);
};
if (data.kind === 'preregistration') {
  for (const item of data.sections) {
    const section = document.getElementById(item.preregistration);
    const heading = section && (/^H[1-6]$/.test(section.tagName) ? section : section.querySelector('h1,h2,h3,h4,h5,h6'));
    add(heading, item.code.map(id => [data.titles[id], 'code-browser.html#' + id]), 'Code Browser');
  }
} else {
  const reverse = new Map();
  for (const item of data.sections) for (const id of item.code) {
    if (!reverse.has(id)) reverse.set(id, []);
    reverse.get(id).push([item.label, 'preregistration.html#' + item.preregistration]);
  }
  for (const [id, links] of reverse) {
    const box = document.getElementById(id + '-file');
    add(box && box.querySelector('header'), links, 'Preregistration');
  }
}
})();
</script>
'''.replace('__PUBLICATION_DATA__', payload)
    return text.replace('</body>', script + '\n</body>', 1)

def link_publication_sections(preregistration, browser, mappings_path, output):
    mappings = json.loads(mappings_path.read_text())
    prereg_text = preregistration.read_text()
    # Versioned sites keep the survey beside the HTML and offer only the PDF
    # alternate format. These changes affect generated navigation, not QMD.
    prereg_text = prereg_text.replace('href="../qualtrics/ZMASCpanel.qsf"',
                                     'href="ZMASCpanel.qsf"')
    prereg_text = re.sub(r'<li>\s*<a href="preregistration[.]docx">.*?</a>\s*</li>',
                        '', prereg_text, flags=re.S)
    browser_text = browser.read_text()
    code_sections = json.loads(Path(str(browser) + '.sections.json').read_text())
    code_titles = {row['id']: row['title'].strip() for row in code_sections}
    document_ids = read_identifiers(prereg_text)
    missing_document = sorted({row['preregistration'] for row in mappings} - document_ids)
    missing_code = sorted({code for row in mappings for code in row['code']} - code_titles.keys())
    if missing_document or missing_code:
        raise ValueError(f'Section map needs review. Missing document sections: {missing_document}; missing code sections: {missing_code}')
    if len({row['preregistration'] for row in mappings}) != len(mappings):
        raise ValueError('Duplicate preregistration section in mapping')
    # The labels follow the preregistration's current headings, so a renumbering
    # cannot leave a stale label; the map only assigns sections to code.
    headings = read_section_headings(prereg_text)
    mappings = [dict(row, label=headings.get(row['preregistration'], row.get('label', row['preregistration'])))
                for row in mappings]
    linked_prereg = append_navigation(prereg_text, mappings, code_titles, 'preregistration')
    linked_browser = append_navigation(browser_text, mappings, code_titles, 'browser')
    for source in [preregistration, browser]:
        if source.resolve() == (output / ('preregistration.html' if source == preregistration else 'code-browser.html')).resolve():
            raise ValueError('Output must be separate from the original rendered inputs.')
    output.mkdir(parents=True, exist_ok=True)
    (output / 'preregistration.html').write_text(linked_prereg)
    (output / 'code-browser.html').write_text(linked_browser)
    (output / 'index.html').write_text('''<!doctype html><html lang="en"><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>Social Motives and Ideology</title><body style="max-width:52rem;margin:4rem auto;padding:1rem;font:18px/1.6 system-ui"><h1>Social Motives and Ideology</h1><p>Preparation preview. The preregistration and results report remain under author review.</p><ul><li><a href="preregistration.html">Preregistration</a></li><li><a href="code-browser.html">Code Browser</a></li></ul><p>The Code Browser is generated from the implemented analysis pipeline and report code. Section links connect it with the preregistration.</p></body></html>''')
    print(f'Linked {len(mappings)} preregistration sections to {len({code for row in mappings for code in row["code"]})} code sections in {output}')

def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--preregistration', type=Path, required=True)
    parser.add_argument('--browser', type=Path, required=True)
    parser.add_argument('--mapping', type=Path, default=Path(__file__).with_name('publication_sections.json'))
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    link_publication_sections(args.preregistration, args.browser, args.mapping, args.output)

if __name__ == '__main__':
    main()
