#!/usr/bin/env python3
"""Print a self-contained accepted report HTML locally, without executing its code.

Requirements: Python 3, PyMuPDF (fitz), Pillow, XeLaTeX with standalone and
amsmath. The tested local versions are recorded in the export manifest.
This offline exporter does not invoke a browser, R, renv, targets or models.
"""

from pathlib import Path
from html.parser import HTMLParser
from html import escape
import re, base64, subprocess, json, io, hashlib, argparse, gzip, tempfile, unicodedata, sys
import fitz
from PIL import Image, ImageOps, ImageDraw, ImageChops, ImageStat


cli=argparse.ArgumentParser(description=__doc__)
cli.add_argument('--input-html',type=Path,required=True,help='Self-contained HTML; gzip is also accepted')
cli.add_argument('--output-pdf',type=Path,required=True)
cli.add_argument('--stylesheet',type=Path,default=Path(__file__).with_name('results-print.css'))
cli.add_argument('--qa-dir',type=Path,required=True,help='Contact sheets and export manifest destination')
cli.add_argument('--page-images',type=Path,help='Optional individual page PNG destination')
cli.add_argument('--source-commit',default='',help='Descriptive provenance only; no Git operations run')
args=cli.parse_args()
SOURCE=args.input_html.resolve()
args.output_pdf.parent.mkdir(parents=True,exist_ok=True)
args.qa_dir.mkdir(parents=True,exist_ok=True)
if args.page_images:args.page_images.mkdir(parents=True,exist_ok=True)
scratch=tempfile.TemporaryDirectory(prefix='results-pdf-')
TMP=Path(scratch.name)
input_bytes=SOURCE.read_bytes()
if SOURCE.suffix=='.gz':input_bytes=gzip.decompress(input_bytes)
input_html=input_bytes.decode('utf-8')

class Node:
 def __init__(self,tag='',attrs=None): self.tag=tag; self.attrs=dict(attrs or []); self.children=[]
 def text(self): return ''.join(x.text() if isinstance(x,Node) else x for x in self.children)
 def walk(self):
  yield self
  for x in self.children:
   if isinstance(x,Node):yield from x.walk()
 def html(self):
  if not self.tag:return ''.join(x.html() if isinstance(x,Node) else escape(x,quote=False) for x in self.children)
  a=''.join(' '+k+'="'+escape(v or '',quote=True)+'"' for k,v in self.attrs.items())
  if self.tag in {'img','br','hr','col','meta','link','input'}:return '<'+self.tag+a+'>'
  return '<'+self.tag+a+'>'+''.join(x.html() if isinstance(x,Node) else escape(x,quote=False) for x in self.children)+'</'+self.tag+'>'
class Parser(HTMLParser):
 def __init__(self):super().__init__(convert_charrefs=True);self.root=Node();self.stack=[self.root]
 def handle_starttag(self,tag,attrs):
  n=Node(tag,attrs);self.stack[-1].children.append(n)
  if tag not in {'img','br','hr','col','meta','link','input','base'}:self.stack.append(n)
 def handle_endtag(self,tag):
  for i in range(len(self.stack)-1,0,-1):
   if self.stack[i].tag==tag:self.stack=self.stack[:i];return
 def handle_data(self,data):self.stack[-1].children.append(data)

p=Parser();p.feed(input_html);main=next(n for n in p.root.walk() if n.tag=='main')
# The HTML keeps its navigation; printed pages start with the report itself.
for n in list(main.walk()):
 n.children=[x for x in n.children if not isinstance(x,Node) or
             (x.attrs.get('role')!='doc-toc' and x.attrs.get('id') not in {'TOC','TOC-body'})]
 if n.tag=='a' and n.attrs.get('href') in {'../../preregistration/renders/preregistration.html','preregistration.html'} and n.text()=='Preregistration (HTML)':
  n.children=['Preregistration (PDF)']
title=next(n.text().strip() for n in main.walk() if n.tag=='h1' and 'title' in n.attrs.get('class','').split())
title_header=next(n for n in main.walk() if n.attrs.get('id')=='title-block-header')
authors=[n.text().strip() for n in title_header.walk() if n.tag=='p' and
         ('author' in n.attrs.get('class','').split() or
          any(x.tag=='a' and x.attrs.get('href','').startswith('https://orcid.org/') for x in n.walk()))]
contributors=[n.text().strip() for n in main.walk() if 'report-author' in n.attrs.get('class','').split()]
if contributors:authors=contributors
maths=[n for n in main.walk() if n.tag=='span' and 'math' in n.attrs.get('class','').split()]
for i,n in enumerate(maths):
 tex=n.text().strip(); body=re.sub(r'^\\[\[(]|\\[\])]$', '', tex).strip()
 latex='\\documentclass[border=1pt]{standalone}\n\\usepackage{amsmath}\n\\begin{document}\n{\\fontsize{11}{14}\\selectfont $'+('\\displaystyle ' if 'display' in n.attrs.get('class','') else '')+body+'$}\n\\end{document}\n'
 (TMP/f'math-{i}.tex').write_text(latex)
 if not (TMP/f'math-{i}.pdf').exists():
  r=subprocess.run(['xelatex','-interaction=nonstopmode','-halt-on-error',f'-output-directory={TMP}',str(TMP/f'math-{i}.tex')],capture_output=True,text=True)
  if r.returncode:raise RuntimeError(r.stdout[-2500:])
 d=fitz.open(TMP/f'math-{i}.pdf'); page=d[0];pix=page.get_pixmap(matrix=fitz.Matrix(4,4),alpha=True)
 uri='data:image/png;base64,'+base64.b64encode(pix.tobytes('png')).decode()
 w,h=page.rect.width,page.rect.height
 ratio=min(1,495/w)
 if 'display' in n.attrs.get('class',''):ratio=min(ratio,0.92)
 image=Node('img',{'src':uri,'width':str(w*ratio),'height':str(h*ratio),'style':f'width:{w*ratio}px;height:{h*ratio}px;vertical-align:middle;','alt':tex})
 n.children=[image];n.attrs={'class':'math-rendered '+('display-equation' if 'display' in n.attrs.get('class','') else 'inline-equation')}

# PDF-only layout normalization retains all content and image bytes.
for n in list(main.walk()):
 n.children=[x for x in n.children if not isinstance(x,Node) or x.tag not in {'script','style'}]
 if n.tag=='nav':n.tag='div'
 if n.tag=='span':n.attrs.pop('style',None)
 if n.tag=='th':
  parts=[]
  for c in n.children:
   if isinstance(c,Node) and c.tag=='sub':parts.append(Node('br'))
   parts.append(c)
  n.children=parts
 if n.tag in {'td','th'}:
  align='right' if 'gt_right' in n.attrs.get('class','') else 'center' if 'gt_center' in n.attrs.get('class','') else 'left'
  n.attrs['style']='text-align:'+align+';'
 if n.tag=='div':n.attrs.pop('style',None)
 if n.tag=='col':n.attrs.pop('style',None)
 if n.tag=='img' and 'math' not in n.attrs.get('alt',''):
  if 'height:30px' in n.attrs.get('style',''):
   n.attrs['width']='67.5';n.attrs['height']='22.5';n.attrs['style']='width:67.5px;height:22.5px;'
  elif n.attrs.get('src','').startswith('data:image') and not n.attrs.get('alt','').startswith('\\'):
   raw=base64.b64decode(n.attrs['src'].split(',',1)[1]);im=Image.open(io.BytesIO(raw));w,h=im.size
   scale=min(495/w,600/h)
   n.attrs['width']=str(w*scale);n.attrs['height']=str(h*scale);n.attrs['style']=f'width:{w*scale}px;height:{h*scale}px;'
 if n.tag=='figure':n.attrs['class']='table-float' if any(x.tag=='table' for x in n.walk()) else 'figure-float'
 if n.tag in {'h1','h2','h3','h4'} and 'data-anchor-id' in n.attrs:
  # Section IDs remain on their enclosing sections; heading also supplies outlines.
  pass

CSS=args.stylesheet.read_text()

# CSS preserves fractional point dimensions; HTML width/height attributes truncate.
for n in main.walk():
 if n.tag=='img':
  n.attrs.pop('width',None);n.attrs.pop('height',None)

# Match the HTML's APA caption placement without changing caption text.
for n in main.walk():
 if n.tag=='figure' and any(x.tag=='table' for x in n.walk()):
  captions=[x for x in n.children if isinstance(x,Node) and x.tag=='figcaption']
  if captions:n.children=captions+[x for x in n.children if x not in captions]

blocks=[]
def flatten(n):
 if not isinstance(n,Node):return
 if n.tag=='section':
  for x in n.children:
   if isinstance(x,Node):
    if n.attrs.get('id') and x.tag in ('h1','h2','h3','h4'):
     x.attrs['id']=n.attrs['id']
    flatten(x)
 elif n.tag=='main':
  for x in n.children:flatten(x)
 else:blocks.append(n)
flatten(main)
print('Block count',len(blocks),flush=True)
for index,n in enumerate(main.walk()):
 if n.tag=='a' and 'href' in n.attrs:n.attrs['id']=f'pdf-link-{index}'

render_html=main.html();(TMP/'print-source.html').write_text(render_html)
positions=[];buffer=io.BytesIO();writer=fitz.DocumentWriter(buffer)
page_index=-1;device=None;y=42;page_size=None
portrait=fitz.paper_rect('a4');landscape=fitz.Rect(0,0,portrait.height,portrait.width)
def begin_page(size):
 global page_index,device,y,page_size
 if device:writer.end_page()
 device=writer.begin_page(size);page_index+=1;page_size=size;y=42

def capture(story):
 def record(pos):
  if pos.id or pos.heading:
   positions.append({'page':page_index,'rect':list(pos.rect),'id':pos.id,'heading':pos.heading,'text':pos.text,'open_close':pos.open_close})
 story.element_positions(record)

for index,block in enumerate(blocks):
 html=block.html();floating=any(x.tag in ('table','img') for x in block.walk())
 s=fitz.Story(html,user_css=CSS,em=10.5)
 more,filled=s.place(fitz.Rect(42,42,portrait.width-42,10000))
 filled=fitz.Rect(filled);measured=filled.y1-42
 wide=filled.x1>portrait.width-41
 size=landscape if wide else portrait
 if wide:
  s=fitz.Story(html,user_css=CSS,em=10.5)
  more,filled=s.place(fitz.Rect(42,42,size.width-42,10000));filled=fitz.Rect(filled);measured=filled.y1-42
  if filled.x1>size.width-41:raise RuntimeError(('Too wide',index,filled))
 if not device or size!=page_size or y+min(measured,32)>size.height-45 or (floating and measured<=size.height-87 and y+measured>size.height-45):begin_page(size)
 # Start the cover's first chapter and the two end-matter parts on clean pages.
 if block.tag=='h1' and (block.attrs.get('id') in ('sec-introduction','sec-appendix','sec-supplement') or block.text() in ('Appendix','Electronic Supplement')) and y>42:
  begin_page(size)
 # Headings stay with at least three lines of following prose.
 if block.tag in ('h1','h2','h3','h4') and y+measured+45>size.height-45:begin_page(size)
 s=fitz.Story(html,user_css=CSS,em=10.5)
 while True:
  more,filled=s.place(fitz.Rect(42,y,size.width-42,size.height-45))
  filled=fitz.Rect(filled)
  if filled.x1>size.width-41:raise RuntimeError(('Clipped horizontal',index,filled))
  capture(s);s.draw(device)
  y=filled.y1
  if not more:break
  begin_page(size)
 if index%30==0:print('Placed block',index,'page',page_index+1,flush=True)
writer.end_page();writer.close()
doc=fitz.open('pdf',buffer.getvalue())
anchors={}
for p in positions:
 if p['id'] and not p['id'].startswith('pdf-link-') and p['open_close']&1:
  anchors.setdefault(p['id'],p)
links={n.attrs['id']:n.attrs['href'] for n in main.walk() if n.tag=='a' and 'href' in n.attrs}
link_boxes={}
for p in positions:
 if p['id'] in links and p['open_close']&1 and fitz.Rect(p['rect']).width>0:
  key=(p['page'],p['id'])
  link_boxes[key]=link_boxes.get(key,fitz.Rect(p['rect'])) | fitz.Rect(p['rect'])
unresolved=[]
for (page_number,link_id),rect in link_boxes.items():
 href=links[link_id]
 if href.startswith('#'):
  target=anchors.get(href[1:])
  if target:doc[page_number].insert_link({'kind':fitz.LINK_GOTO,'from':rect,'page':target['page'],'to':fitz.Point(target['rect'][:2])})
  else:unresolved.append(href)
 elif href.startswith(('http://','https://')):
  doc[page_number].insert_link({'kind':fitz.LINK_URI,'from':rect,'uri':href})
 elif href in {'../../preregistration/renders/preregistration.html','preregistration.html'}:
  doc[page_number].insert_link({'kind':fitz.LINK_GOTOR,'from':rect,'file':'preregistration.pdf','page':0})
if unresolved:raise RuntimeError(('Unresolved internal links',sorted(set(unresolved))))
outline=[]
for p in positions:
 if p['heading'] and p['text'] and p['open_close']&1:
  level=min(p['heading'],4)
  if outline:level=min(level,outline[-1][0]+1)
  else:level=1
  outline.append([level,p['text'],p['page']+1])
if outline:doc.set_toc(outline)
for i,page in enumerate(doc):
 page.insert_text((page.rect.width/2-7,page.rect.height-23),str(i+1),fontsize=9,color=(.3,.3,.3))
doc.set_metadata({'title':title,'author':'; '.join(authors),'subject':'Methods and results on synthetic data, Appendix and Electronic Supplement','producer':'Offline MuPDF renderer from accepted HTML'})
doc.save(args.output_pdf,garbage=4,deflate=True)
(args.qa_dir/'positions.json').write_text(json.dumps(positions,indent=2))
print('PDF pages:',len(doc))
for i,page in enumerate(doc):
 page.get_pixmap(matrix=fitz.Matrix(1,1)).save(TMP/f'page-{i+1:03}.png')
for start in range(0,len(doc),12):
 canvas=Image.new('RGB',(3*330,4*460),'#ddd');draw=ImageDraw.Draw(canvas)
 for idx in range(start,min(start+12,len(doc))):
  im=Image.open(TMP/f'page-{idx+1:03}.png');im.thumbnail((310,430))
  x=(idx-start)%3*330+10;y0=(idx-start)//3*460+20
  canvas.paste(im,(x,y0));draw.text((x,y0-15),f'Page {idx+1}',fill='black')
 canvas.save(args.qa_dir/f'contact-{start//12+1:02}.png')
if args.page_images:
 for path in TMP.glob('page-*.png'):
  (args.page_images/path.name).write_bytes(path.read_bytes())

# Verify content and geometry independently of the visual inspection.
def compact(text):return re.sub(r'\s+','',unicodedata.normalize('NFKC',text))
all_text=compact(''.join(page.get_text(clip=fitz.Rect(0,0,page.rect.width,page.rect.height-38)) for page in doc))
missing=[]
checked=0
for node in main.walk():
 if node.tag in ('p','figcaption','h1','h2','h3','h4','td','th'):
  value=compact(node.text())
  if value:
   checked+=1
   if value not in all_text:missing.append((node.tag,node.text()))
if missing:raise RuntimeError(('Missing source text',missing[:20],len(missing)))
bounds=[]
for i,page in enumerate(doc):
 for b in page.get_text('dict')['blocks']:
  if b['type']!=0:continue
  for line in b['lines']:
   for span in line['spans']:
    rect=fitz.Rect(span['bbox'])
    if rect.x0<40 or rect.x1>page.rect.width-40 or rect.y0<39 or (rect.y1>page.rect.height-38 and span['text']!=str(i+1)):
     bounds.append((i+1,span['text'],list(rect)))
if bounds:raise RuntimeError(('Text outside print area',bounds[:20]))
if '\ufffd' in all_text:raise RuntimeError('Replacement glyph found')

# Check every original PNG, including histograms, through PDF alpha encoding.
def pixels(image):
 image=image.convert('RGBA')
 white=Image.new('RGBA',image.size,'white');white.alpha_composite(image)
 return hashlib.sha256(white.convert('RGB').tobytes()).hexdigest()
source_images=[]
parser_original=Parser();parser_original.feed(input_html)
original_main=next(n for n in parser_original.root.walk() if n.tag=='main')
for n in original_main.walk():
 if n.tag=='img':
  uri=n.attrs.get('src','')
  if not uri.startswith('data:image/png;base64,'):raise RuntimeError('Input image must be embedded PNG')
  im=Image.open(io.BytesIO(base64.b64decode(uri.split(',',1)[1])))
  source_images.append({'dimensions':list(im.size),'pixels_sha256':pixels(im),'variable':n.attrs.get('data-variable'),'histogram':'height:30px' in n.attrs.get('style','')})
pdf_images={}
for page in doc:
 for img in page.get_images():
  pix=fitz.Pixmap(doc,img[0])
  if img[1]:pix=fitz.Pixmap(pix,fitz.Pixmap(doc,img[1]))
  pdf_images[img[0]]=Image.open(io.BytesIO(pix.tobytes('png'))).copy()
def white_rgb(image):
 image=image.convert('RGBA');white=Image.new('RGBA',image.size,'white');white.alpha_composite(image)
 return white.convert('RGB')
source_nodes=[n for n in original_main.walk() if n.tag=='img']
image_matches=[]
for i,n in enumerate(source_nodes):
 im=Image.open(io.BytesIO(base64.b64decode(n.attrs['src'].split(',',1)[1])))
 candidates=[]
 for xref,other in pdf_images.items():
  if im.size!=other.size:continue
  diff=ImageChops.difference(white_rgb(im),white_rgb(other))
  maximum=max(pair[1] for pair in diff.getextrema())
  mean=sum(ImageStat.Stat(diff).mean)/3
  candidates.append((mean,maximum,xref))
 if not candidates:raise RuntimeError(('Source image dimensions lost',i,im.size))
 mean,maximum,xref=min(candidates)
 # MuPDF splits PNG alpha into PDF colour and mask streams. Its integer
 # premultiply/unpremultiply round-trip changes antialiased alpha edges by
 # up to two 8-bit channel units. Fully opaque plot pixels match exactly.
 if maximum>2 or mean>.01:raise RuntimeError(('Source image pixel mismatch',i,mean,maximum))
 image_matches.append({'source_image':i+1,'pdf_xref':xref,'maximum_channel_difference':maximum,'mean_channel_difference':mean})
histogram_sizes=[]
histogram_xrefs={match['pdf_xref'] for im,match in zip(source_images,image_matches) if im['histogram']}
for i,page in enumerate(doc):
 for im in page.get_image_info(xrefs=True):
  if im['xref'] in histogram_xrefs:
   rect=fitz.Rect(im['bbox']);histogram_sizes.append({'page':i+1,'width_pt':rect.width,'height_pt':rect.height})
assert len(histogram_sizes)==sum(im['histogram'] for im in source_images)
assert all(abs(im['width_pt']-67.5)<.1 and abs(im['height_pt']-22.5)<.1 for im in histogram_sizes)
manifest={
 'input':str(SOURCE),'input_html_sha256':hashlib.sha256(input_bytes).hexdigest(),
 'source_commit':args.source_commit,'output':str(args.output_pdf.resolve()),
 'pdf_sha256':hashlib.sha256(args.output_pdf.read_bytes()).hexdigest(),
 'page_count':len(doc),'landscape_pages':[i+1 for i,p in enumerate(doc) if p.rect.width>p.rect.height],
 'table_count':sum(n.tag=='table' for n in original_main.walk()),
 'figure_caption_count':sum(n.tag=='figcaption' for n in original_main.walk()),
 'source_images':source_images,'PDF_image_matches':image_matches,'math_sources':[n.text() for n in original_main.walk() if n.tag=='span' and 'math' in n.attrs.get('class','').split()],
 'histogram_display':histogram_sizes,'checked_source_text_fragments':checked,
 'checks':{'source_text_fragments':'passed','embedded_PNG_pixels':'passed: same dimensions; maximum alpha round-trip difference 2/255, mean under 0.01/255','histogram_dimensions':'passed','internal_links':'passed','glyph_and_text_bounds':'passed'},
 'visual_qa':'Pending separate inspection of every contact-sheet page and detailed table/figure pages.',
 'runtimes':{'python':sys.version.split()[0],'pymupdf':fitz.VersionBind,'pillow':Image.__version__,'xelatex':subprocess.check_output(['xelatex','--version'],text=True).splitlines()[0]},
 'required_tools':'Python 3 with PyMuPDF and Pillow; XeLaTeX with standalone.cls and amsmath.sty; results-print.css.',
 'scope':'Offline presentation export of the accepted HTML. No browser, R, renv, targets, model fitting or full pipeline run.',
 'limitations':'Existing plot pixels are preserved, including inherited Figure S3 label overlaps; this export does not regenerate scientific plots.',
}
(args.qa_dir/'manifest.json').write_text(json.dumps(manifest,indent=2)+'\n')
print('Passed: all source text fragments, PNG pixel round-trip checks, histogram sizes, internal links and text/glyph bounds.')
print('PDF:',args.output_pdf)
print('Contact sheets:',args.qa_dir)
scratch.cleanup()
