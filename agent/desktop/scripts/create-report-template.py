from pathlib import Path
from docx import Document
from docx.shared import Cm, Pt, RGBColor
from docx.oxml import OxmlElement
from docx.oxml.ns import qn
doc=Document()
section=doc.sections[0]
section.page_width=Cm(21);section.page_height=Cm(29.7)
section.left_margin=section.right_margin=Cm(2)
section.top_margin=section.bottom_margin=Cm(2)
for name,size in [('Normal',10.5),('Title',22),('Heading 1',15),('Heading 2',12),('Heading 3',11)]:
    style=doc.styles[name];style.font.name='Microsoft YaHei';style.font.size=Pt(size);style.font.color.rgb=RGBColor(0,0,0)
    style.element.get_or_add_rPr().get_or_add_rFonts().set(qn('w:eastAsia'),'Microsoft YaHei')
    style.paragraph_format.space_after=Pt(7)
    if name!='Normal':
        style.paragraph_format.keep_with_next=True;style.paragraph_format.space_before=Pt(12)
    else:style.paragraph_format.line_spacing=1.2
style=doc.styles['Table Grid'];pr=OxmlElement('w:tblPr');borders=OxmlElement('w:tblBorders')
for side in ['top','left','bottom','right','insideH','insideV']:
    b=OxmlElement('w:'+side);b.set(qn('w:val'),'single');b.set(qn('w:sz'),'4');b.set(qn('w:color'),'D9D9D9');borders.append(b)
pr.append(borders);style.element.append(pr)
target=Path(__file__).resolve().parents[1]/'backend/report-template.docx'
for border in doc.styles.element.findall('.//'+qn('w:pBdr')):
    border.getparent().remove(border)
doc.save(target)
print(target)
