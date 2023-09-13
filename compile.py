# Copyright (c) 2023 Diana Bendun
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

import bs4
import re
from slugify import slugify
import math
import subprocess

def inject_table_of_contents(soup: bs4.BeautifulSoup):
    toc = []

    for header in soup.find_all(re.compile('^h[2-3]')):
        slug = slugify(header.string)
        header.attrs['id'] = slug
        if 'skip-toc' not in header.attrs.get('class', []):
            toc.append((int(header.name[1:]), header.string, slug))

    assert len(set(name for _, _, name in toc)) == len(toc), "Non unique slugs"

    levels = [0]

    text = ""
    for level, content, name in toc:
        if levels[-1] < level:
            text += "<ol>\n"
            levels.append(level)
        if levels[-1] > level:
            text += "</ol>\n"
            levels.pop()
        text += f'<li><a href="#{name}">{content}</li>\n'

    text += "</ol>"
    toc_html = bs4.BeautifulSoup(text, 'html.parser')
    for toc in soup.find_all(id='toc'):
        toc.replace_with(toc_html)

def time_to_read(soup: bs4.BeautifulSoup):
    WORD_LENGTH = 5
    WPM = 200

    def is_visible(node: bs4.Tag) -> bool:
        return (
            node.parent.name not in ['style', 'script', '[document]', 'head']
            and not isinstance(node, bs4.element.Comment)
            and not node.string.isspace()
        )

    visible_text = filter(is_visible, soup.find_all(string=True))
    minutes = math.ceil(sum(len(text.string.strip())/WORD_LENGTH for text in visible_text) / WPM)
    for ttr in soup.find_all(id='time-to-read'):
        ttr.replace_with(f"{minutes}")

def main():
    subprocess.run(["dot", "-Tpng", "-O", "sentances.dot"], check=True)

    with open('source.html') as f:
        source = f.read()

    soup = bs4.BeautifulSoup(source, 'html.parser')
    inject_table_of_contents(soup)
    time_to_read(soup)

    with open('all.html', 'w') as f:
        print(soup.prettify(), file=f)

if __name__ == "__main__":
    main()
