from podparser.parser import Parser
from utils import append_new_line
import re

p = Parser(config='1861-1862/config', dir_path='1861-1862/', verbose = False, start = 71, end = 340)
# p = Parser(config='1861-1862/config', dir_path='1861-1862/', verbose = False, start = 340, end = 340)
dir = p.run_parser()
for page in dir.pages:
    for entry in page.entries:
        entry = re.sub(r"\n$", "", str(entry))
        entry = "Page {0}\t".format(page.number) + entry
        append_new_line('1861-1862/general-directory.txt', entry)
