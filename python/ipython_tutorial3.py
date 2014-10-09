# -*- coding: utf-8 -*-
"""
Created on Thu Oct  9 13:25:43 2014

@author: ivan
"""

from IPython.core.magic import (register_line_magic, 
                                register_cell_magic)

@register_line_magic
def hello(line):
    if line == 'french':
        print("Salut tout le monde!")
    else:
        print("Hello world!")
%hello french

import pandas as pd
#from StringIO import StringIO  # Python 2
from io import StringIO  # Python 3

@register_cell_magic
def csv(line, cell):
    # We create a string buffer containing the
    # contents of the cell.
    sio = StringIO(cell)
    # We use Pandas' read_csv function to parse
    # the CSV string.
    return pd.read_csv(sio)
    
%%csv
col1,col2,col3
0,1,2
3,4,5
7,8,9

df = _
df.describe()

%%writefile csvmagic.py
import pandas as pd
#from StringIO import StringIO  # Python 2
from io import StringIO  # Python 3

def csv(line, cell):
    sio = StringIO(cell)
    return pd.read_csv(sio)

def load_ipython_extension(ipython):
    """This function is called when the extension is loaded.
    It accepts an IPython InteractiveShell instance.
    We can register the magic with the `register_magic_function`
    method of the shell instance."""
    ipython.register_magic_function(csv, 'cell')
    
%load_ext csvmagic

%%csv
col1,col2,col3
0,1,2
3,4,5
7,8,9
