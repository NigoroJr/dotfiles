from __future__ import unicode_literals
from ptpython.layout import CompletionVisualisation

def configure(repl):
    repl.completion_visualisation = CompletionVisualisation.POP_UP

    repl.show_line_numbers = True

    repl.highlight_matching_parenthesis = True

    repl.prompt_style = "ipython"

    repl.confirm_exit = False

    repl.show_status_bar = False

    repl.use_code_colorscheme("monokai")

    repl.use_ui_colorscheme("blue")
