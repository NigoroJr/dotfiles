from __future__ import unicode_literals
from pygments.token import Token
from ptpython.layout import CompletionVisualisation

def configure(repl):
    repl.completion_visualisation = CompletionVisualisation.POP_UP

    repl.show_line_numbers = True

    repl.highlight_matching_parenthesis = True

    repl.prompt_style = 'ipython'

    repl.confirm_exit = False

    repl.use_code_colorscheme('monokai')

    repl.use_ui_colorscheme('blue')

_custom_ui_colorscheme = {
    Token.Layout.Prompt: 'bg:#eeeeff #000000 bold',
}
