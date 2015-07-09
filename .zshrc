# vim: foldmethod=marker ft=zsh
# The local .zshrc is read at the end

# Enable completion
zstyle :compinstall filename "$HOME/.zshrc"

autoload -Uz compinit && compinit

# Read aliases
if [ -f ~/.aliases ]; then
    source ~/.aliases
fi

# Compile .zshrc if necessary
if [ ! -f ~/.zshrc.zwc -o ~/.zshrc -nt ~/.zshrc.zwc ]; then
    zcompile ~/.zshrc
fi

# ls colors {{{
case `uname -s` in
    Darwin)
        if hash brew 2>/dev/null; then
            local gnubin=$(brew --prefix coreutils)/libexec/gnubin
            PATH=$PATH:$gnubin
        fi
        # No dircolors
        if [ -f $gnubin/dircolors ]; then
            # colorize list
            eval `dircolors`
            export ZLS_COLORS=$LS_COLORS
            zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}

            alias ls='gls --color=auto'
        fi
        ;;
    *)
        # colorize list
        eval `dircolors`
        export ZLS_COLORS=$LS_COLORS
        zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
        ;;
esac
# }}}

# Go completion
if [ -f ~/.zshcomp_go ]; then
    source ~/.zshcomp_go
elif hash go 2>/dev/null && [ -f `go env GOROOT`/misc/zsh/go ]; then
    source `go env GOROOT`/misc/zsh/go
fi

# In tmux, don't show status bar if there's only one window
if [[ -n $TMUX ]]; then
    if [ $(tmux list-windows | wc -l) -eq 1 ]; then
        tmux set-option status off >/dev/null
    else
        tmux set-option status on >/dev/null
    fi
fi

# enable cursor selection
zstyle ':completion:*:default' menu select=1
zstyle ':completion:*:(processes|jobs)' menu yes select=2

# Limit Coredump size
limit coredumpsize 102400

# Options {{{
setopt auto_cd              # cd when only directory is entered
setopt auto_list            # Show list of completion
setopt auto_name_dirs       # Use named dirs automatically
setopt auto_param_keys      # Complete variables
setopt auto_param_slash     # Complete / automatically
setopt auto_pushd
setopt auto_remove_slash    # Remove trailing / automatically
setopt auto_resume          # Resume when suspended command is entered
setopt cdable_vars          # cd to named dirs without ~ at beginning
setopt correct              # Suggest correction
setopt extended_glob
setopt hist_ignore_all_dups
setopt hist_ignore_space    # Don't add commands that start with space
setopt hist_reduce_blanks
setopt hist_save_no_dups    # Add only last command on duplicate
setopt hist_verify          # Edit before running history
setopt list_packed          # Compact list
setopt list_types           # Show file types
setopt long_list_jobs       # Set jobs -l as the output for jobs
setopt magic_equal_subst    # Completion like --prefix=/usr etc.
setopt nobeep
setopt noflowcontrol        # C-s for incremental forward search
setopt no_nomatch           # Allow things like HEAD^^ in Git repositories
setopt numeric_glob_sort
setopt prompt_subst
setopt pushd_ignore_dups    # Don't pushd the same directory
setopt share_history
unsetopt auto_menu          # Don't change completion with Tab
unsetopt promptcr           # Show lines without trailing \n
# }}}

# Key bindings {{{
# emacs keybinding
bindkey -e
bindkey -M emacs 'n' down-line-or-history
bindkey -M emacs 'p' up-line-or-history

# Better history search
autoload history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end
bindkey '' history-beginning-search-backward-end
bindkey '' history-beginning-search-forward-end

# Incremental backward search starting from previously executed command {{{
history-incremental-pattern-search-backward-start-from-previous() {
    CMD=$BUFFER
    zle up-history
    zle history-incremental-pattern-search-backward

    # Was aborted
    # Either works
    #if (( #KEYS == ##\C-g )) || (( #KEYS == ##\C-c )) || (( #KEYS == ##\C-\\ )); then
    if [ "$KEYS" == "" ] || [ "$KEYS" == "" ] || [ "$KEYS" == "" ]; then
        BUFFER=$CMD
    fi
}
zle -N history-incremental-pattern-search-backward-start-from-previous
# }}}

# bindkey '' history-incremental-pattern-search-backward
bindkey '' history-incremental-pattern-search-backward-start-from-previous
bindkey 's' history-incremental-pattern-search-forward

bindkey -M isearch '' backward-kill-word
bindkey -M isearch '' history-incremental-pattern-search-backward

bindkey '[Z' reverse-menu-complete # S-Tab to reverse traverse completion

#bindkey -s 'a' " | awk '{ print $ }'"

# }}}

# Functions {{{
# Create and go into that directory
mcd() {
    mkdir $@ && cd $_
}
compdef _mkdir mcd

# Brace expansion with completion. For example,
#
#   mv ~/{foo,bar}_
#
# pressing <Tab> when the cursor is at _ expands to
#
#   mv ~/foo ~/bar _
#
# even if there's still more path to type. With this function, one can type
#
#   mv ${=$( bre foo bar _
#
# and get the same completion as foo_ while having the same expansion as
#
#   {foo,bar}_
#
# . However, the limitation is that this function does not know about the
# outer array expansion such as:
#
#   mv ~/${^${ bre foo bar _
#
# and thus, needs to be used like:
#
#   mv $( bre ~/foo ~/bar _
#
# otherwise, it will show the completions for the current directory in the
# previous example.
#
# Also, note that putting a strange value as the first argument like
#
#   bre "$( rm -rf ~ )" _
#
# and calling the completion function is dangerous.
#
# http://stackoverflow.com/a/27485157/2580849

bre() {
    local len last
    local -a args
    # first=${@[1]}
    # shift
    # args=($@)
    len=$(( $# - 1 ))
    args=(${@:1:$len})
    last=${@: -1}

    for arg in $args; do
        # echo "$first$arg"
        echo "$arg$last"
    done
}

_bre() {
    local context state line ret=1
    local -a matching

    _arguments \
        '1: :->none' \
        '*: :->rest' \
        && ret=0

    case $state; in
        none)
            _arguments \
                '1:file:_files' \
                && ret=0
            ;;
        rest)
            eval first=$line[1]
            # Use a different (and most of the times safe) delimeter
            matching=($( eval "ls -d $first*" | sed -e "s|$first||" ))
            _arguments \
                "*:arg:($matching)" \
                && ret=0
                # "*:files:_files -g '(click*):s/click//'" \
                # "*:files:_files -g '${line[1]}*:s/${line[1]}//'" \
                ;;
    esac

    return ret
}

compdef _bre bre
# }}}

# Settings for prompt
if [ -f ~/.zshrc_prompt ]; then
    source ~/.zshrc_prompt
fi

# Read local configs if any
if [ -f ~/.localrc/zshrc ]; then
    source ~/.localrc/zshrc
fi
