# vim: foldmethod=marker
# The local .zshrc (.zshrc_local) is read at the end

# Read Source File
if [ -f ~/.aliases ]; then
    source ~/.aliases
fi

# Settings for prompt
if [ -f ~/.zshrc_prompt ]; then
    source ~/.zshrc_prompt
fi

# ls colors {{{
case `uname -s` in
    # Macintosh
    Darwin)
        #PATH=$PATH:$(brew --prefix coreutils)/libexec/gnubin
        local PATH_TO_BREW=`which brew | awk '{ print $1 }'`
        if [ $PATH_TO_BREW != brew ]; then
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

# Set EDITOR to my *favorite* editor
export EDITOR="vim"

# Change the PATH according to whether the user is root or not
# Copied from /etc/profile
if [ "$EUID" = "0" ] || [ "$USER" = "root" ] ; then
	PATH="/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:${ROOTPATH}"
else
	PATH="/usr/local/bin:/usr/bin:/bin:${PATH}"
fi

# Add adt-bundle to PATH if the directory exists
if [ -d ~/pkg_src/adt-bundle ]; then
    # add android sdk to PATH
    local ANDROID_SDK=$HOME/pkg_src/adt-bundle/sdk
    # local ANDROID_SDK=/usr/local/src/adt-bundle/sdk
    export PATH=$PATH:$ANDROID_SDK/tools:$ANDROID_SDK/platform-tools
fi

# Useful when using git
# Copied from: http://subtech.g.hatena.ne.jp/cho45/20080617/1213629154 {{{
typeset -A heads
heads=(
"HEAD^"     "HEAD\\^"
"HEAD^^"    "HEAD\\^\\^"
"HEAD^^^"   "HEAD\\^\\^\\^"
"HEAD^^^^"  "HEAD\\^\\^\\^\\^"
"HEAD^^^^^" "HEAD\\^\\^\\^\\^\\^"
)
magic-abbrev-expand () {
    local MATCH
    LBUFFER=${LBUFFER%%(#m)[-_a-zA-Z0-9^]#}
    LBUFFER+=${heads[$MATCH]:-$MATCH}
}

magic-abbrev-expand-and-insert () {
    magic-abbrev-expand
    zle self-insert
}

magic-abbrev-expand-and-accept () {
    magic-abbrev-expand
    zle accept-line
}

no-magic-abbrev-expand () {
    LBUFFER+=' '
}

zle -N magic-abbrev-expand
zle -N magic-abbrev-expand-and-insert
zle -N magic-abbrev-expand-and-accept
zle -N no-magic-abbrev-expand
bindkey "\r"  magic-abbrev-expand-and-accept # Can't M-x RET
bindkey "^J"  accept-line # no magic
bindkey " "   magic-abbrev-expand-and-insert
bindkey "."   magic-abbrev-expand-and-insert
bindkey "^x " no-magic-abbrev-expand
# }}}

zstyle :compinstall filename "$HOME/.zshrc"

autoload -Uz compinit
compinit

HISTFILE=~/.histfile
HISTSIZE=6000000
SAVEHIST=6000000

# Limit Coredump size
limit coredumpsize 102400

# Show even when no Return at the end
unsetopt promptcr

# emacs keybind
bindkey -e

# use colors
setopt prompt_subst

# don't beep
setopt nobeep

# set jobs -l as the output for jobs
setopt long_list_jobs

# resume when susupended command is entered
setopt auto_resume

# show list of completion
setopt auto_list

# Suggest correction
setopt correct

# show file types
setopt list_types

# compact list
setopt list_packed

# don't add history when it's the same command as previous
setopt hist_ignore_dups

# auto push when cd
setopt auto_pushd

# but don't push the same directory
setopt pushd_ignore_dups

# use # ~ ^ as regex
setopt extended_glob

# don't switch completion with Tab
unsetopt auto_menu

# edit before running history
setopt hist_verify

# completion after =/usr etc.
setopt magic_equal_subst

# sort files numerically
setopt numeric_glob_sort

# only directories when cd
setopt auto_cd

# complete / automatically
setopt auto_param_slash

# complete variable
setopt auto_param_keys

# don't remove last / automatically
unsetopt auto_remove_slash

# share history
setopt share_history

# don't add to history when command starts with ' ' (space)
setopt hist_ignore_space

# enable cursor selection
zstyle ':completion:*:default' menu select=1

# Read local enviroment File if there is one
if [ -f ~/.zshrc_local ]; then
    source ~/.zshrc_local
fi

# search history
autoload history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end
bindkey "" history-beginning-search-backward-end
bindkey "" history-beginning-search-forward-end
bindkey '' history-incremental-pattern-search-backward
bindkey 's' history-incremental-pattern-search-forward

