# Read Source File
if [ -f ~/.aliases ]; then
    source ~/.aliases
fi

# Settings for prompt
if [ -f ~/.zshrc_prompt ]; then
    source ~/.zshrc_prompt
fi

# See if it's Mac
case `uname -s` in
    # Macintosh
    Darwin)
        #PATH=$PATH:$(brew --prefix coreutils)/libexec/gnubin
        local gnubin=$(brew --prefix coreutils)/libexec/gnubin
        PATH=$PATH:$gnubin
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

# add android sdk to PATH
# local ANDROID_SDK=$HOME/pkg_src/adt-bundle-linux/sdk
local ANDROID_SDK=/usr/local/src/adt-bundle/sdk
export PATH=$PATH:$ANDROID_SDK/tools:$ANDROID_SDK/platform-tools

zstyle :compinstall filename '/home/naoki/.zshrc'

autoload -Uz compinit
compinit

HISTFILE=~/.histfile
HISTSIZE=100000
SAVEHIST=100000

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
