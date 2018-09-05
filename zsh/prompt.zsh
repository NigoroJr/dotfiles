autoload -Uz colors && colors

autoload -Uz add-zsh-hook
add-zsh-hook precmd _set_prompt

_set_prompt() {
    if [[ $TERM =~ "256color" ]]; then
        _prompt_random_256
    else
        _prompt_random_16
    fi
}

# Get the status if in git repository {{{
_prompt_git_status() {
    local prompt
    local branch_name staged modified untracked conflicts

    branch_name="$( git rev-parse --abbrev-ref HEAD 2>/dev/null )"
    # Not in git repository
    if [[ -z $branch_name ]]; then
        return
    # Detached state
    elif [[ $branch_name == HEAD ]]; then
        branch_name="$( git rev-parse HEAD | head -c 7 )"
    fi

    # Clean repository?
    if [[ -z $( git status --short 2>/dev/null ) ]]; then
        echo "%F{046}$branch_name%f"
        return
    fi

    staged="$( git diff --cached --numstat | command wc -l | tr -d ' ' )"
    if [[ $staged -ne 0 ]]; then
        prompt+="%F{010}S%f$staged "
    fi

    modified="$( git diff --numstat | command wc -l | tr -d ' ' )"
    if [[ $modified -ne 0 ]]; then
        prompt+="%F{009}M%f$modified "
    fi

    untracked="$( git status --short | command grep '^\s*??' | command wc -l | tr -d ' ' )"
    if [[ $untracked -ne 0 ]]; then
        prompt+="%F{099}U%f$untracked "
    fi

    conflicts="$( git status --short | command grep '^\s*UU' | command wc -l | tr -d ' ' )"
    if [[ $conflicts -ne 0 ]]; then
        prompt+="%F{190}C%f$conflicts "
    fi

    if [[ -n $( git status | grep '^rebase in progress' ) ]]; then
        # Rebase is in progress
        prompt+="%F{039}REBASE%f"
    else
        prompt+="%F{197}$branch_name%f"
    fi

    echo $prompt
}
# }}}

_prompt_arrow() {
    local rnd=$1
    shift
    local -a valid_colors
    valid_colors=($@)
    local ind=$(( $rnd % ${#valid_colors[@]} + 1))
    local arrow="%F{${valid_colors[$ind]}}->%f"

    echo $arrow
}

# Randomly selects a 256 color to use for the arrow {{{
_prompt_random_256() {
    local front arrow rnd exit_stat l_prompt
    local -a valid_colors

    # Change hostname on remote login
    if [[ -n $REMOTEHOST ]] || [[ -n $SSH_CONNECTION ]]; then
        front="%F{207}%m%f"
    else
        front="%F{222}%m%f"
    fi

    valid_colors=({001..015} {022..051} {058..087} {094..159} {161..231} {242..255})
    # Needed so that $RANDOM is referenced from parent shell not subshell
    rnd=$RANDOM
    arrow=$( _prompt_arrow $rnd $valid_colors )

    # Change color depending on exit status of previous command
    if [[ -n $TMUX ]]; then
        sign='>>'
    else
        sign=' >'
    fi
    exit_stat="%0(?,%F{046}$sign%f,%20(?,%F{046}$sign%f,%F{009}$sign%f))"

    # Construct left prompt
    l_prompt="$front$arrow%F{45}%n%f$exit_stat "

    # Display in two lines if too long
    local pwd_length=${(c)#${(D)PWD}}
    if [[ $pwd_length -ge 45 ]]; then
        PROMPT="[ %F{219}%~%f ]"$'\n'"$l_prompt"
    else
        PROMPT="$l_prompt"
    fi

    local -a prompts
    # Show git repository info
    local git_prompt="$( _prompt_git_status )"
    if [[ -n $git_prompt ]]; then
        prompts+="$git_prompt"
    fi
    if (( $+commands[pyenv] )); then
        local pyenv_prompt="$( pyenv version-name )"
        if [[ $pyenv_prompt != system ]]; then
            prompts+="$pyenv_prompt"
        fi
    fi

    unset RPROMPT
    if (( $#prompts > 0 )); then
        RPROMPT="(${(j: | :)prompts})"
    fi

    # Right prompt
    if [[ $pwd_length -lt 45 ]]; then
        RPROMPT+=" [ %F{051}%~%f ]"
    fi
}
# }}}

# Selects a random color for the arrow {{{
_prompt_random_16() {
    local front arrow rnd sign exit_stat
    local -a valid_colors

    front="%m$arrow%F{blue}%B%n%b%f"

    valid_colors=("grey" "magenta" "white" "red" "green" "yellow" "cyan")
    rnd=$RANDOM
    arrow=$( _prompt_arrow $rnd $valid_colors )

    # Change color depending on exit status of previous command
    if [[ -n $TMUX ]]; then
        sign='>>'
    else
        sign=' >'
    fi
    exit_stat="%0(?,%F{green}%B$sign%b%f,%20(?,%F{green}%B$sign%b%f,%F{red}%B$sign%b%f))"

    PROMPT="%m$arrow%F{blue}%B%n%b%f$exit_stat "
}
# }}}
