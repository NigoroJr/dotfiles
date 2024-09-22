autoload -Uz colors && colors

autoload -Uz add-zsh-hook
add-zsh-hook precmd _set_prompt

_set_prompt() {
    if [[ $TERM =~ "256color" ]] || [[ $TERM = "alacritty" ]]; then
        _prompt_random_256
    else
        _prompt_random_16
    fi
}

# Get the status if in git repository {{{
_prompt_git_status() {
    local prompt
    local branch_name staged modified untracked conflicts
    local git_status="$( command git status --porcelain 2>/dev/null )"

    branch_name="$( command git rev-parse --abbrev-ref HEAD 2>/dev/null )"
    # Not in git repository
    if [[ -z $branch_name ]]; then
        return
    # Detached state
    elif [[ $branch_name == HEAD ]]; then
        branch_name="$( command git rev-parse HEAD 2>/dev/null | head -c 7 )"
    fi

    # Clean repository?
    if [[ -z $git_status ]]; then
        echo "%F{046}$branch_name%f"
        return
    fi

    staged="$( echo $git_status | command grep --count "^[MA]." )"
    if (( $staged != 0 )); then
        prompt+="%F{082}S%f$staged "
    fi

    modified="$( echo $git_status | command grep --count "^.M" )"
    if (( $modified != 0 )); then
        prompt+="%F{196}M%f$modified "
    fi

    untracked="$( echo $git_status | command grep --count "^??" )"
    if (( $untracked != 0 )); then
        prompt+="%F{099}U%f$untracked "
    fi

    conflicts="$( echo $git_status | command grep --count "^UU" )"
    if (( $conflicts != 0 )); then
        prompt+="%F{190}C%f$conflicts "
    fi

    # if [[ -n $( command git status | grep "^rebase in progress" ) ]]; then
    #     # Rebase is in progress
    #     prompt+="%F{039}REBASE%f"
    # else
    #     prompt+="%F{197}$branch_name%f"
    # fi
    prompt+="%F{197}$branch_name%f"

    echo $prompt
}
# }}}

_mise_get_version() {
    local what="$1"

    mise ls "$what" --no-header --current 2>/dev/null | awk '{ print $2 }'
}

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
    local front

    # Change hostname on remote login
    if [[ -n $REMOTEHOST ]] || [[ -n $SSH_CONNECTION ]]; then
        front="%F{207}%m%f"
    else
        front="%F{222}%m%f"
    fi

    local -a valid_colors
    valid_colors=(
        {001..007}
        {009..015}
        {022..051}
        {058..087}
        {094..159}
        {161..231}
        {242..255}
    )
    # Needed so that $RANDOM is referenced from parent shell not subshell
    local rnd="$RANDOM"
    local arrow="$( _prompt_arrow $rnd $valid_colors )"

    # Change color depending on exit status of previous command
    if [[ -n $TMUX ]]; then
        sign=">>"
    else
        sign=" >"
    fi
    local exit_stat="%0(?,%F{046}$sign%f,%20(?,%F{046}$sign%f,%F{196}$sign%f))"

    # Construct prompt with arrow
    local arrow_prompt="$front$arrow%F{045}%n%f$exit_stat "

    # Currenty directory
    # local pwd_prompt="%(10~|%-3~/.../%3~|%~)"
    local pwd_prompt="%~"
    local pwd_length=${(c)#${(%)pwd_prompt}}
    # If too long, abbreviate (4 characters for the "[  ]")
    local max_length=$(( ${COLUMNS:-80} - 4 ))
    if (( $pwd_length >= $max_length )); then
        local -a full_path
        full_path=( ${(s:/:)${(%)pwd_prompt}} )

        local -a abbrev_path
        for dir in ${full_path[0,-2]}; do
            if (( ${(c)#dir} > 3 )); then
                abbrev_path+="%B$( echo $dir | head -c 3 )%b"
            else
                abbrev_path+="$dir"
            fi
        done
        abbrev_path+="${full_path[-1]}"

        pwd_prompt="${(j:/:)abbrev_path}"
    fi
    pwd_prompt="[ %F{051}${pwd_prompt}%f ]"

    # Additional helpful info
    local -a additional_prompts
    # Git repository info
    local git_prompt="$( _prompt_git_status )"
    if [[ -n $git_prompt ]]; then
        additional_prompts+="$git_prompt"
    fi

    if (( $+commands[mise] )); then
        # Python
        local python_version="$( _mise_get_version python )"
        if [[ -n $python_version ]]; then
            additional_prompts+="%F{069}py:$python_version${VIRTUAL_ENV:+ venv}%f"
        fi
        # Ruby
        local ruby_version="$( _mise_get_version ruby )"
        if [[ -n $ruby_version ]]; then
            additional_prompts+="%F{124}rb:$ruby_version%f"
        fi
        # Node
        local node_version="$( _mise_get_version node )"
        if [[ -n $node_version ]]; then
            additional_prompts+="%F{028}js:$node_version%f"
        fi
    else
        # Python info
        if [[ -n $VIRTUAL_ENV ]]; then
            additional_prompts+="$( basename $VIRTUAL_ENV )"
        elif (( $+commands[pyenv] )); then
            local pyenv_prompt="$( pyenv version-name )"
            if [[ $pyenv_prompt != system ]]; then
                additional_prompts+="$pyenv_prompt"
            fi
        elif [[ -n $CONDA_DEFAULT_ENV ]]; then
            additional_prompts+="$CONDA_DEFAULT_ENV"
        fi
    fi

    # Custom prompt
    if [[ -n $ADDITIONAL_PROMPT ]]; then
        additional_prompts+="$( eval "$ADDITIONAL_PROMPT" )"
    fi

    local prefix_prompt=$'\n'
    if (( $#additional_prompts > 0 )); then
        prefix_prompt+="${(j: | :)additional_prompts[@]}"$'\n'
    fi

    PROMPT="${prefix_prompt}${pwd_prompt}"$'\n'"${arrow_prompt}"
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
        sign=">>"
    else
        sign=" >"
    fi
    exit_stat="%0(?,%F{green}%B$sign%b%f,%20(?,%F{green}%B$sign%b%f,%F{red}%B$sign%b%f))"

    PROMPT="%m$arrow%F{blue}%B%n%b%f$exit_stat "
}
# }}}
