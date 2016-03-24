#!/usr/bin/env zsh

psg() {
    ps aux | grep "\(^USER\|$1\)" | grep -v grep
}

psgz() {
    local -a zombies=( $( ps aux | awk '/ Z / && !/awk/ { print $2 }' ) )
    local -a zombies_exp
    if (( $#zombies != 0 )); then
        zombies_exp=( "\b${^zombies[@]}\b" )
        ps auxf | grep -v grep | grep -B 4 "${(F)zombies_exp}"
    fi
}

mcm() {
    local threads="${1:-1}"
    make clean && make -j"$threads"
}

# Create and go into that directory
mcd() {
    local dir="${1:-tmp}"
    mkdir -p "$dir" && cd "$dir"
}
compdef _mkdir mcd

sudo() {
    if [[ $1 == -- ]]; then
        shift
        command sudo "$@"
    elif [[ $1 =~ ^vim?$ ]] && (( $+commands[sudoedit] )); then
        shift
        command sudoedit "$@"
    else
        command sudo "$@"
    fi
}

# Add upstream for a repository hosted on github.com
grau() {
    local cmd username repo_name upstream

    username="$1"

    if [[ -z $username ]]; then
        echo 'Need username for upstream' 1>&2
        return 1
    fi

    if (( $+commands[hub] )); then
        cmd=hub
        upstream=$username
    elif (( $+commands[git] )); then
        cmd=git
        repo_name=$( git remote show -n origin | \
            awk -F '/' '/Fetch/ { print $NF }' )
        upstream="https://github.com/$username/$repo_name"
    else
        echo "You don't even have git!" 1>&2
        return 1
    fi

    command $cmd remote add upstream $upstream && \
        echo "Added $username${repo_name:+/$repo_name} as upstream"
}

mkmv() {
    mkdir -p $@[$#] && mv $@
}

dumd() {
    local depth="$1"
    shift
    du -h --max-depth="$depth" $@
}

unzip() {
    local old_pwd="$PWD"
    local tmp_dir
    local final_dir
    local -a raw_args

    if [[ $@ =~ -(l|z) ]]; then
        command unzip $@
        return $status
    fi

    tmp_dir="${$( mktemp -d -p . ):A}"

    # Convert file paths to absolute paths
    for arg in $@; do
        if ! [[ $arg =~ ^- ]]; then
            [[ -e $arg ]] && arg="$arg:A"
            final_dir=${final_dir:-$arg:t:r:r}
        fi
        raw_args+=( "$arg" )
    done

    (
        local dir_pat="*(/)"
        local -a curr_dir_dirs

        builtin cd -q "$tmp_dir"
        command unzip $raw_args
        curr_dir_dirs=( ${~dir_pat} )

        if (( $#curr_dir_dirs == 1 )) && [[ $curr_dir_dirs[1] != $dir_pat ]]; then
            mv "$curr_dir_dirs[1]" "$old_pwd"
            rm -rf "$tmp_dir:A"
        else
            mv "$tmp_dir" "$old_pwd/$final_dir"
        fi
    )

    builtin cd -q "$old_pwd"
}
