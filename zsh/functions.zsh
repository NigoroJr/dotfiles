#!/usr/bin/env zsh

psg() {
    ps aux | grep "\(^USER\|$1\)" | grep -v grep
}

psgz() {
    local cmd='ps auxf'
    local -a zombies
    local -a zombies_exp

    zombies=( $( ps aux | awk '/ Z / && !/awk/ { print $2 }' ) )

    if [[ ${(L)"$( uname -s )"} == darwin ]]; then
        if (( ! $+commands[pstree] )); then
            echo 'pstree not found' >&2
            return 1
        fi

        cmd='pstree'
    fi

    if (( $#zombies != 0 )); then
        zombies_exp=( "\b${^zombies[@]}\b" )
        eval "$cmd" | grep -v grep | grep -B 4 "${(F)zombies_exp}"
    fi
}

mcm() {
    local threads="${1:-5}"
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

viwhich() {
    vi "$( which "$1" )"
}
compdef _command viwhich

mkrar() {
    local src_name="$1"

    if ! (( $+commands[rar] )); then
        echo "mkrar: rar command not found" >&2
        return 127
    elif ! [[ -d $src_name ]]; then
        echo "mkrar: $src_name is not a directory" >&2
        return 1
    fi

    rar a "$src_name.rar" "$src_name" \
        && rm -r "$src_name"
}
compdef _directories mkrar

trim() {
    local -a ins
    if [[ -p /dev/stdin ]]; then
        ins=( '-' )
    elif (( $#argv >= 1 )); then
        ins=( ${(@)argv[@]} )
    fi

    zsh -c "cat $ins" | while read line; do
        echo "${${line## #}%% #}"
    done
}

gcl() {
    local repo
    local website
    local url
    local clone_path_prefix="$HOME/src/clones"
    local clone_path
    local cd_in=false
    local into_src=false

    while getopts 'hp:cs' flag; do
        case "$flag" in
            p)
                clone_path_prefix="$OPTARG"
                ;;
            c)
                cd_in=true
                ;;
            s)
                into_src=true
                ;;
            h)
                echo "Usage: $0 [-h] [-s] [-c] [-p <prefix>] <repo> [website]"
                return 0
        esac

    done
    shift $(( $OPTIND - 1 ))

    repo="$1"
    website="${2:-github}"

    if (( $+commands[git] == 0 )); then
        echo "gcl: command not found: git" 2>&1
        return 1
    fi

    if [[ -z $repo ]]; then
        echo "gcl: repository not given" 2>&1
        return 1
    fi

    case "$website" in
        github)
            url="https://github.com"
            ;;
        bitbucket)
            url="https://bitbucket.org"
            ;;
        gist)
            url="https://gist.github.com"
            ;;
        *)
            echo "$website Didn't match anything"
            return 1
    esac

    url="$url/$repo"
    if $into_src; then
        clone_path="./src/$repo"
    else
        clone_path="$clone_path_prefix/$repo"
    fi

    git clone "$url" "$clone_path"

    if (( $status == 0 )) && $cd_in; then
        builtin cd "$clone_path"
    fi
}

mkvi() {
    local full_path="$1"
    local dirname="${full_path:h}"

    if [[ ! -d $dirname ]]; then
        mkdir -p "$dirname"
    fi

    vi "$full_path"
}

mount() {
    if ! [[ -f ~/.mount-commands ]]; then
        command mount ${argv[@]}
        return
    fi

    while getopts 'l' flag; do
        case "$flag" in
            l)
                awk -F:= '{ print $1 ": " $2 }' ~/.mount-commands
                return
                ;;
        esac

    done
    shift $(( $OPTIND - 1 ))

    local -A mount_args
    while read line; do
        if [[ $line =~ '^[[:blank:]]*#' ]]; then
            continue
        fi
        local -a mount_arg
        mount_arg=( ${(s!:=!)line} )
        mount_arg[1]="$( echo $mount_arg[1] | awk '{ $1 = $1; print }' )"
        mount_arg[2]="$( echo $mount_arg[2] | awk '{ $1 = $1; print }' )"
        mount_args[$mount_arg[1]]=$mount_arg[2]
    done < ~/.mount-commands

    if (( $#argv == 0 )); then
        command mount
    elif (( $#argv == 1 )); then
        local subcommand="$1"
        if (( ${mount_args[(I)$subcommand]} )); then
            echo "Invalid command: $subcommand" >&2
            return
        fi
        sudo mount ${(s: :)${mount_args[$subcommand]}}
    else
        command mount ${argv[@]}
    fi

}

swapmv() {
    local a="$1"
    local b="$2"
    local tmp_name="$( mktemp -p ${a:h} )"

    mv "$a" "$tmp_name" && \
        mv "$b" "$a" && \
        mv "$tmp_name" "$b"
}

trn() {
    local new_name="$1"

    if [[ -z $new_name ]]; then
        new_name="${SHELL:t}"
    fi

    tmux rename-window "$new_name"
}

ghpr() {
    local rem_name="$1"
    local pr_id="$2"
    local gh_url
    local branch

    gh_url="$( command git remote get-url ${rem_name} )"

    if [[ -z ${gh_url} ]]; then
        return 1
    fi

    if [[ -n $( echo "$gh_url" | grep -o '.*@.*:.*/.*' ) ]]; then
        host_name_repo="$( echo "$gh_url" | sed -e 's/^.*@\(.*\):\(.*\)\/\(.*\)$/\1\/\2\/\3/' )"
        gh_url="https://${host_name_repo}"
    fi

    branch="$( grep 'head-ref' =( curl -sL ${gh_url}/pull/${pr_id} ) | \
        egrep -o '<span class="css-truncate-target">[^<]+</span>' | \
        tail -n 1 | \
        egrep -o '>[^<]+<' | \
        tr -d '<>' )"

    if [[ -z ${branch} ]]; then
        echo "No branch found in URL: $gh_url/pull/${pr_id}" >&2
        return 1
    fi

    command git fetch ${rem_name} "pull/${pr_id}/head:${branch}"
}

git-https-to-ssh() {
    local rem_name="${1:-origin}"
    local old_url="$( command git remote get-url $rem_name )"
    local new_url="$( echo $old_url | sed -e 's!^https\?://\([^/]\+\)/!git@\1:!' )"

    command git remote set-url $rem_name $new_url
    echo "URL of $rem_name set: $new_url"
}

zsh-pip-cache-packages() {
    typeset -gx -a ZSH_PIP_INDEXES
    ZSH_PIP_INDEXES=( https://pypi.python.org/simple/ )
    typeset -gx ZSH_PIP_CACHE_FILE="${XDG_CACHE_HOME:-$HOME/.cache}/pip-zsh/index"

    if [[ ! -d ${ZSH_PIP_CACHE_FILE:h} ]]; then
        mkdir -p ${ZSH_PIP_CACHE_FILE:h}
    fi

    if [[ ! -f $ZSH_PIP_CACHE_FILE ]]; then
        echo -n "getting package index..."
        tmp_cache="$( mktemp )"
        for index in $ZSH_PIP_INDEXES ; do
            curl -sL "$index" 2>/dev/null \
                | sed -n '/<a href/ s/.*>\([^<]\{1,\}\).*/\1/p' \
                >> $tmp_cache
        done
        sort $tmp_cache | uniq | tr "\n" " " > $ZSH_PIP_CACHE_FILE
        rm $tmp_cache
        echo -n "done!"
    fi
}
