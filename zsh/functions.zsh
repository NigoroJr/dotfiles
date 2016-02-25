psg() {
    ps aux | grep "\(^USER\|$1\)" | grep -v grep
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
if [[ $SHELL =~ "zsh" ]]; then
    compdef _mkdir mcd
fi

sudo() {
    if [[ $1 = "--" ]]; then
        shift
        command sudo "$@"
    elif [[ $1 = "vi" ]] || [[ $1 = "vim" ]] && hash sudoedit 2>/dev/null; then
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

    if [ -z $username ]; then
        echo 'Need username for upstream' 1>&2
        return 1
    fi

    if type hub &>/dev/null; then
        cmd=hub
        upstream=$username
    elif type git &>/dev/null; then
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
