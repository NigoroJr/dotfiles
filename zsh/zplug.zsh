local os="${(L)$( uname -s )}"
if [[ $os =~ cygwin ]]; then
    os=cygwin
fi

zplug 'zplug/zplug'

zplug 'NigoroJr/mkmv', \
    as:command, \
    use:'mkmv'

zplug 'NigoroJr/scripts', \
    from:bitbucket, \
    as:command, \
    use:'bin/*'

zplug 'NigoroJr/scripts', \
    from:bitbucket, \
    as:command, \
    use:"$os/*"

zplug 'NigoroJr/do-after', \
    as:command, \
    use:do-after
# Completion
zplug 'NigoroJr/do-after', \
    as:plugin

zplug "jhawthorn/fzy", \
    as:command, \
    rename-to:fzy, \
    hook-build:"PREFIX=$HOME/usr make install"

zplug 'NigoroJr/644ae8775023be82544d', \
    from:gist, \
    as:command, \
    rename-to:sort-du, \
    use:sort_du.rb

zplug 'plugins/golang', \
    from:oh-my-zsh, \
    ignore:oh-my-zsh.sh, \
    defer:2

zplug 'plugins/gem', \
    from:oh-my-zsh, \
    ignore:'{oh-my-zsh.sh,plugins/gem/gem.plugin.zsh}', \
    defer:2

zplug 'plugins/pip', \
    from:oh-my-zsh, \
    ignore:'{oh-my-zsh.sh,plugins/pip/pip.plugin.zsh}', \
    defer:2

zplug 'Valodim/zsh-curl-completion', \
    defer:2

zplug 'plugins/gem', \
    from:oh-my-zsh, \
    ignore:'{oh-my-zsh.sh,plugins/gem/gem.plugin.zsh}', \
    defer:2

zplug 'zsh-users/zsh-syntax-highlighting', \
    defer:3

zplug 'knu/z', \
    use:'z.sh', \
    defer:1
zplug 'zsh-users/zaw', defer:2
zplug 'NigoroJr/zaw-z', defer:3
