# Utilities for ROS

if (( ! $+commands[rosversion] )) && [[ ! -d /opt/ros ]]; then
    return
fi

if (( $+commands[catkin] )); then
    local -a opts
    opts=( --link-devel \
        --no-install \
        --env-cache \
        --cmake-args -DCMAKE_BUILD_TYPE=Release -- \
        )
    catkin config ${opts[@]} >/dev/null &
fi

source $HOME/.rosfunctions.zsh
source $HOME/.rosaliases.zsh

add-zsh-hook chpwd __source_ros

rossetup
