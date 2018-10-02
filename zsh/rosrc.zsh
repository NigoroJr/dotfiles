# Utilities for ROS

if (( ! $+commands[rosversion] )) && [[ ! -d /opt/ros ]]; then
    return
fi

source $HOME/.rosfunctions.zsh
source $HOME/.rosaliases.zsh

add-zsh-hook chpwd __source_ros

rossetup
