# Utilities for ROS

if (( ! $+commands[rosversion] )) && [[ ! -d /opt/ros ]]; then
    return
fi

# ROS1 is installed
if (( $+commands[roscore] )); then
    source $HOME/.ros1functions.zsh
    source $HOME/.ros1aliases.zsh
    add-zsh-hook chpwd __source_ros

    rossetup
elif (( $+commands[ros2] )); then
    source $HOME/.ros2functions.zsh
    source $HOME/.ros2aliases.zsh
fi
