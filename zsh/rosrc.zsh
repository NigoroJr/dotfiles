# Utilities for ROS

if (( ! $+commands[rosversion] )) && [[ ! -d /opt/ros ]]; then
    return
fi

# ROS2 is installed
if (( $+commands[ros2] )); then
    source $HOME/.ros2functions.zsh
    source $HOME/.ros2aliases.zsh
else
    source $HOME/.ros1functions.zsh
    source $HOME/.ros1aliases.zsh
    add-zsh-hook chpwd __source_ros

    rossetup
fi
