# Utilities for ROS

# Which ROS version to use (1, 2, or empty to disable)
export USE_ROS_1_OR_2=2

if (( ! $+commands[rosversion] )) && [[ ! -d /opt/ros ]]; then
    # ROS not installed
    return
elif [[ -z $USE_ROS_1_OR_2 ]]; then
    # ROS explicitly disabled
    return
fi

# Names of ROS versions, in order from oldest to newest
local -a _ros1_names=( groovy hydro indigo jade kinetic lunar melodic noetic )
local -a _ros2_names=( ardent bouncy crystal dashing eloquent foxy galactic )

# Find out the version name
local _ros_version_name
if (( $USE_ROS_1_OR_2 == 1 )); then
    for _version_name in ${(aO)_ros1_names[@]}; do
        if [[ -f /opt/ros/$_version_name/setup.zsh ]]; then
            _ros_version_name="$_version_name"
            break
        fi
    done

    source $HOME/.ros1functions.zsh
    source $HOME/.ros1aliases.zsh
    add-zsh-hook chpwd __source_ros

    rossetup $_ros_version_name
elif (( $USE_ROS_1_OR_2 == 2 )); then
    for _version_name in ${(aO)_ros2_names[@]}; do
        if [[ -f /opt/ros/$_version_name/setup.zsh ]]; then
            _ros_version_name="$_version_name"
            break
        fi
    done

    source /opt/ros/$_ros_version_name/setup.zsh

    source $HOME/.ros2functions.zsh
    source $HOME/.ros2aliases.zsh

    add-zsh-hook chpwd __source_ros
fi
