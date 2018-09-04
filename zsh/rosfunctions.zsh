# Utility functions for ROS

rosclear() {
    unset ROS_ETC_DIR
    unset ROS_DISTRO
    unset ROSLISP_PACKAGE_DIRECTORIES
    unset ROSCONSOLE_FORMAT
    unset ROS_LIB_DIR
    unset ROS_MASTER_URI
    unset ROS_PACKAGE_PATH
    unset ROS_ROOT
    unset CATKIN_SETUP_UTIL_ARGS
    # Note: May unset non-ROS cmake prefix
    unset CMAKE_PREFIX_PATH
}

rossetup() {
    local common_ws

    # Note: May not be good if non-ROS paths are set
    unset PYTHONPATH
    rosclear

    # Defaults to latest, but may change depending on the workspace path name
    export ROS_DISTRO=${1:-"$( __latest_ros_distro )"}
    export ROSCONSOLE_FORMAT='[${severity}] [${node}] [${time}]: ${message}'
    # export ROSCONSOLE_FORMAT='[${severity}] [${node} : ${function} : ${line}] [${time}]: ${message}'
    export RESIBOTS_DIR=$HOME/usr
    export GAZEBO_PLUGIN_PATH=$HOME/.gazebo/plugins:$GAZEBO_PLUGIN_PATH
    export CATKIN_SETUP_UTIL_ARGS='--extend'


    if [[ -f /opt/ros/$ROS_DISTRO/setup.zsh ]]; then
        source /opt/ros/$ROS_DISTRO/setup.zsh
    fi
    common_ws="$HOME/ros/workspaces/$ROS_DISTRO/common"
    if [[ -e $common_ws/devel/setup.zsh ]]; then
        ros_source_setup_script $common_ws/devel/setup.zsh
    fi
}

__is_ros_ws() {
    local cwd="${1:-$PWD}"
    [[ -d $cwd/src ]] && [[ -d $cwd/build ]] && [[ -d $cwd/devel ]] \
        && [[ -e $cwd/devel/setup.zsh ]] && [[ -e $cwd/devel/.catkin ]]
}

__latest_ros_distro() {
    print -l /opt/ros/*(On:t) | head -n 1
}

ck() {
    local -a default_args
    default_args=( '--cmake-args' '-DCMAKE_BUILD_TYPE=Release' )
    local -a args
    args=( $@ )

    if (( ! $+commands[catkin] )); then
        ck-legacy ${args[@]}
        return
    fi

    # Assume we're in a package
    if ! __is_ros_ws "$PWD"; then
        default_args+='--this'
    fi

    catkin build ${default_args[@]} ${args[@]}
}

# catkin_make version of ck
ck-legacy() {
    local -a default_args
    default_args=( '-DCMAKE_BUILD_TYPE=Release' )
    local -a args
    args=( $@ )

    local cwd="$PWD"
    while [[ $cwd != '/' ]]; do
        if __is_ros_ws $cwd; then
            (
                builtin cd -q $cwd
                if (( $#args == 0 )); then
                    catkin_make ${default_args[@]}
                else
                    catkin_make ${default_args[@]} ${args[@]}
                fi
            )
            return
        fi

        cwd="${cwd:h}"
    done

    catkin_make ${default_args[@]} ${args[@]}
}

rws() {
    local ws_prefix="${ROS_HOME:-$HOME/ros/workspaces}"
    local ws_name="$1"

    if ! [[ -d $ws_prefix/$ws_name ]]; then
        echo "Workspace $ws_name not found" >&2
        return
    fi

    if ! [[ -d $ws_prefix/$ws_name/devel/setup.zsh ]]; then
        echo "setup.zsh not found in $ws_prefix/$ws_name/devel" >&2
        return
    fi

    ros_source_setup_script $ws_prefix/$ws_name/devel/setup.zsh
}
# TODO: completion

__clear_cmake_prefix_path() {
    # Removes the pre-filled CMAKE_PREFIX_PATH from _setup_util.py in the
    # given directory. Ref: Section 3.2 of http://wiki.ros.org/catkin/what
    local dirname="$1"
    local fn="$dirname/_setup_util.py"
    local system_ros="/opt/ros/$ROS_DISTRO"

    if ! [[ -d $system_ros ]]; then
        system_ros=''
    fi

    if ! [[ -f $fn ]]; then
        return
    fi

    sed -i -e "s!CMAKE_PREFIX_PATH = '[^']*'!CMAKE_PREFIX_PATH = '$system_ros'!" $fn
}

rpp() {
    local -a rpp
    local -a cpp
    rpp=( $( echo ${(s#;#)${(s#:#)ROS_PACKAGE_PATH}} | sed -e "s#$HOME#~#g" ) )
    cpp=( $( echo ${(s#;#)${(s#:#)CMAKE_PREFIX_PATH}} | sed -e "s#$HOME#~#g" ) )
    pp=( $( echo ${(s#;#)${(s#:#)PYTHONPATH}} | sed -e "s#$HOME#~#g" ) )
    echo "ROS_PACKAGE_PATH"
    for p in ${rpp[@]}; do
        echo "  $p"
    done
    echo "CMAKE_PREFIX_PATH"
    for p in ${cpp[@]}; do
        echo "  $p"
    done
    echo "PYTHONPATH"
    for p in ${pp[@]}; do
        echo "  $p"
    done
}

ros_source_setup_script() {
    local setup_script="$1"

    if ! [[ -f ${setup_script} ]]; then
        return
    fi

    __clear_cmake_prefix_path ${setup_script:h}
    source ${setup_script}

    # Need to source /opt/ros/*/setup.zsh to set ROS_PACKAGE_PATH
    if [[ -f /opt/ros/$ROS_DISTRO/setup.zsh ]]; then
        source /opt/ros/$ROS_DISTRO/setup.zsh
    fi
}

__distro_from_ws_name() {
    # Attempts to deduce the ROS distribution name from the workspace name.
    # Note: We can't use rosversion -d because we may be in a workspace that's
    # built for a different distro than what we are currently using.
    local ws_name="$1"
    local -a distros

    # Level 1: explicitly specified in .rosdistro file
    # Traverse up the directory tree and see if we can find a .rosdistro file
    local p="$ws_name"
    while [[ $p != / ]]; do
        if [[ -f $p/.rosdistro ]]; then
            cat $p/.rosdistro
            return
        fi
        p="${p:h}"
    done

    if [[ -d /opt/ros ]]; then
        distros=( ${$( print -l /opt/ros/* ):t} )
    else
        echo -n ""
        return
    fi

    # Level 2: distro name is the directory name
    for name in ${distros[@]}; do
        if [[ $ws_name == */$name/* ]]; then
            echo -n $name
            return
        fi
    done

    # Level 3: directory name contains distro name
    for name in ${distros[@]}; do
        if [[ $ws_name == /*$name*/ ]]; then
            echo -n $name
            return
        fi
    done

    echo -n ""
}

__source_ros() {
    # If no ROS, don't bother going further
    if (( ! $+commands[rosversion] )); then
        return
    fi
    typeset -gxA OLD_ROS_ENV
    typeset -gx PRESERVE_LAST_WS="${PRESERVE_LAST_WS:-true}"

    if [[ -z $OLD_ROS_ENV[MODIFIED] ]] || ! $OLD_ROS_ENV[MODIFIED]; then
        # Save old state
        OLD_ROS_ENV[ROS_DISTRO]="$ROS_DISTRO"
        OLD_ROS_ENV[CMAKE_PREFIX_PATH]="$CMAKE_PREFIX_PATH"
        OLD_ROS_ENV[LD_LIBRARY_PATH]="$LD_LIBRARY_PATH"
        OLD_ROS_ENV[PKG_CONFIG_PATH]="$PKG_CONFIG_PATH"
        OLD_ROS_ENV[ROSLISP_PACKAGE_DIRECTORIES]="$ROSLISP_PACKAGE_DIRECTORIES"
        OLD_ROS_ENV[ROS_PACKAGE_PATH]="$ROS_PACKAGE_PATH"
        OLD_ROS_ENV[PYTHONPATH]="$PYTHONPATH"
    fi

    # Went out of workspace with PRESERVE_LAST_WS == true or
    # Went into a workspace with PRESERVE_LAST_WS == false
    if ( ! ${PRESERVE_LAST_WS} && $OLD_ROS_ENV[MODIFIED] ) \
        || ( ${PRESERVE_LAST_WS} && __is_ros_ws ); then
        # Revert to old state
        ROS_DISTRO="$OLD_ROS_ENV[ROS_DISTRO]"
        CMAKE_PREFIX_PATH="$OLD_ROS_ENV[CMAKE_PREFIX_PATH]"
        LD_LIBRARY_PATH="$OLD_ROS_ENV[LD_LIBRARY_PATH]"
        PKG_CONFIG_PATH="$OLD_ROS_ENV[PKG_CONFIG_PATH]"
        ROSLISP_PACKAGE_DIRECTORIES="$OLD_ROS_ENV[ROSLISP_PACKAGE_DIRECTORIES]"
        ROS_PACKAGE_PATH="$OLD_ROS_ENV[ROS_PACKAGE_PATH]"
        PYTHONPATH="$OLD_ROS_ENV[PYTHONPATH]"

        OLD_ROS_ENV[MODIFIED]="false"
    fi

    # Check that the directory that we moved in is indeed a catkin workspace
    if __is_ros_ws; then
        if [[ $ROS_AUTO_DISTRO == true ]]; then
            # Figure out distro if possible, otherwise keep the current distro
            local ros_distro="$( __distro_from_ws_name $PWD )"
            if [[ -n $ros_distro ]] && [[ $ros_distro != $ROS_DISTRO ]]; then
                rosclear
                rossetup $ros_distro
                echo "Switched distro to $ros_distro"
            fi
        fi
        OLD_ROS_ENV[MODIFIED]="true"
        ros_source_setup_script devel/setup.zsh
        # Special handling in case non-APT-provided ros_comm is installed
        if [[ -d $( rospack find rospy 2>/dev/null )/src ]]; then
            PYTHONPATH="$( rospack find rospy 2>/dev/null )/src:$PYTHONPATH"
        fi
    fi
}
