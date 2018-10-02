# Utility functions for ROS

rosclear() {
    # Perform a "level 1" reset (see comment in rossetup)
    unset ROS_DISTRO
    unset ROS_ROOT
    unset ROS_MASTER_URI
    unset ROS_PACKAGE_PATH
    unset ROS_ETC_DIR
    unset ROSLISP_PACKAGE_DIRECTORIES
    unset ROSCONSOLE_FORMAT
    unset ROS_LIB_DIR
    unset CATKIN_SETUP_UTIL_ARGS

    # Reset system environment variables
    export CMAKE_PREFIX_PATH="$NON_ROS_ENV[CMAKE_PREFIX_PATH]"
    export LD_LIBRARY_PATH="$NON_ROS_ENV[LD_LIBRARY_PATH]"
    export PKG_CONFIG_PATH="$NON_ROS_ENV[PKG_CONFIG_PATH]"
    export PYTHONPATH="$NON_ROS_ENV[PYTHONPATH]"
}

rosreset() {
    # Perform a "level 2" reset (see comment in rossetup)
    export ROS_DISTRO="$PURE_ROS_ENV[ROS_DISTRO]"
    export ROS_PACKAGE_PATH="$PURE_ROS_ENV[ROS_PACKAGE_PATH]"
    export ROSLISP_PACKAGE_DIRECTORIES="$PURE_ROS_ENV[ROSLISP_PACKAGE_DIRECTORIES]"
    export CMAKE_PREFIX_PATH="$PURE_ROS_ENV[CMAKE_PREFIX_PATH]"
    export LD_LIBRARY_PATH="$PURE_ROS_ENV[LD_LIBRARY_PATH]"
    export PKG_CONFIG_PATH="$PURE_ROS_ENV[PKG_CONFIG_PATH]"
    export PYTHONPATH="$PURE_ROS_ENV[PYTHONPATH]"
}

rossetup() {
    # Setting up a ROS environment involves resetting the environment
    # variables. A "level 1" reset (executed by default) resets the
    # environment variables to the state before ROS was sourced the first
    # time. A "level 2" reset (specified by a -r) is less aggressive than
    # level 1, in which the environment variables are set to the state before
    # entering the first ROS workspace (after the common workspaces are
    # sourced). To preserve the current environment variables, use the -p
    # option.
    local common_ws
    local preserve_env=false
    local arg

    # Environment variables before ROS is sourced
    typeset -gxA NON_ROS_ENV
    # Environment variables after common workspace is sourced
    typeset -gxA PURE_ROS_ENV

    while getopts 'hrp' flag; do
        case "$flag" in
            p)
                preserve_env=true
                ;;
            r)
                rosreset
                ;;
        esac

    done
    shift $(( $OPTIND - 1 ))

    arg="$1"

    if ! $preserve_env; then
        rosclear
    fi

    if [[ -z $NON_ROS_ENV ]]; then
        NON_ROS_ENV[CMAKE_PREFIX_PATH]="$CMAKE_PREFIX_PATH"
        NON_ROS_ENV[LD_LIBRARY_PATH]="$LD_LIBRARY_PATH"
        NON_ROS_ENV[PKG_CONFIG_PATH]="$PKG_CONFIG_PATH"
        NON_ROS_ENV[PYTHONPATH]="$PYTHONPATH"
    fi

    # Defaults to latest, but may change depending on the workspace path name
    export ROS_DISTRO=${arg:-"$( __latest_ros_distro )"}
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

    PURE_ROS_ENV[ROS_DISTRO]="$ROS_DISTRO"
    PURE_ROS_ENV[ROS_PACKAGE_PATH]="$ROS_PACKAGE_PATH"
    PURE_ROS_ENV[ROSLISP_PACKAGE_DIRECTORIES]="$ROSLISP_PACKAGE_DIRECTORIES"
    PURE_ROS_ENV[CMAKE_PREFIX_PATH]="$CMAKE_PREFIX_PATH"
    PURE_ROS_ENV[LD_LIBRARY_PATH]="$LD_LIBRARY_PATH"
    PURE_ROS_ENV[PKG_CONFIG_PATH]="$PKG_CONFIG_PATH"
    PURE_ROS_ENV[PYTHONPATH]="$PYTHONPATH"
}

__is_ros_ws() {
    local cwd="${1:-$PWD}"
    if [[ -L $cwd/src/CMakeLists.txt ]]; then
        local tgt_basename="$( print $cwd/src/CMakeLists.txt(:A:t) )"
        if [[ $tgt_basename == toplevel.cmake ]]; then
            return true
        fi
    fi

    [[ -d $cwd/src ]] && [[ -d $cwd/build ]] && [[ -d $cwd/devel ]] \
        && [[ -e $cwd/devel/setup.zsh ]] && [[ -e $cwd/devel/.catkin ]]
}

__latest_ros_distro() {
    print -l /opt/ros/*(On:t) | head -n 1
}

ck() {
    local -a default_args
    default_args=( \
        '-l' '4' \
        '--mem-limit' '90%' \
        '--cmake-args' '-DCMAKE_BUILD_TYPE=Release' '--' \
        '--no-notify' )
    local -a args
    args=( $@ )

    if (( ! $+commands[catkin] )); then
        ck-legacy ${args[@]}
        return
    fi

    if ! __is_ros_ws; then
        default_args=( '--this' ${default_args[@]} )
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

make-ws() {
    mkdir -p bin config data launch maps media/images media/videos rviz ws/src
    touch README.md
}

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
    local -aU rpp_filtered
    local -a cpp
    local -a pp
    rpp=( $( echo ${(s#;#)${(s#:#)ROS_PACKAGE_PATH}} | sed -e "s#$HOME#~#g" ) )
    cpp=( $( echo ${(s#;#)${(s#:#)CMAKE_PREFIX_PATH}} | sed -e "s#$HOME#~#g" ) )
    pp=( $( echo ${(s#;#)${(s#:#)PYTHONPATH}} | sed -e "s#$HOME#~#g" ) )

    rpp_filtered+=( $( sed -e 's#\(^~/ros/workspaces/[^/]\+/[^/]\+\)/src/.*$#\1#' =( print -l $rpp ) ) )

    echo "ROS_DISTRO: $ROS_DISTRO"
    echo "ROS_PACKAGE_PATH"
    for p in ${rpp_filtered[@]}; do
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

    echo -n "$ROS_DISTRO"
}

__source_ros() {
    # If no ROS, don't bother going further
    if (( ! $+commands[rosversion] )); then
        return
    fi

    # Check that the directory that we moved in is indeed a catkin workspace
    if __is_ros_ws; then
        local prev_distro="$ROS_DISTRO"
        # Figure out distro if possible, otherwise keep the current distro
        export ROS_DISTRO="$( __distro_from_ws_name $PWD )"
        if [[ -n $ROS_DISTRO ]] && [[ $ROS_DISTRO != $prev_distro ]]; then
            rossetup $ROS_DISTRO
            echo "Switched distro to $ROS_DISTRO"
        else
            # Reset to "level 2"
            rossetup -r $ROS_DISTRO
        fi

        ros_source_setup_script devel/setup.zsh

        # Special handling in case non-APT-provided ros_comm is installed
        if [[ -d $( rospack find rospy 2>/dev/null )/src ]]; then
            PYTHONPATH="$( rospack find rospy 2>/dev/null )/src:$PYTHONPATH"
        fi
    fi
}
