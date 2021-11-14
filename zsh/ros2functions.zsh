
ck() {
    local -a default_args
    default_args=( \
        "--cmake-args" "-DCMAKE_BUILD_TYPE=Release" \
        )
    local -a args
    args=( $@ )

    local ws_path

    if ! __is_ros_ws; then
        ws_path="$PWD"
        while [[ $ws_path != "/" ]]; do
            if __is_ros_ws $ws_path; then
                default_args=( "--base-paths" "$ws_path" ${default_args[@]} )
                break
            fi
            ws_path="$( dirname $ws_path )"
        done
    fi

    if [[ -n $ws_path ]]; then
        ( cd $ws_path && colcon build ${default_args[@]} ${args[@]} )
    else
        colcon build ${default_args[@]} ${args[@]}
    fi
}

__is_ros_ws() {
    local cwd="${1:-$PWD}"

    if [[ -d $cwd/src ]] && [[ -d $cwd/build ]] && [[ -e $cwd/build/.built_by ]]; then
        return 0
    else
        return 1
    fi
}

__source_ros() {
    # If no ROS, don't bother going further
    if (( ! $+commands[rosversion] )); then
        return
    fi

    # Check that the directory that we moved in is indeed a catkin workspace
    if __is_ros_ws; then
        if [[ -e install/local_setup.zsh ]]; then
            source install/local_setup.zsh
        fi
    fi
}
