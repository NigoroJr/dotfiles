# Utility aliases for ROS

alias rgp="rosrun rqt_graph rqt_graph"
alias rtt="rosrun rqt_tf_tree rqt_tf_tree"
if (( $+functions[ros_source_setup_script] )); then
    alias rsu="ros_source_setup_script devel/setup.zsh"
else
    alias rsu="source devel/setup.zsh"
fi
alias rr="rosrun"
alias rb="rosbag"
alias rl="roslaunch"
alias rte="rostopic echo"
alias rtl="rostopic list"
alias rti="rostopic info"
alias rth="rostopic hz"
alias rnl="rosnode list"
alias rni="rosnode info"
alias rpf="rospack find"

if (( $+commands[catkin] )); then
    alias c="catkin"
fi
