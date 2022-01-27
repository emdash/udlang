#! /bin/bash

# Quick and dirty shell wrapper for gmachine.py and heapviz.py

# run the gmachine and post-process its output
function rebuild {
    python3 gmachine.py "$@" | python3 heapviz.py > debug_log.js
}

# open a new browser tab (firefox only) pointing at the debugger page
function launch {
    firefox ./debugger.html
    sleep 1
    refresh
}

# rebuild the debug output, and refresh the browser tab.
# firefox-only.
# requires xdotool installed
function refresh {
    echo rebuild "$@"
    rebuild "$@"

    # save terminal window id
    CURRENT_WID=$(xdotool getwindowfocus)

    # activate the last firefox window we find, and tell it to refresh
    WID=$(xdotool search --name "Firefox" | tail -n 1)
    xdotool windowactivate $WID
    xdotool key F5

    # return to the terminal
    xdotool windowactivate $CURRENT_WID
}

"$@"
