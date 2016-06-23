#! /usr/bin/env nix-shell
#! nix-shell -i bash -p bash bc pv
set -e
# Analyse results of proc_trace.sh

function msg {
    echo -e "$*" 1>&2
}

msg "Reading input..."
INPUT=$(cat | grep -e "^\(\[\|[0-9]\)")
msg "Finished reading input"

# PID to use for the top-level process
TOP=0
function get_top_pid {
    # Guess the top-level PID as the one which has no clone line
    while read -r PIDGTP
    do
        if echo "$INPUT" | grep "clone(.*) = $PIDGTP" > /dev/null
        then
            true # no-op
        else
            TOP="$PIDGTP"
            return
        fi
    done < <(echo "$INPUT" | grep -o "^\[pid *[0-9]*\]" | grep -o "[0-9]*")
}

msg "Finding top-level process ID"
get_top_pid
msg "Found top-level PID: $TOP"

function strip_prefix {
    echo "$1" | sed -e 's/\[pid *[0-9]*\] //g'
}

function pid_of {
    # Try to infer which process a line comes from. This is either a [pid 1234]
    # prefix, or no prefix if the line comes from the top process
    if PRE=$(echo "$1" | grep -o "^\[pid *[0-9]*\]")
    then
        echo "$PRE" | grep -o "[0-9]*"
    else
        echo "$TOP"
    fi
}

function lines_of {
    # Returns lines associated with the given PID
    if [[ "$1" -eq "$TOP" ]]
    then
        echo "$INPUT" | grep -v "^\[pid *[0-9]*\]"
    else
        echo "$INPUT" | grep "^\[pid *$1\]"
    fi
}

function time_of {
    # Get the timestamp from a line
    echo "$1" | sed -e 's/\[pid *[0-9]*\] //g' | cut -d ' ' -f 1
}

function find_creation {
    # Find the line (if any) where the given PID was spawned
    echo "$INPUT" | grep "clone(.*) = $1" | head -n1
}

function creation_time {
    if [[ "$1" -eq "$TOP" ]]
    then
        time_of "$(echo "$INPUT" | head -n1)"
    else
        time_of "$(find_creation "$1")"
    fi
}

function find_destruction {
    # Find the line (if any) where the given PID exited
    if [[ "$1" -eq "$TOP" ]]
    then
        # The top process exits on the last line
        echo "$INPUT" | tail -n1
    else
        lines_of "$1" | grep "+++ exited with"
    fi
}

function destruction_time {
    time_of "$(find_destruction "$1")"
}

function contains_element {
    # Taken from http://stackoverflow.com/a/8574392/884682
    local e
    for e in "${@:2}"; do [[ "$e" == "$1" ]] && return 0; done
    return 1
}

function gte {
    # $1 >= $2
    (( $(echo "$1 >= $2" | bc -l) ))
}

function all_creations {
    echo "$INPUT" | grep "clone("
}

function all_destructions {
    echo "$INPUT" | grep "+++ exited with"
}

function all_lines {
    # We only care about creations and destructions
    all_creations
    all_destructions
}

function all_times {
    # All times, in order
    all_lines | sed -e 's/\[pid *[0-9]*\] //g' | cut -d ' ' -f 1 | sort -gu
}
msg "Getting all times"
ALL_TIMES=$(all_times)
msg "Got all times"

function pid_col {
    C=$(creation_time    "$1")
    D=$(destruction_time "$1")

    if [[ -z "$C" ]]
    then
        C=$(echo "$ALL_TIMES" | head -n1)
    fi

    if [[ -z "$D" ]]
    then
        D=$(echo "$ALL_TIMES" | tail -n1)
    fi

    STARTED=0
    STOPPED=0
    while read -r TIM
    do
        if [[ "$STOPPED" -eq 1 ]]
        then
            echo 0
            continue
        fi

        if  [[ "$STARTED" -eq 0 ]]
        then
            if gte "$C" "$TIM"
            then
                echo "0"
            else
                STARTED=1
                echo "1"
            fi
        else
            if gte "$TIM" "$D"
            then
                STOPPED=1
                echo "0"
            else
                echo "1"
            fi
        fi
    done < <(echo "$ALL_TIMES")
}

function pid_cols {
    GOT_COLS="$ALL_TIMES"
    THIS_COUNT=1
    for PIDC in "${PIDS[@]}"
    do
        GOT_COLS=$(paste -d "," <(echo "$GOT_COLS") <(pid_col "$PIDC"))
        THIS_COUNT=$(( THIS_COUNT + 1 ))
        msg "$THIS_COUNT/$PID_COUNT"
    done 2> >(pv -etpl -s "$TIMES" > /dev/null)
    echo "$GOT_COLS"
}

function get_all_pids {
    # Fill PIDS with all PIDs that were logged
    echo "$TOP"
    echo "$INPUT" | grep -o '\[pid *[0-9]*\]' | grep -o '[0-9]*' | sort -u
}

msg "Looking up all process IDs"
readarray -t PIDS < <(get_all_pids)
msg "Got all PIDs"

function name_of {
    FOUND=$(lines_of "$1" |
                   grep "execve(" |
                   grep -o 'execve("[^"]*"' |
                   sed -e 's/^execve(//g' | sed -e 's/,//g' |
                   head -n1)
    if [[ -n "$FOUND" ]]
    then
        echo "$FOUND"
    else
        echo "Unknown"
    fi
}

function make_heading {
    printf "Time,"
    for PIDMH in "${PIDS[@]}"
    do
        COL=$(name_of "$PIDMH")
        printf "%s," "$COL"
    done
    echo ""
}

function rows_with_heading {
    printf "Heading..." 1>&2
    make_heading | tee >(tr , '\n' | pv -etls "$PID_COUNT" > /dev/null)
    msg "Heading finished. Rows..."
    pid_cols
    msg "Rows finished"
}

function make_csv {
    rows_with_heading | sed -e 's/,$//g'
}

PID_COUNT="${#PIDS[@]}"
TIMES=$(echo "$ALL_TIMES" | wc -l)

msg "Making CSV"
make_csv
msg "Finished"
