#!/bin/bash
# execjob.sh
# Invoke from R with exec_background('AutoSampleNextGeh.sh', '/path/to/X.353219')
# See also http://veithen.github.io/2014/11/16/sigterm-propagation.html

# JOBDIR is created by caller
JOBDIR=$1
CHILDPID=
PIDFILE="$JOBDIR/pid"
STARTFILE="$JOBDIR/start"
STATFILE="$JOBDIR/stat"
LOGFILE="$JOBDIR/log"
ERRFILE="$JOBDIR/err"
EXITFILE="$JOBDIR/exit"
ENDFILE="$JOBDIR/end"

# Perform program exit housekeeping
function on_exit() {
    rv=$?
    # Caller has redirected stdout to $LOGFILE
    echo "done"
    echo "done" > $STATFILE
    echo "$rv" > $EXITFILE
    echo `date +%s` > $ENDFILE
    # Add script-specific clean up here also
    exit $rv
}

function on_kill() {
    [ -z $CHILDPID ] || kill -TERM $CHILDPID
    exit 129
}

# Start of process
echo $$ > $PIDFILE
echo `date +%s` > $STARTFILE
echo "running" > $STATFILE

# Caller has redirected stdout to $LOGFILE
echo "starting"
echo $@

# Clean up temporary files on any program exit
trap on_exit EXIT CHLD
trap on_kill HUP INT TERM

# Remove JOBDIR and CMD args
shift
CMD=$1
shift

# Spawn subshell in the background to run the actual script with arguments
$CMD "$@" &
CHILDPID=$!
wait $CHILDPID
wait $CHILDPID
