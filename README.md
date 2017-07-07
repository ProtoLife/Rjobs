# Rjobs

This is a proposal to create a simple "job" management system for
server.R.

Code to manage named jobs is in the file "jobmgr.R", which is meant to
be sourced by server.R.

The function `backgroundJob` creates a subdirectory to hold files that
can be used to monitor the job's progress and result.  STDOUT and STDERR
for the job are redirected to `log` and `err` files in the subdirectory.

Then `backgroundJob` launches a shell script named "execjob.sh".
This script executes the real background child process, such as
"AutoSampleNextGen.sh", and captures interrupts that the R polling code
can use to cancel the job.


## Issues / Discussion

### Killing of grandchild processes

Right now, if the job is canceled, the function `killPid` in jobmgr.R
collects all the descendant processes of the "execjob.sh" process,
using the script "descendantpids.sh". Then these descendant processes
are killed in reverse order, followed by the killing of the main
subprocess itself.  Perhaps killing of grandchildren
processes should be the responsibility of scripts such as
"AutoSampleNextGen.sh", which should take care to clean up temporary files,
clusters, etc. if they themselves are killed.


### Error reporting

The production "AutoSampleNextGen.sh" (and other scripts) append errors
to a single log file named "ShExecLog.tmp", which collects errors from
all scripts that are run, and which is checked by server.R's `checkError`
function.  I propose that each process, such as "AutoSampleNextGen.sh",
that is launched using this management system, instead just appends
errors to its STDERR using redirection commands `1>&2` or just `>&2`.  
So (from "AutoSampleNextGen.sh"):

```
    echo "Unknown argument '$1'"
    echo "$scriptName: ERROR analyzing response measurements."
    echo "$scriptName: error" >> $bashScriptExecutionLogFile
    echo "$scriptName: END"; exit 1
```

would become:

```
    echo "Unknown argument '$1'"
    echo "$scriptName: ERROR analyzing response measurements."
    echo "$scriptName: error" >&2
    echo "$scriptName: END"; exit 1
```
