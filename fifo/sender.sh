#!/bin/bash
# Open the named pipe for writing binary data
exec 3> mypipe
# Send binary data (example: random binary data)
head -c 64 /dev/urandom >&3
# Close the named pipe
exec 3>&-