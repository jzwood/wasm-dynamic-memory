#!/bin/bash
# Open the named pipe for reading binary data
exec 3< mypipe
# Read the binary data
dd bs=64 count=1 <&3 > received_data.bin
# Close the named pipe
exec 3<&-
# Optionally, process the received data (e.g., display as hex)
hexdump -C received_data.bin
