#! /usr/bin/env bash

# Run GHCi with HUMMING_CONNECTION_STRING setup.

# Start the command in the background and capture its PID.
# setsid is used to run humming in a new session. This is to
# avoid that a Ctrl-C within GHCi (for instance when running
# :main serve) also kills our `humming run`.
rm -f /tmp/connection_string.txt
setsid humming run > /tmp/connection_string.txt 2>&1 &
humming_pid=$!

# Function to clean up and exit
cleanup() {
  echo "Cleaning up..."
  kill $humming_pid 2>/dev/null
  exit
}
trap cleanup EXIT INT TERM

# Wait for the connection string to be written to the file
while [ ! -s /tmp/connection_string.txt ]; do
  echo "Waiting for a connection string..."
  sleep 1
done

# Read the connection string
connection_string=$(cat /tmp/connection_string.txt)

# Clean up
rm -f /tmp/connection_string.txt

# Use the connection string in another command
echo "Using connection string: $connection_string"
export HUMMING_CONNECTION_STRING="$connection_string"

scripts/ghci.sh
