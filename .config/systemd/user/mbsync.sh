#!/bin/env bash

set -eux

# Fetch new messages
/usr/bin/mbsync --config /home/lassemaatta/.config/mbsyncrc -Va

# Only one mu process can access the mailbox at once
pkill -2 -u $(id -u lassemaatta) mu || true

# Update the index
/usr/bin/mu index
