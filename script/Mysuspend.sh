#!/bin/bash

if pgrep light-locker &> /dev/null; then
    pkill light-locker &
else
    echo "Systemd Suspend"
    light-locker --no-late-locking --lock-on-suspend
    systemctl suspend
fi

exit 0
