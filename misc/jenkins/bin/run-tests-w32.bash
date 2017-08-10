#!/bin/bash

# Locking.
exec 9<"$0"
echo -n "Aquiring lock on $0... "
if ! flock --timeout 15 9 ; then
    echo "failed!"
    exit 1
fi
echo "ok."

set -ex

URI="qemu:///system"
GUEST="win8.1"
GUEST_CDROM="sda"
SSH="gpg@192.168.122.117"
SSH_COMMAND_TIMEOUT="60m"

function vdo() {
    virsh -c "$URI" "$@"
}

function vssh() {
    # OpenSSH on Windows does not cope well with a closed stdin.
    timeout </dev/zero "$SSH_COMMAND_TIMEOUT" ssh "$SSH" "$@"
}

function vping() {
    if ssh </dev/zero >/dev/null 2>&1 -oConnectTimeout=1 \
                                      "$SSH" "echo pong" ; then
	return 0
    else
	return 1
    fi
}

function vwait() {
    echo >&2 -n "Waiting for the machine to boot... "
    while ! vping ; do echo >&2 -n . ; sleep 1 ; done
}

# Revert to current snapshot and start the machine.
vdo snapshot-revert --snapshotname tests --force --running "$GUEST"

# Insert the CD.
vdo change-media --update "$GUEST" "$GUEST_CDROM" "$1"

set +x
vwait
set -x

#sleep 5 # XXX: Let things settle.

if [ "$2" ]; then
    scp "$2" "$SSH:"
    sleep 5 # XXX: openssh on windows is a bit fragile...
    time vssh "cmd /c $(basename $2)"
else
    time vssh "cmd /c d:/run-tests.bat"
fi

sleep 5 # XXX: openssh on windows is a bit fragile...

# The scp server is a bit fragile as well, and I believe globbing does
# not work.  Simply use gpgtar.
vssh 'powershell -Command "cd c:\temp\logs ; d:/gnupg/gpgtar.exe --create ."' | tar x --warning=no-timestamp

# Shutdown.
vdo shutdown "$GUEST"
