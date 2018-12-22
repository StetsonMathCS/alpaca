#!/usr/bin/env bash

export DEBIAN_FRONTEND=noninteractive

set -x
set -e

# Mount the Guest Additions ISO and install them for this Virtualbox version.
vbox_guest_additions_iso="${HOME}/VBoxGuestAdditions_$(cat ~/.vbox_version).iso"
if [ ! -e "${vbox_guest_additions_iso}" ]; then
	echo "error - virtualbox guest additions ISO not found in expected path of ${vbox_guest_additions_iso}"
	exit 1
fi
sudo mkdir -p /tmp/vbox_guest_additions
sudo mount -o loop "${vbox_guest_additions_iso}" /tmp/vbox_guest_additions
cd /tmp/vbox_guest_additions
# Don't die on failure here, without X11 libraries installed (creates a mess of our OS),
# the stuff we need installs and X11 will fail. 
set +e
sudo ./VBoxLinuxAdditions.run --nox11
set -e
sleep 1
cd /
sudo umount /tmp/vbox_guest_additions
sudo rm -rf /tmp/vbox_guest_additions
