#!/bin/bash
# Checkout all of my repos
set -x
mkdir -p ~/repos
pushd ~/repos
#CNTX=users; NAME=holdenk;
#for PAGE in {1..5}; do
#  REPOS=$(curl "https://api.github.com/$CNTX/$NAME/repos?page=$PAGE&per_page=100" |
#            grep -e 'git_url*' |
# 	    cut -d \" -f 4)
#  for REPO in ${REPOS[@]}; do 
#    git clone $REPO || echo "Already cloned $REPO"
#  done
#done
popd
set -ex
# More bluetooth stuff
sudo apt-get install -y bluez*
#tailscale
echo 'net.ipv4.ip_forward = 1' | sudo tee -a /etc/sysctl.conf

if [ -z "$SERVER" ]; then
 # NOTE: These instructions only work for 64 bit Debian-based
 # Linux distributions such as Ubuntu, Mint etc.

 # 1. Install our official public software signing key
 wget -O- https://updates.signal.org/desktop/apt/keys.asc |\
   gpg --dearmor | sudo tee /usr/share/keyrings/signal-desktop-keyring.gpg > /dev/null

   # 2. Add our repository to your list of repositories
   # Signal uses 'xenial' for all Ubuntu versions for compatibility
   echo "deb [arch=amd64 signed-by=/usr/share/keyrings/signal-desktop-keyring.gpg] https://updates.signal.org/desktop/apt xenial main" |\
   sudo tee /etc/apt/sources.list.d/signal-xenial.list

   # 3. Update your package database and install signal
   sudo apt update && sudo apt install signal-desktop
fi

curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | gpg --dearmor | sudo tee /usr/share/keyrings/scalasbt-release.gpg > /dev/null

# Tailscale installation using official install script
curl -fsSL https://tailscale.com/install.sh | sh
# sbt
sudo apt-get install apt-transport-https curl gnupg -yqq
echo "deb https://repo.scala-sbt.org/scalasbt/debian all main" | sudo tee /etc/apt/sources.list.d/sbt.list
echo "deb https://repo.scala-sbt.org/scalasbt/debian /" | sudo tee /etc/apt/sources.list.d/sbt_old.list
curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | sudo -H gpg --no-default-keyring --keyring gnupg-ring:/etc/apt/trusted.gpg.d/scalasbt-release.gpg --import
sudo chmod 644 /etc/apt/trusted.gpg.d/scalasbt-release.gpg
sudo apt-get update
sudo apt-get install -y sbt
# Skip tailscale because of potential conflicts
#sudo apt-get install tailscale sbt
#if [ -z "$SERVER" ]; then
#   sudo tailscale up
#fi
wget https://dl.google.com/go/go1.13.1.linux-amd64.tar.gz
sudo tar -C /usr/local -xvf go1.13.1.linux-amd64.tar.gz

GH_USER=${GH_USER:-holdenk}
curl https://github.com/${GH_USER}.keys | ${RUN_DEST_CMD} tee -a ~/authorized_keys
