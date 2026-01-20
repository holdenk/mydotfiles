#!/bin/bash
# Checkout all of my repos
set -x
mkdir -p ~/repos
pushd ~/repos
CNTX=users; NAME=holdenk;
for PAGE in {1..5}; do
  REPOS=$(curl "https://api.github.com/$CNTX/$NAME/repos?page=$PAGE&per_page=100" |
            grep -e 'git_url*' |
 	    cut -d \" -f 4)
  for REPO in ${REPOS[@]}; do 
    git clone $REPO || echo "Already cloned $REPO"
  done
done
popd
set -ex
# More bluetooth stuff
sudo apt-get install bluez*
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

# Tailscale repository setup with version detection
UBUNTU_CODENAME=$(. /etc/os-release && echo "${UBUNTU_CODENAME:-$VERSION_CODENAME}")
# Tailscale uses the generic codename for newer versions not yet explicitly supported
# Try the current codename first, fall back to 'noble' for Ubuntu 25.10+ if not available
if curl -f -s -I --connect-timeout 10 --max-time 30 "https://pkgs.tailscale.com/stable/ubuntu/${UBUNTU_CODENAME}/Release" > /dev/null 2>&1; then
  TAILSCALE_CODENAME=$UBUNTU_CODENAME
else
  # Fall back to noble (24.04) for newer versions
  TAILSCALE_CODENAME="noble"
fi

curl -fsSL https://pkgs.tailscale.com/stable/ubuntu/${TAILSCALE_CODENAME}.noarmor.gpg | sudo tee /usr/share/keyrings/tailscale-archive-keyring.gpg >/dev/null || { echo 'Warning: Failed to download Tailscale keyring'; }
curl -fsSL https://pkgs.tailscale.com/stable/ubuntu/${TAILSCALE_CODENAME}.tailscale-keyring.list | sudo tee /etc/apt/sources.list.d/tailscale.list || { echo 'Warning: Failed to download Tailscale list'; }
echo "deb https://dl.bintray.com/sbt/debian /" | sudo tee -a /etc/apt/sources.list.d/sbt.list
sudo apt-get update
# Skip tailscale because of potential conflicts
#sudo apt-get install tailscale sbt
#if [ -z "$SERVER" ]; then
#   sudo tailscale up
#fi
wget https://dl.google.com/go/go1.13.1.linux-amd64.tar.gz
sudo tar -C /usr/local -xvf go1.13.1.linux-amd64.tar.gz
sudo pip install Pygments sphinx pypandoc mkdocs &

# Try to find available gem version dynamically
GEM_CMD=""
for gem_path in /usr/bin/gem*; do
  if [ -x "$gem_path" ]; then
    GEM_CMD="$gem_path"
    break
  fi
done

if [ -n "$GEM_CMD" ]; then
  sudo $GEM_CMD install jekyll pygments.rb &
else
  echo "Warning: No gem command found, skipping jekyll/pygments.rb installation"
fi

sudo Rscript -e 'install.packages(c("knitr", "devtools", "roxygen2", "testthat", "rmarkdown"), repos="http://cran.stat.ucla.edu/")' &
gpg2 --recv-keys 409B6B1796C275462A1703113804BB82D39DC0E3
curl -L https://get.rvm.io | bash -s stable
source ~/.rvm/scripts/rvm
rvm install 2.4.2 &&
wait
rvm use ruby-2.4.2
gem install jekyll pygments.rb &
#echo "Installing ffmpeg"
#pushd /tmp
#git clone --depth 1 git://source.ffmpeg.org/ffmpeg.git
#pushd ffmpeg
#./configure --enable-shared --prefix=/usr
#make -j4
#sudo checkinstall --pkgname=FFmpeg --fstrans=no --backup=no \
#     --pkgversion="$(date +%Y%m%d)-git" --deldoc=yes
#popd
#popd
mkdir ~/bin
if [ -z "$SERVER" ]; then
   pushd /tmp
   wget https://github.com/ksonnet/ksonnet/releases/download/v0.9.1/ks_0.9.1_linux_amd64.tar.gz
   tar -xvf ks_0.9.1_linux_amd64.tar.gz
   mv ks_0.9.1_linux_amd64/ks ~/bin/
   popd
   # Bluez
   mkdir -p /tmp/bluez
   pushd /tmp/bluez
   wget http://www.kernel.org/pub/linux/bluetooth/bluez-5.54.tar.xz
   tar -xvf bluez-5.54.tar.xz
   cd bluez-5.54
   ./configure
   make
   sudo make install
   mkdir -p /tmp/libbtbb
   pushd /tmp/libbtbb
   wget https://github.com/greatscottgadgets/libbtbb/archive/2018-12-R1.tar.gz -O libbtbb-2018-12-R1.tar.gz
   tar xf libbtbb-2018-12-R1.tar.gz
   cd libbtbb-2018-12-R1
   export ENABLE_PYTHON=true
   mkdir build
   cd build
   cmake ..
   make
   sudo make install
   sudo ldconfig
fi
# ssh keys
GH_USER=${GH_USER:-holdenk}
curl https://github.com/${GH_USER}.keys | ${RUN_DEST_CMD} tee -a ~/authorized_keys
# TODO: Disable password login
wget https://download.brother.com/welcome/dlf006893/linux-brprinter-installer-2.2.3-1.gz
gunzip linux-brprinter-installer-2.2.3-1.gz
chmod a+x linux-brprinter-installer-2.2.3-1
./linux-brprinter-installer-2.2.3-1.gz
