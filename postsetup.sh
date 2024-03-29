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
   sudo apt-key add -

   # 2. Add our repository to your list of repositories
   echo "deb [arch=amd64] https://updates.signal.org/desktop/apt xenial main" |\
   sudo tee -a /etc/apt/sources.list.d/signal-xenial.list

   # 3. Update your package database and install signal
   sudo apt update && sudo apt install signal-desktop
fi

curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | sudo apt-key add
curl https://pkgs.tailscale.com/stable/ubuntu/focal.gpg | sudo apt-key add -
curl https://pkgs.tailscale.com/stable/ubuntu/focal.list | sudo tee /etc/apt/sources.list.d/tailscale.list
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
sudo gem2.3 install jekyll pygments.rb &
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
