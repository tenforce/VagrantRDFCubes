#!/usr/bin/env bash
#################################################################
# Install the necessary components for building and installing
# the unifiedviews system from the github.
#
apt-get update
apt-get upgrade
apt-get install -y xinit
# apt-get install -y apache2
apt-get install -y xinit
apt-get install -y gnome-shell
apt-get install -y xterm
apt-get install -y gnome-terminal
apt-get install -y gdm3
dpkg-reconfigure gdm3

# Now start to setup for building unified views, etc.
apt-get install -y openjdk-7-jre
apt-get install -y openjdk-7-jdk
apt-get install -y tomcat7
apt-get install -y git
apt-get install -y maven
apt-get install -y bash
apt-get install -y debconf-utils
# apt-get install -y emacs

# Tools required for virtuoso building ...

# apt-get install -y autoconf
# apt-get install -y automake
# apt-get install -y libtool
# apt-get install -y flex
# apt-get install -y bison
# apt-get install -y gperf
# apt-get install -y gawk
# apt-get install -y m4
# apt-get install -y make
# apt-get install -y openssl
# apt-get install -y libssl-dev

apt-get install -y dpkg-dev build-essential
apt-get install -y quilt gdebi

###############################################################
# Pull the virtuoso opensource latest package
# if [ -d virtuoso-opensource ]
# then
#  cd virtuoso-opensource ; git pull
# else
#  git clone https://github.com/openlink/virtuoso-opensource.git
# fi;
# cd ${HOME}/virtuoso-opensource
# apt-get build-dep virtuoso-opensource
# ./autogen.sh
# dpkg-buildpackage -rfakeroot
# gdebi *.deb

###############################################################
# Set the default values for the debconf questions
#
echo "mysql-server-5.5 mysql-server/root_password_again password root" | debconf-set-selections
echo "mysql-server-5.5 mysql-server/root_password password root" | debconf-set-selections
echo "mysql-server-5.5 mysql-server-5.5/root_password_again password root" | debconf-set-selections
echo "mysql-server-5.5 mysql-server-5.5/root_password password root" | debconf-set-selections
echo "virtuoso-opensource-6.1 virtuoso-opensource-6.1/dba-password-again password root" | debconf-set-selections
echo "virtuoso-opensource-6.1 virtuoso-opensource-6.1/dba-password password root" | debconf-set-selections
echo "unifiedviews-webapp-mysql       frontend/mysql_db_password password root"| debconf-set-selections
echo "unifiedviews-webapp-shared	frontend/mysql_dba_user	string	uv" | debconf-set-selections
echo "unifiedviews-webapp-shared      frontend/mysql_dba_password password root"| debconf-set-selections
echo "unifiedviews-webapp-mysql       frontend/mysql_db_user string root" | debconf-set-selections
echo "unifiedviews-webapp-mysql       frontend/mysql_db_password password uv"| debconf-set-selections
echo "unifiedviews-webapp-mysql       frontend/mysql_db_user string uv"| debconf-set-selections
echo "unifiedviews-backend-mysql      backend/mysql_root password root"| debconf-set-selections
echo "unifiedviews-backend-mysql      backend/mysql_db_password password uv"| debconf-set-selections
echo "unifiedviews-backend-mysql      backend/mysql_db_user string uv"| debconf-set-selections

##############################################################
# DB installations, etc which have customised values.

apt-get install -y mysql-server-5.5 mysql-server
apt-get install -y virtuoso-opensource
apt-get install -y iceweasel
apt-get update

###############################################################
# Unified views pulling, packaging and hopefully the installation.
#
cd ${HOME}
if [ -d Packages ]
then
    cd Packages ; git pull
else
    git clone --branch  UV_v1.5.5 https://github.com/UnifiedViews/Packages.git
fi
cd ${HOME}/Packages
mvn package
cd target
cp ${HOME}/Packages/*/target/*.deb .
echo "****** packages built - installing"
dpkg -i unifiedviews-*mysql*1.5.5_all.deb unifiedviews-backend-shared-1.5.5_all.deb unifiedviews-backend-1.5.5_all.deb unifiedviews-webapp_1.5.5_all.deb unifiedviews-webapp-shared_1.5.5_all.deb
apt-get -f -y install 

###############################################################
# Include the plugins?
echo "****** unified views installed - build plugins"

cd ${HOME}
if [ -d Plugins ]
then
   cd Plugins ; git pull
else
       git clone --branch UV_Plugins_v1.5.5 https://github.com/UnifiedViews/Plugins.git
fi
cd ${HOME}/Plugins
mvn install -Dprofile=extract-jars
echo "****** copy all plugins"
cp -fr /var/lib/unifiedviews/target/dpu /var/lib/unifiedviews/target/dpu.old
for i in e* t* l* ; do mkdir -p /var/lib/unifiedviews/target/dpu/uv-$i; cp $i/target/uv-$i*.jar /var/lib/unifiedviews/target/dpu/uv-$i; done
# Remove duplicate line??
sed -e '24d'  plugins.sql | mysql -uroot -proot unifiedviews

###############################################################
# Setup other services
update-rc.d unifiedviews-backend defaults
# Allows login without password
echo "vagrant ALL=(ALL) NOPASSWD:ALL" > /etc/sudoers.d/vagrant
echo "****** done with bootstrap"
