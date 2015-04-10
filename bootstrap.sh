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
apt-get install -y swiprolog
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

# Setup for expanding the source lists
echo "deb http://stack.linkeddata.org/deb/ ldstack main" >> /etc/apt/sources.list
apt-get update upgrade

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

apt-get install -y lod2statworkbench
apt-get install -y mysql-server-5.5 mysql-server
apt-get install -y virtuoso-opensource
apt-get install -y iceweasel
apt-get update

# Allows login without password
echo "vagrant ALL=(ALL) NOPASSWD:ALL" > /etc/sudoers.d/vagrant
echo "****** done with bootstrap"
