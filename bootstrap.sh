#!/usr/bin/env bash
#################################################################
# Install the necessary components for building and installing
# the RDF Data Cube Examples and the lodstatworkbench (eventually)
#
apt-get update
apt-get upgrade -y
apt-get install -y xinit
apt-get install -y xterm
apt-get install -y gnome-terminal
apt-get install -y gnome-shell
apt-get install -y debconf-utils
apt-get install -y gnome-shell
apt-get install -y gdm

# Now start to setup for building unified views, etc.
apt-get install -y openjdk-7-jre
apt-get install -y openjdk-7-jdk
apt-get install -y git
apt-get install -y bash

# Install the latest version of swi-prolog (not the one from precise)
apt-get install -y make
apt-get install -y python-software-properties software-properties-common
apt-add-repository -y ppa:swi-prolog/stable > log
apt-get update -y
apt-get install -y swi-prolog
apt-get install -y dos2unix

# Setup for expanding the source lists
# echo "deb http://stack.linkeddata.org/deb/ ldstack main" >> /etc/apt/sources.list
apt-get update && apt-get -y upgrade

###############################################################
# Set the default values for the debconf questions
#
echo "virtuoso-opensource-6.1 virtuoso-opensource-6.1/dba-password-again password root" | debconf-set-selections
echo "virtuoso-opensource-6.1 virtuoso-opensource-6.1/dba-password password root" | debconf-set-selections

##############################################################
# installation of the LOD2 tools, etc. which have customised 
# values.
apt-get install -y virtuoso-opensource
apt-get install -y lod2statworkbench
apt-get install -y iceweasel

##############################################################
# Allows login without password (locally)
echo "vagrant ALL=(ALL) NOPASSWD:ALL" > /etc/sudoers.d/vagrant
chmod 440 /etc/sudoers.d/vagrant
echo "****** done with main bootstrap creation"

##############################################################
# Now rebuild the datafiles and add the cubes to the virtuoso
# system.

cp /vagrant/virtuoso.ini /etc/virtuoso-opensource-6.1/
service virtuoso-opensource-6.1 restart
( cd /vagrant_opendata ; make -k all )

echo "****** done with bootstrap of RDF DataCube test machine"
