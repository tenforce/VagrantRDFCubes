#!/usr/bin/env bash
#################################################################
# Install the necessary components for building and installing
# the RDF Data Cube Examples and the lodstatworkbench (eventually)
#

#################################################################
# Setup the extra source lists (lod2 stack and prolog)
#
apt-get install -y python-software-properties software-properties-common
apt-add-repository -y ppa:swi-prolog/stable > log
echo "deb http://stack.linkeddata.org/deb/ ldstack main" >> /etc/apt/sources.list
# extra 4E34CBDD64B8E176
apt-get update -y --force-yes 

apt-get install -y dkms virtualbox-guest-dkms virtualbox-guest-x11
apt-get install --no-install-recommends -y ubuntu-desktop
apt-get install -y iceweasel

# Now start to setup basics, etc.
apt-get install -y openjdk-7-jre openjdk-7-jdk
apt-get install -y git bash make curl sed

# Install the latest version of swi-prolog (not the one from precise)
apt-get install -y swi-prolog dos2unix

###############################################################
# Set the default values for the debconf questions
#
apt-get install -y debconf-utils
echo "virtuoso-opensource-7.1 virtuoso-opensource-7.1/dba-password-again password root" | debconf-set-selections
echo "virtuoso-opensource-7.1 virtuoso-opensource-7.1/dba-password password root" | debconf-set-selections

##############################################################
# installation of the LOD2 tools, etc. which have customised 
# values.
apt-get install -y --force-yes lod2statworkbench
# Change the default homepage
sed -i 's|\("browser.startup.homepage",\) "\(.*\)"|\1 "http://localhost:8080/lod2statworkbench"|' .mozilla/firefox/*.default/prefs.js

##############################################################
# Allows login without password (locally)
echo "vagrant ALL=(ALL) NOPASSWD:ALL" > /etc/sudoers.d/vagrant
chmod 440 /etc/sudoers.d/vagrant
echo "****** done with main bootstrap creation"

##############################################################
# Now rebuild the datafiles and add the cubes to the virtuoso
# system.
 
cp /vagrant/virtuoso.ini /etc/virtuoso-opensource-7.1/
service virtuoso-opensource-7.1 restart
( cd /vagrant_opendata ; make -k all )

# Cleanup anything left around (switch off release upgrader)
apt-get remove ubuntu-release-upgrader-core
apt-get autoclean
echo "****** done with bootstrap of RDF DataCube test machine"
