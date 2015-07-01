#!/usr/bin/env bash

set -e

PM=

set_package_manager() {
    if [ "$(command -v apt-get)" != "" ]; then
        PM=apt-get
    elif [ "$(command -v dnf)" != "" ]; then
        PM=dnf
    elif [ "$(command -v yum)" != "" ]; then
        PM=yum
    elif [ "$(command -v packman)" != "" ]; then
        PM=packman
    fi
}

if [ "$(id -u)" != "0" ]; then
    echo "This script must be run as root" 1>&2
    exit 1
fi

set_package_manager

echo "Using package manager $PM"

if [ "$(command -v mysqladmin)" = "" ]; then
    sudo $PM -y install mariadb-server
else
    echo "MariaDB server found"
fi

if [ "$(command -v mysql)" = "" ]; then
    sudo $PM -y install mariadb
else
    echo "MariaDB client found"
fi

if [ "$(command -v searchd)" = "" ]; then
    sudo $PM -y install sphinx
else
    echo "Sphinx found"
fi

read -r -p "Is MariaDB server running? [y/N] " response
if [[ $response =~ ^([nN][oO]|[nN])$ ]]; then
    sudo service mysqld start
fi
   
read -r -p "Should I create a root MariaDB account? [y/N] " response
if [[ $response =~ ^([yY][eE][sS]|[yY])$ ]]; then
    read -s -p "Password for root user: " root_password
    sudo mysqladmin -u root password $root_password
fi

mysql -u root -p -e "
CREATE DATABASE emacs_user;
GRANT ALL ON emacs_user.* TO emacs@localhost IDENTIFIED BY 'emacs';
USE emacs_user;
CREATE TABLE documents (id INT NOT NULL AUTO_INCREMENT PRIMARY KEY,
       document VARCHAR(256) NOT NULL UNIQUE);
"
