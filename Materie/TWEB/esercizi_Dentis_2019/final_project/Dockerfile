FROM ubuntu:18.04

RUN apt-get update && \
    apt-get install -y software-properties-common && \
    rm -rf /var/lib/apt/lists/* && \
    apt-add-repository ppa:ondrej/php

RUN apt-get update && apt-get -y upgrade && DEBIAN_FRONTEND=noninteractive apt-get -y install \
    apache2 php7.0 php7.0-mysql libapache2-mod-php7.0

# Enable apache mods.
RUN a2enmod php7.0
RUN a2enmod rewrite

# Update the PHP.ini file, enable <? ?> tags and quieten logging.
RUN sed -i "s/short_open_tag = Off/short_open_tag = On/" /etc/php/7.0/apache2/php.ini
RUN sed -i "s/error_reporting = .*$/error_reporting = E_ERROR | E_WARNING | E_PARSE/" /etc/php/7.0/apache2/php.ini

# Manually set up the apache environment variables
ENV APACHE_RUN_USER www-data
ENV APACHE_RUN_GROUP www-data
ENV APACHE_LOG_DIR /var/log/apache2
ENV APACHE_LOCK_DIR /var/lock/apache2
ENV APACHE_PID_FILE /var/run/apache2.pid

EXPOSE 80

RUN rm -r /var/www/html/*

WORKDIR /var/www/html

COPY ./ /var/www/html
COPY ./entrypoint.sh /var/www/html

RUN chmod +x ./entrypoint.sh

RUN echo "installing mariadb"

RUN \
  apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 0xcbcb082a1bb943db && \
  echo "add-apt-repository 'deb [arch=amd64,arm64,ppc64el] http://sfo1.mirrors.digitalocean.com/mariadb/repo/10.3/ubuntu `lsb_release -cs` main" && \
  apt-get update && \
  DEBIAN_FRONTEND=noninteractive apt-get install -y mariadb-server && \
  rm -rf /var/lib/apt/lists/* && \
  apt install mariadb-server

ENV DATABASE_ROOT_PASS password

RUN service mysql start \
    && mysqladmin -u root password "$DATABASE_ROOT_PASS" \
    && mysql -u root -p"$DATABASE_ROOT_PASS" -e "UPDATE mysql.user SET Password=PASSWORD('$DATABASE_ROOT_PASS') WHERE User='root'" \
    && mysql -u root -p"$DATABASE_ROOT_PASS" -e "DELETE FROM mysql.user WHERE User='root' AND Host NOT IN ('localhost', '127.0.0.1', '::1')" \
    && mysql -u root -p"$DATABASE_ROOT_PASS" -e "DELETE FROM mysql.user WHERE User=''" \
	&& mysql -u root -p"$DATABASE_ROOT_PASS" -e "CREATE USER 'scuola'@'%' IDENTIFIED BY 'scuola'" \
	&& mysql -u root -p"$DATABASE_ROOT_PASS" -e "GRANT ALL PRIVILEGES ON * . * TO 'scuola'@'%'" \
    && mysql -u root -p"$DATABASE_ROOT_PASS" -e "DELETE FROM mysql.db WHERE Db='test' OR Db='test\_%'" \
    && mysql -u root -p"$DATABASE_ROOT_PASS" -e "FLUSH PRIVILEGES" \
    && mysql -u scuola -p"scuola" -e "source ./database.sql" 


# Define mountable directories.
VOLUME ["/etc/mysql", "/var/lib/mysql"]

CMD ./entrypoint.sh