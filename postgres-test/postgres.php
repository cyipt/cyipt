<?php

/* Install (on a Mac) using:

# Install PostgreSQL main program and PostGIS plugin
brew install postgresql
brew install postgis

# Install pgsql module for PHP; in this example we firstly check the PHP version, search for available module names, and having found the right item, install
ls /usr/local/Cellar/|grep php
brew search php71
brew install php71-pdo-pgsql

# Start Postgres and enable at startup
# See: https://www.codementor.io/devops/tutorial/getting-started-postgresql-server-mac-osx
pg_ctl -D /usr/local/var/postgres start && brew services start postgresql

# Show version, to confirm successful installation
postgres -V

# Connect to Postgres on the command-line, as the root user 'postgres'
psql postgres

# Create a database, and list databases to confirm it is there
CREATE DATABASE cyipt;
\list

# Create a new user, and then list users to confirm it is there
CREATE ROLE cyipt WITH LOGIN PASSWORD 'cyipttest';
\du

# Add permissions for that user
GRANT ALL PRIVILEGES ON DATABASE cyipt TO cyipt;
\du

# Exit
\q

# Can connect to the database using: psql DBNAME USERNAME
# GUI also available at: https://www.pgadmin.org/
psql cyipt cyipt

# Run this file using
php postgres.php

*/


?>
