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



# Define the settings, using the credentials above
$settings = array (
	'hostname' => 'localhost',
	'username' => 'cyipt',
	'password' => 'cyipttest',
	'database' => 'cyipt',
);

# Connect to the database
# We use the PDO database abstraction library, and provide a DSN connection string in this format: 'pgsql:host=localhost;dbname=example'
try {
	$databaseConnection = new PDO ("pgsql:host={$settings['hostname']};dbname={$settings['database']}", $settings['username'], $settings['password']);
	$databaseConnection->setAttribute (PDO::ATTR_ERRMODE, PDO::ERRMODE_EXCEPTION);
} catch (PDOException $e) {
	print "Error!: " . $e->getMessage ();
	die;
}

# Create a table (if not already present)
$query = "
	CREATE TABLE IF NOT EXISTS people (
		id SERIAL PRIMARY KEY,
		name VARCHAR(255) NOT NULL,
		location VARCHAR(255) NOT NULL,
		created_at TIMESTAMP DEFAULT NOW()
	);
";
try {
	$databaseConnection->query ($query);
} catch (PDOException $e) {
	echo "Error!: " . $e->getMessage ();
	die;
}

# Insert data
$query = "
	INSERT INTO people (name, location)
	VALUES
		('Martin', 'Cambridge'),
		('Robin', 'Leeds'),
		('Malcolm', 'Leeds');
";
$databaseConnection->query ($query);

# Select the data
$query = "SELECT * FROM people;";
foreach ($databaseConnection->query ($query) as $row) {
	echo "{$row['name']} is from {$row['location']}; record created at {$row['created_at']}\n";
}

# Delete all existing data
$query = "DELETE FROM people;";
$databaseConnection->query ($query);

?>
