docker run -d -v /home/sloby/projects/sandbox/r-jdbc/data:/data -p 3306:3306 -e MYSQL_PASS="masterkey" -e ON_CREATE_DB="dbs" -e STARTUP_SQL="/data/export.sql" --name="mysql" tutum/mysql
mysql -uadmin -pPASSWORD -h 127.0.0.1 -P3306
docker run -d -v /home/sloby/projects/sandbox/r-jdbc/data:/databases -p 3050:3050 --name="fb" firebird
