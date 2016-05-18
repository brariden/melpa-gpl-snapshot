- [README](#readme)
  - [Introduce](#introduce)
  - [Install](#install)
  - [Configure](#configure)
  - [Usage](#usage)

# README<a id="org76fd9fa"></a>

## Introduce<a id="org36ab8d0"></a>

sql-mssql is a sql.el extension, which can let Linux/Unix user connect
Microsoft SQL Server with the help of sqsh, this extension may useless
for Windows user.

## Install<a id="orga276551"></a>

1.  Install sqsh, unixODBC and freetds (For example: apt-get install sqsh unixodbc freetds)
2.  Install sql-mssql (You can config Melpa and install it with \`package-install' command)

## Configure<a id="org14b35ca"></a>

1.  Config unixODBC, you can view the below documents as reference
    1.  <http://richbs.org/post/43142767072/connecting-to-microsoft-sql-server-from-unix>
    2.  <http://askubuntu.com/questions/167491/connecting-ms-sql-using-freetds-and-unixodbc-isql-no-default-driver-specified>
    3.  <http://help.interfaceware.com/kb/904>
    4.  Google: unixodbc mssql
2.  Config sql.el

        (require 'sql)
        (require 'sql-mssql)

## Usage<a id="orgd18d2ae"></a>

1.  Method1: M-x sql-mssql
2.  Method2: M-x sql-mssql-connect (you must set \`sql-connection-alist' before)
