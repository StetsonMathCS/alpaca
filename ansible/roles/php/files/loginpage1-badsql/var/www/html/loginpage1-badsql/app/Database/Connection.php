<?php
namespace App\Database;

class Connection {
    private static $instance = null;

    const DB_HOST = 'localhost';
    const DB_NAME = 'logindb1';
    const DB_USER = 'root';
    const DB_PASS = 'password';
    const DB_PORT = '3306';
    const DB_CHAR = 'utf8';

	private function __construct() {}
	private function __clone() {}

	public static function instance()
	{
		if (self::$instance === null)
		{
			$opt  = array(
					\PDO::ATTR_ERRMODE            => \PDO::ERRMODE_EXCEPTION,
					\PDO::ATTR_DEFAULT_FETCH_MODE => \PDO::FETCH_ASSOC,
					\PDO::ATTR_EMULATE_PREPARES   => FALSE,
			);
			$dsn = 'mysql:host='.self::DB_HOST.';dbname='.self::DB_NAME.';port='.self::DB_PORT.';charset='.self::DB_CHAR;
			self::$instance = new \PDO($dsn, self::DB_USER, self::DB_PASS, $opt);
		}
		return self::$instance;
	}

	public static function __callStatic($method, $args)
	{
		return call_user_func_array(array(self::instance(), $method), $args);
	}

	public static function run($sql)
    {
        $stmt = self::instance()->query($sql);
		//$stmt->execute($args);
		return $stmt;
	}

	public static function run_multiple($sql, $args = [])
    {
        $stmt = self::instance()->prepare($sql);
        $result = array();
        foreach ($args as $value)
        {
            $stmt->execute([$value]);
            $result[$value] = $stmt->fetchAll();
        }
        return $result;
    }
}
