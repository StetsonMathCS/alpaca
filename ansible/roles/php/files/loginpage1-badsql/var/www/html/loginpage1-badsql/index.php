<?php
require_once __DIR__ . '/vendor/autoload.php';
session_start();
//ini_set('display_errors', 'on');

if ($_SESSION['authorized'] !== 1) {
    $controller = 'access';
    $action = 'login';
}
else if (isset($_GET['controller']) && isset($_GET['action'])) {
    $controller = $_GET['controller'];
    $action = $_GET['action'];
} else {
    $controller = 'main';
    $action = 'home';
}

require_once ('views/layout.php');
?>