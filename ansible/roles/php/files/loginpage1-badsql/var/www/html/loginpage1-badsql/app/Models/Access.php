<?php
namespace App\Models;

use App\Database\Connection as db;

class Access
{
    public function validate() {
        $user = $_POST['username'];
        $password = $_POST['password'];

        $find_user = "select * from users where users_username = '$user'";
        $stmt = db::run($find_user);

        $row = $stmt->fetchAll();
        foreach($row as $element) {
            if(password_verify($password, $element['users_password'])) {
                $_SESSION['authorized'] = 1;
                $_SESSION['data'] = $row;
                return true;
            }
        }
        return false;
    }
    
    public function logout()
    {
        session_unset();
        session_destroy();
        session_write_close();
    }

}
