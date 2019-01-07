<?php
namespace App\Controllers;

use App\Renderer as Renderer;
use App\Models\Access as Access;

class AccessController
{

    public function login()
    {
        if (isset($_SESSION['authorized']) and ($_SESSION['authorized'] == 1)) {
            $view = new Renderer('views/main/');
            $view->render('home.php');
        } else  {
            if(isset($_POST['username']) and isset($_POST['password'])) {
                $model = new Access();
                if ($model->validate()) {
                    header("Location: index.php");
                }
            }

            $view = new Renderer('views/access/');
            $view->render('login.php');
        }
    }

    public function logout()
    {
        if (! isset($_SESSION['authorized'])) {
            $view = new Renderer('views/main/');
            $view->title = 'You are not logged in';
            $view->render('home.php');
        } else {

            $model = new Access();
            $model->logout();
            header("Location: index.php");
            
        }
    }
}
