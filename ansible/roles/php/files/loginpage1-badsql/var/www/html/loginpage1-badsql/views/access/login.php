<!DOCTYPE html>
<html>
<head>
    <link href="https://fonts.googleapis.com/icon?family=Material+Icons" rel="stylesheet">
    <link rel="stylesheet" type="text/css" href="https://cdnjs.cloudflare.com/ajax/libs/materialize/0.97.5/css/materialize.min.css">
    <style>
        body {
            display: flex;
            min-height: 100vh;
            flex-direction: column;
        }
        main {
            flex: 1 0 auto;
        }
        body {
            background: #fff;
        }
        .input-field input[type=date]:focus + label,
        .input-field input[type=text]:focus + label,
        .input-field input[type=email]:focus + label,
        .input-field input[type=password]:focus + label {
            color: #e91e63;
        }
        .input-field input[type=date]:focus,
        .input-field input[type=text]:focus,
        .input-field input[type=email]:focus,
        .input-field input[type=password]:focus {
            border-bottom: 2px solid #e91e63;
            box-shadow: none;
        }
    </style>
</head>
<body>
<div class="section"></div>
<main>
    <h4 class="blue-text center">Login</h4>
    <div class="section"></div>
    <div class="container center-align">
        <div class="z-depth-1 grey lighten-4 row" style="display: inline-block; padding: 32px 48px 0px 48px; border: 1px solid #EEE;">
            <form class="col s12" method="post">
                <div class='row'>
                    <div class='input-field col s12 left-align'>
                        <input type='text' name='username' id='username' />
                        <label for='username'>Enter your username</label>
                    </div>
                </div>
                <div class='row'>
                    <div class='input-field col s12 left-align'>
                        <input type='password' name='password' id='password' />
                        <label for='password'>Enter your password</label>
                    </div>
                </div>
                <input type='submit' name='submit' value='Login' />
            </form>
        </div>
    </div>
    <div class="section"></div>
    <div class="section"></div>
</main>
</body>
<script type="text/javascript" src="https://cdnjs.cloudflare.com/ajax/libs/jquery/2.2.1/jquery.min.js"></script>
<script type="text/javascript" src="https://cdnjs.cloudflare.com/ajax/libs/materialize/0.97.5/js/materialize.min.js"></script>
</html>


<!--<form method='post'>-->
<!--    <p>-->
<!--        Username: <input type='text' name='username' />-->
<!--    </p>-->
<!--    <p>-->
<!--        Password: <input type='password' name='password' />-->
<!--    </p>-->
<!--    <p>-->
<!--        <input type='submit' name='submit' value='Login' />-->
<!--    </p>-->
<!--</form>-->