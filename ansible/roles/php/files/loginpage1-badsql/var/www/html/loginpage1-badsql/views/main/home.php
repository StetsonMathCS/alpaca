<?php $userData = $_SESSION['data']; ?>

<!DOCTYPE html>
<html>
<head>
    <script src="https://ajax.googleapis.com/ajax/libs/jquery/2.1.1/jquery.min.js"></script>
    <script src="https://cdnjs.cloudflare.com/ajax/libs/materialize/0.97.7/js/materialize.min.js"></script>
    <link href="https://fonts.googleapis.com/icon?family=Material+Icons" rel="stylesheet">
    <title>Account</title>
    <style>
        body {
            background: ivory;
        }
    </style>
</head>
<body>

<div class="section"></div>
<div class="container">
    <div class="row">
        <div id="profile-page-wall-posts" class="row">
            <div class="col s12">

                <?php foreach($userData as $data) { ?>
                    <div class="container">
                        <div class="row valign-wrapper">
                            <div class="col s12 valign">
                                <div class="card blue darken-2">
                                    <div class="card-content white-text">
                                        <span class="card-title">ID: <?= $data['users_id'] ?></span>
                                            <p>Username: <?= $data['users_username'] ?></p>
                                            <p>Password: <?= $data['users_password'] ?></p>
                                    </div>
                                </div>
                            </div>
                        </div>
                    </div>

                <?php } ?>

            </div>
        </div>

    </div>
</div>
</body>
</html>
