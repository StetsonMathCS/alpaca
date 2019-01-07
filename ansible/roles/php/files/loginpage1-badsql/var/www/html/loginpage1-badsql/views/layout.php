<!DOCTYPE html>
<html>
<head>
    <!-- Compiled and minified CSS -->
    <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/materialize/0.98.0/css/materialize.min.css">
    <!-- Compiled and minified JavaScript -->
    <script src="https://cdnjs.cloudflare.com/ajax/libs/materialize/0.98.0/js/materialize.min.js"></script>
    <!-- Material Icons -->
    <link href="https://fonts.googleapis.com/icon?family=Material+Icons" rel="stylesheet">
    <style>
        body {
            display: flex;
            min-height: 100vh;
            flex-direction: column;
        }

        main {
            flex: 1 0 auto;
        }

    </style>
</head>
<body>
<header>
    <nav class="blue lighten-2" role="navigation">
        <?php if(!isset($_SESSION['authorized']) && $_SESSION['authorized'] !== 1) { ?>
            <div class="nav-wrapper container"><a id="logo-container" href="?controller=main&action=home" class="brand-logo">Alpaca</a>
                <ul class="right hide-on-med-and-down">
                    <li><a href='?controller=access&action=login'>Login</a></li>
                </ul>

                <ul id="nav-mobile" class="side-nav">
                    <li><a href='?controller=access&action=login'>Login</a></li>
                </ul>
                <a href="#" data-activates="nav-mobile" class="button-collapse"><i class="material-icons">menu</i></a>
            </div>
        <?php } else if(isset($_SESSION['authorized']) && $_SESSION['authorized'] === 1) { ?>
            <div class="nav-wrapper container"><a id="logo-container" href="?controller=user&action=dashboard" class="brand-logo">Alpaca</a>
                <ul class="right hide-on-med-and-down">
                    <li><a href="?controller=access&action=logout">Logout</a></li>
                </ul>

                <ul id="nav-mobile" class="side-nav">
                    <li><a href="?controller=access&action=logout">Logout</a></li>
                </ul>
                <a href="#" data-activates="nav-mobile" class="button-collapse"><i class="material-icons">menu</i></a>
            </div>
        <?php } ?>
    </nav>
</header>

<main>
    <?php require_once('routes.php'); ?>
</main>

<footer class="page-footer blue lighten-2">
    <div class="container">
        <div class="row">
            <div class="col l6 s12">
                <h5 class="white-text">Info</h5>
                <p class="grey-text text-lighten-4">Simple Web Application for Alpaca. Use for practicing SQL Injection.</p>
            </div>
        </div>
    </div>
    <div class="footer-copyright">
        <div class="container">
            Made by <a class="orange-text text-lighten-3" href="http://materializecss.com">Materialize</a>
        </div>
    </div>
</footer>
</body>
</html>