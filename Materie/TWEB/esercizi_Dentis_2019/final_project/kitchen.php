<?php
if(!isset($_SESSION)) {
    session_start();
}
if ($_SESSION["role"] != "cook"){
    header("Location: /index.php");
    die();
}
include ("./html/top.html");
?>
    <link rel="stylesheet" type="text/css" href="css/kitchen.css">
    <script src="js/kitchen.js" type="text/javascript"></script>
</head>
<body>
    <nav><a href="index.php">Gambrinus<img src="img/logo_circular.jpg" alt="Gambrinus logo"></a> <button id="log_in_out" >Log Out</button></nav>
<div id="orders">

</div>


<?php

include ("./html/bottom.html");
?>
