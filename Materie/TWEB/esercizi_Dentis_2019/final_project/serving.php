<?php
if(!isset($_SESSION)) {
    session_start();
}
if ($_SESSION["role"] != "waiter"){
    header("Location: /index.php");
    die();
}
include ("./html/top.html");
?>
<link rel="stylesheet" type="text/css" href="css/serving.css">
<script src="js/serving.js" type="text/javascript"></script>
</head>
<body>
<nav><ul><li id="logo"><a href="index.php">Gambrinus<img src="img/logo_circular.jpg" alt="logo Gambrinus"></a></li><li id="log_in_out_item"><button id="log_in_out" >Log Out</button></li></ul></nav>
<div id="mainBox">
    <div class="column" id="tableDiv">
        <h2>Liberi</h2>
        <ul id="freeTable"></ul>
        <h2>Da servire</h2>
        <ul id="occupiedTable"></ul>
    </div>
    <div class="column" id="orderDiv">
        <h1>Tavolo <span id="tableNum">-1</span></h1>
        <ul id="order">
        </ul>


    </div>
    <div id="orderAction">
        <button id="send" class="orderAction">Invia</button>
        <button id="delete" class="orderAction">Elimina</button>
    </div>
</div>
<p id="userMessage"></p>

<?php
include ("./html/bottom.html");
?>
