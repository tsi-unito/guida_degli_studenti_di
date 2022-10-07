<?php include("html/top.html")?>
    <link rel="stylesheet" type="text/css" href="css/login.css">
    <link href="img/favicon.ico" type="image/png" rel="shortcut icon" >
    <script src="js/login.js" type="text/javascript"></script>
</head>
<body>
    <nav><a href="index.php">Gambrinus<img src="img/logo_circular.jpg" alt="Gambrinus logo"></a> <button id="log_in_out" >Sign Up</button></nav>
    <div class="frames">
        <img id="imgWide" src="img/logo.jpg" alt="logo Gambrinus">
        <p class="error"></p>
        <div id="loginBox">
            <input name="username" type="text" size="12" placeholder="username">
            <input name="password" type="password" size="12" placeholder="password">
            <input name="submitLogin" type="submit" value="Login">
        </div>
    </div>

    <div class="frames" id="signUpBox" >
        <h1>Registrati</h1>
        <ul>
            <li>
                <label class="descriptor">Nome:</label>
                <input name="name" type="text" size="12" placeholder="nome">
            </li>
            <li>
                <label class="descriptor">Username:</label>
                <input name="username" type="text" size="12" placeholder="username">
            </li>
            <li>
                <label class="descriptor">Password:</label>
                <input name="password" type="password" size="12" placeholder="password">
            </li>
            <li>
                <label class="descriptor">Ruolo:</label>
                <input type="radio" name="role" value="cook" class="inputRight"><label class="inputRight">Cucina</label>
                <input type="radio" name="role" value="waiter" checked class="inputRight"><label class="inputRight">Sala</label>
            </li>
        </ul>
        <p class="error"></p>
        <input name="submitSign" type="submit" value="Register">
    </div>
<?php include("./html/bottom.html") ?>