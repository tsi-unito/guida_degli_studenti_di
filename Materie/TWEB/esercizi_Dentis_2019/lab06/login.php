<?php
//check if the user is sending a request or is just here to compile the authentication form
if (isset($_REQUEST["username"]) && isset($_REQUEST["password"])) {
    header("Content-type:application/json");
    try {
        $db = new PDO ("mysql:dbname=film;host=192.168.1.36", "scuola", "scuola");
        $db->setAttribute(PDO::ATTR_ERRMODE, PDO::ERRMODE_EXCEPTION);
    } catch (PDOException $ex) {
        die(json_encode(array( 'error' => array('message' => 'database auth error',"errCode" => 'DBConnectionError','exception' => $ex->getMessage()))));
    }
    //if the credential are right send a positive response, else return an error.
    if (checkCredential($db)){
        //if credentials are right set session parameters
        session_start();
        $_SESSION["username"]=$_REQUEST["username"];
        die(json_encode(array('sessionInfo' => array('session' => true, 'autentication' => true))));
    }else {
        die(json_encode(array('sessionInfo' => array('session' => false, 'autentication' => true), 'error' => array('message' => 'wrong username or password','errCode' => 'wrongCredentials'))));
    }
}

//retrieve the encrypted password of the user from the database. If the password is correct return true
function checkCredential($db){
    $user=$db->quote($_REQUEST["username"]);
    $pass=$db->quote($_REQUEST["password"]);
    $result = $db->query("select Username,Password from user where Username = $user;");
    $result = $result->fetch()['Password'];
    return $result == hash('sha256',substr($pass,1,strlen($pass)-2));
}
?>
<?php include("top.html");?>

<script src="login.js" type="text/javascript"></script>
</head>
<body>

<?php include("banner.html");?>

    <h1>The One Degree of Kevin Bacon</h1>
    <fieldset>
        <legend>Log in</legend>
        <div id="errMsg"></div>
        <div id="auth">
            <input name="username" type="text" size="12" placeholder="username">
            <input name="password" type="password" size="12" placeholder="password">
            <input name="submit" type="submit">
        </div>
    </fieldset>
</div>

<div id="w3c">
    <a href="http://validator.w3.org/check/referer">
        <img width="88" src="https://upload.wikimedia.org/wikipedia/commons/b/bb/W3C_HTML5_certified.png" alt="Valid HTML5!">
    </a>
    <a href="http://jigsaw.w3.org/css-validator/check/referer">
        <img src="http://jigsaw.w3.org/css-validator/images/vcss" alt="Valid CSS" >
    </a>
</div>
</body>
</html>
