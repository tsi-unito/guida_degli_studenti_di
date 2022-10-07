<?php
include ("database.php");

header("Content-type:application/json");

//check if get/post parameters are set properly. this strange boolean expression check if the user has send user and password (only in case the user requested a login)
//if the request is a logout the parameters user and password doesn't have to be set
if (!isset($_REQUEST['login']) || ($_REQUEST["login"] != "logout" && (!isset($_REQUEST['username']) || !isset($_REQUEST['password']) || $_REQUEST['username'] == "" || $_REQUEST['password'] == ""))){
    die(json_encode(array('info' => $_REQUEST,'error' => array('title' => 'Unauthorized', 'status' => 401,'detail' => 'no login parameter specified'))));
}

//decide whatever the user wants to login, create a new account or logout
if ($_REQUEST["login"] == "new"){
    //if an old session is active destroy it before the new login
    if (isset($_SESSION["UID"])) {
        $id = $_SESSION["UID"];
        $role = $_SESSION["role"];
        session_unset();
    }
    $id = register($_REQUEST["name"],$_REQUEST["username"],$_REQUEST["password"],$_REQUEST["role"]);
    $_SESSION["UID"] = $id;
    $info = getUserInfo($id);
    $_SESSION["role"] = $info["cucina"] ? "cook" : "waiter";
    die(json_encode(array('info' => $_REQUEST,'response' => array('message' => 'User successfully registered', 'id' => $id ,'role' => $_SESSION["role"] ))));
}else if($_REQUEST["login"] == "existing"){
    //if an old session is active destroy it before the new login
    if (isset($_SESSION["UID"])){
        $id = $_SESSION["UID"];
        $role = $_SESSION["role"];
        session_unset();}
    $id = login($_REQUEST["username"],$_REQUEST["password"]);
    if (is_null($id)){
        die(json_encode(array('info' => $_REQUEST,'response' => array('message' => 'authentication failed', 'id' => "" ,'role' => ''),'error' => array('title' => 'Unauthorized', 'status' => 401,'detail' => 'Wrong credentials'))));
    }
    else
        $_SESSION["UID"] = $id;
    $info = getUserInfo($id);
    $_SESSION["role"] = $info["cucina"] ? "cook" : "waiter";
    die(json_encode(array('info' => $_REQUEST,'response' => array('message' => 'authentication succeeded', 'id' => $id , 'role' => $_SESSION["role"]))));

}else{
    //log out or manual injection of parameter, either way i want to respond in the same way, closing the session if it's open
    if (isset($_SESSION["UID"])){
        $id = $_SESSION["UID"];
        $role = $_SESSION["role"];
        session_unset();
        session_destroy();
        die(json_encode(array('info' => $_REQUEST,'response' => array('message' => 'logout succeeded', 'id' => $id , 'role' => $role))));
    }

    die(json_encode(array('info' => $_REQUEST,'response' => array('message' => 'logout succeeded', 'id' => '' , 'role' => ''))));
}


?>

