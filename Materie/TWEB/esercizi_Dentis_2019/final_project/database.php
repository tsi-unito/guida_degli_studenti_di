<?php

//this file act as a "model" to implement connection with database. every function that uses the db is written here
if(!isset($_SESSION)) {
    session_start();
}

header("Content-type:application/json");
//anyway, every function needs a db connection, so this is the first thing to do
try {
    $db = new PDO ("mysql:dbname=servizio;host=127.0.0.1", "scuola", "scuola");
    $db->setAttribute(PDO::ATTR_ERRMODE, PDO::ERRMODE_EXCEPTION);
} catch (PDOException $ex) {
    http_response_code(503);
    die(json_encode(array('info' => array($_REQUEST),'error' => array('title' => 'database auth error', 'status' => 503,'detail' => 'the database is not running or we have problem accessing it'))));
}

/** function register creates a new user, it may close the connection
 * @return string the id of the newly created user
 */
function register($name,$username,$password,$role){
    global $db;
    $name = strip_tags($name);
    $username = strip_tags($username);
    $role = strip_tags($role);
    $nameQuoted = $db->quote($name);
    $usernameQuoted = $db->quote($username);
    $cook = $role == "cook" ? 1 : 0;
    $password = $db->quote(hash('sha256',$password));
    try{
        $db->query("insert into Users values (0,$nameQuoted,$usernameQuoted,$password,$cook);");
    }catch (PDOException $exception){
        if ($exception->getCode() == 23000) //code 2300 is the MySql code for duplicate entry
            die(json_encode(array('info' => $_REQUEST,'error' => array('title' => 'user already registered', 'status' => 422,'detail' => 'the user "' . $username .'" is already registered'))));
        else
            die(json_encode(array('info' => $_REQUEST,'error' => array('title' => 'user already registered', 'status' => 422,'detail' => 'error inserting the user in our database'))));

    }
    return $db->query("select id from Users where username = $usernameQuoted") ->fetch()[0];
}

/**
 * function login checks credential and opens a new session saving the id of the connected user
 * @return mixed|null the id of the user that made the connection or null if the credentials are incorrect
 */
function login($username,$password){
    global $db;
    $user=$db->quote($username);
    $pass=$db->quote($password);
    $result = $db->query("select * from Users where Username = $user;")->fetch(PDO::FETCH_ASSOC);
    if (!$result) //the user doesn't exist, checking for password may cause an error
        return null;
    if ($result['password'] == hash('sha256',substr($pass,1,strlen($pass)-2))) {
        return $result["id"];
    }
    else return null;
}

/**
 * given the type of the plate returns every item in the menu of given category
 * @param $category the name of the category (hamburgher, beer, etc)
 * @return array all the plates of the requested category as a string couple of id and name
 */
function getMenu($category){
    global $db;
    $category = $db ->quote($category);

    $response = $db->query("select id, nome from Menu where categoria = $category;");

    if ($response->rowCount() == 0){
        //possible html or sql injection
    }else
         return $response->fetchAll(PDO::FETCH_ASSOC);
}

/** return all the information of a given user
 * @param $id the id of the user as written in the DB
 * @return mixed an array of string containig all user info or PDO error
 */
function getUserInfo ($id){
    global $db;
    $id = $db->quote($id);
    return $db->query("select * from Users where id = $id")->fetch(PDO::FETCH_ASSOC);
}

/**
 * get all the orders of every table
 * @return array an array containing one line for each plate of the various orders
 */
function getOrders(){
    global $db;
    $query = $db->query("select Ordine.id as id, tavolo,nome,categoria,quantita from Ordine join Menu M on M.id = Ordine.piattoId");
    $response = [];
    $i=-1;
    while($row = $query->fetch(PDO::FETCH_ASSOC)){
        if ($row['id'] != $i){
            //need to allocate a new array inside $response
            $i=$row['id'];
            $response[$row['id']]= array();
        }
        array_push($response[$row['id']],$row);
    }
    return $response;
}

/**
 * get all the orders of a given table
 * @param $table numeric identificator of the requested table
 * @return array an array containing one line for each plate of the various orders
 */
function getOrder($table){
    global $db;
    $tableQuoted = $db->quote($table);
    $query = $db->query("select Ordine.id as id, tavolo,nome,categoria,quantita from Ordine join Menu M on M.id = Ordine.piattoId where tavolo=$tableQuoted");
    try {
        $response = $query ->fetchAll();
    }catch (PDOException $ex){
        die(json_encode(array('info' => $_REQUEST,'error' => array('title' => 'no such table', 'status' => 422,'detail' => 'there is no table "' . $table ))));

    }
    return $response;
}

function deleteOrder($id){
    global $db;
    $id = $db->quote($id);
    try {
        $db->query("delete from Ordine where id=$id");
    } catch (PDOException $exc){
        return false;
    }
    return true;
}

function tableOccupied(){
    global $db;
    return $db->query("select distinct tavolo from Ordine;")->fetchAll(PDO::FETCH_ASSOC);
}

function getCategories(){
    global $db;
    return $db->query("select distinct categoria from Menu;")->fetchAll(PDO::FETCH_ASSOC);
}

/** add an order to the db, receives an associative array with plate and quantity of every plate requested
 * @param $data an associative array with name and quantity of every plate requested
 * @param $table id of the table that requested this order
 */
function newOrder($data,$table){
    global $db;
    //updates the id counter, so every order has is own ID
    $id = $db->query("select MAX(id)as id from Ordine")->fetch()["id"] + 1;
    $table = $db->quote($table);
    $values ="";
    try {
        foreach ($data as $plate => $quantity){
            $plate = $db->quote($plate);
            $quantity = $db->quote($quantity);
            $values = $values . "(". $id .",". $table. ",".$plate.",".$quantity."),";
        }
        //remove the last comma
        $values= substr($values,0,strlen($values)-1);
        $db ->query("insert into Ordine values $values;");
    }catch (PDOException $ex){
        die(json_encode(array('info' => $_REQUEST,'error' => array('title' => 'generic database problem', 'status' => 422,'detail' => 'cannot insert the order in the database, check exception' ))));
    }
    return;
}

?>

