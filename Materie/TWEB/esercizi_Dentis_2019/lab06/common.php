<?php
//check if the user is connected
header("Content-type:application/json");
session_start();
if (!isset($_SESSION["username"])) {
    header("Location: /login.php");
    die();
}
//response to the press of the logout button, clean session and redirect
if (isset($_REQUEST['logout'])){
    session_unset();
    session_destroy();
    header("Location: /login.php");
    die();
}

//first connect to the db
try {
    $db = new PDO ("mysql:dbname=film;host=127.0.0.1", "scuola", "scuola");
    $db->setAttribute(PDO::ATTR_ERRMODE, PDO::ERRMODE_EXCEPTION);
} catch (PDOException $ex) {
    die(json_encode(array('info' => array('firstname' => $_REQUEST["firstname"], 'lastname' => $_REQUEST["lastname"]),'error' => array('message' => 'database auth error','exception' => $ex->getMessage()))));
}
if (!isset($_REQUEST["firstname"]) || !isset($_REQUEST["lastname"]))
    die(json_encode(array('info' => array('firstname' => $_REQUEST["firstname"], 'lastname' => $_REQUEST["lastname"]),'error' => array('message' => 'incorrect data received from client','errCode' => 'noParam'))));
//then quote the parameter and find the id of the searched actor (need's to be done anyway)
$firstname = $db->quote($_REQUEST["firstname"]. "%");
$lastname = $db->quote($_REQUEST["lastname"]);
$id = findActorId($firstname,$lastname);

//is the user searching for every movie or just the ones with Bacon?
if (isset($_REQUEST["all"]) && $_REQUEST["all"] == "true"){
    findAll($id);
}else{
    findBacon($id);
}


function findActorId($firstname, $lastname){
    global $db;
    /*example query using Will Smith
     * select * from actors join roles on actor_id = actors.id where last_name = "Smith" AND first_name LIKE "Will%"
     order by actors.film_count, id, CASE
        when first_name like "Will" then 1
        when first_name like "will%" then 2
        else 3
    end;      */
    $query = $db->query("select distinct id from actors join roles on actor_id = actors.id where last_name = $lastname AND first_name LIKE $firstname order by film_count,id");

    //no actor found
    if ($query->rowCount() == 0) {
        die(json_encode(array('info' => array('firstname' => $_REQUEST["firstname"], 'lastname' => $_REQUEST["lastname"]),'error' => array('message' => 'no such actor in the database','errCode' => 'noActor'))));
    }
    return $query->fetch()["id"];
}

//using the id this function returns every movie the actor with that id has made
function findAll($id)
{
    global $db;
    $query = $db->query("select movies.name, movies.year from roles join movies on movie_id = movies.id where actor_id = $id");
    echo json_encode(array('info' => array('firstname' => $_REQUEST["firstname"], 'lastname' => $_REQUEST["lastname"]), 'data' => $query->fetchAll()));
}

//using the id this function returns every movie the actor with that id has made with Kevin Bacon
function findBacon($id){
    global $db;
    /*
    why shouldn't I hardcode Kevin Bacon's id? It will never change as long as I use this database because it is primary key
    anyway, if the hardcoded id is not a nice solution i can still get it using the function i wrote before
    $baconId = findActorId('"Kevin"','"Bacon"');
    */

    $query = $db->query("select m.name,m.year from roles as a join roles as b on a.movie_id=b.movie_id 
            join movies m on m.id = a.movie_id
            where a.actor_id = $id and b.actor_id=22591;");
    if ($query->rowCount() == 0) {
        die(json_encode(array('info' => array('firstname' => $_REQUEST["firstname"], 'lastname' => $_REQUEST["lastname"]), 'error' => array('message' => 'Actor has made no movies with kevin bacon','errCode' => 'noMovies'))));
    }
    echo json_encode(array('info' => array('firstname' => $_REQUEST["firstname"], 'lastname' => $_REQUEST["lastname"]), 'data' => $query->fetchAll()));
}
?>



