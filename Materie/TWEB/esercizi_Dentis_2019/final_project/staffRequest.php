<?php
$tableNumber=20;
include ("database.php");
if (!isset($_SESSION["UID"])){
    die(json_encode(array('info' => $_REQUEST,'error' => array('title' => 'Unauthorized', 'status' => 401,'detail' => 'not logged in'))));
    //is header("Location: /index.php"); better?
}
/*this cascade of ifs check witch actions does the client wants to be done, then makes sure all the parameters are presents
ultimately call the right function in database.php */
if($_REQUEST["action"] == "orders"){
    $orders = getOrders();
    die(json_encode(array('info' => $_REQUEST,'response' => $orders)));

}else if ($_REQUEST["action"] == "delete"){
    $id = $_REQUEST["id"];
    if (isset($id)) {
        if (deleteOrder($id))
            die(json_encode(array('info' => $_REQUEST, 'response' => array('message' => 'order deleted', 'id' => $id))));
    }
    die(json_encode(array('info' => $_REQUEST,'error' => array('title' => 'no data found', 'status' => 404,'detail' => 'this order doesn\'t exist'))));
}else if($_REQUEST["action"] == "categories"){
    die(json_encode(array('info' => $_REQUEST, 'response' => getCategories())));

}else if ($_REQUEST["action"] == "menu") {
    if (!isset($_REQUEST["category"]))
        die(json_encode(array('info' => $_REQUEST,'error' => array('title' => 'Missing parameter', 'status' => 422,'detail' => 'Missing the category'))));
    die(json_encode(array('info' => $_REQUEST, 'response' => getMenu($_REQUEST["category"]))));

}else if ($_REQUEST["action"] == "tables") {
    die(json_encode(array('info' => $_REQUEST, 'response' => array('total'=> $tableNumber, 'occupied'=>tableOccupied()))));

}else if ($_REQUEST["action"] == "order") {
    if (!isset($_REQUEST["table"]))
        die(json_encode(array('info' => $_REQUEST, 'error' => array('title' => 'Missing parameter', 'status' => 422, 'detail' => 'Missing the table'))));
    die(json_encode(array('info' => $_REQUEST, 'response' => getOrder($_REQUEST["table"]))));

}else if ($_REQUEST["action"] == "newOrder") {
    if (!isset($_REQUEST["order"]) || !isset($_REQUEST["table"]))
        die(json_encode(array('info' => $_REQUEST,'error' => array('title' => 'Missing parameter', 'status' => 422,'detail' => 'Missing the data'))));
    $order = [];
    //AJAX auto-fills missing entry in the array, but i'm sending a dictionary, not an array. i have to filter the data sent
    foreach ($_REQUEST["order"] as $plate => $quantity){
        if ($quantity != "")
            $order[$plate] = $quantity;
    }
    die(json_encode(array('info' => $_REQUEST, 'response' => newOrder($order,$_REQUEST["table"]))));

} else{
    die(json_encode(array('info' => $_REQUEST,'error' => array('title' => 'Unknow action', 'status' => 400,'detail' => 'Unknown action specificated in action field'))));

}?>
