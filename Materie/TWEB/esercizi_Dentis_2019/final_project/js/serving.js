var order, isNewOrder;
$(document).ready(function () {
    //navbar loading, look for every food category
    $.ajax({
            url: "staffRequest.php?",
            type: "POST",
            data: {
                "action": "categories"
            },
            dataType: "json",
            success: navbarload,
            error: ajaxFailed
        });
    //behaviour of the delete button
    $("#delete").on("click", function () {
        $("#order").empty();
        order = [];
    });
    //behaviour of the send button
    $("#send").on("click", function () {
        $.ajax({
                url: "staffRequest.php?",
                type: "POST",
                data: {
                    "action": "newOrder",
                    "order": order,
                    "table": $("#tableNum").html()
                },
                dataType: "json",
                success: orderConfirmed,
                error: ajaxFailed
            }
        )
    });
    //behaviour of the logout button
    $("#log_in_out").on("click", function () {
        $.ajax({
                url: "login.php?",
                type: "POST",
                data: {
                    "login": "logout"
                },
                dataType: "json",
                success: logoutResponse,
                error: ajaxFailed
            }
        )});
        order = [];
        isNewOrder = false
        $(".orderAction").hide();
        //show the list of tables empty and occupied
        checkTables();
        setInterval(updateTables,10000);
        $("#orderDiv > h1").hide();
});


function logoutResponse(json){
    if (json.hasOwnProperty("error")) {
        console.log(json.error); //this is a debug case, if json returns an error something strange happened
        return;
    }

    window.location.href = "index.php";
}

function jsonError(json) {
    console.log(json)
    $("#userMessage").attr("class","error");
    $("#userMessage").html(json.error.detail);
    setTimeout(function (){$("#userMessage").fadeOut()},10000);
    $(".orderAction").fadeOut();
}

function orderConfirmed(json) {
    if (json.hasOwnProperty("error")) {
        jsonError(json);
        return;
    }
    $("#userMessage").attr("class","message");
    $("#userMessage").html("ordine inviato con successo");
    setTimeout(function (){$("#userMessage").fadeOut()},10000);
    $(".orderAction").fadeOut();
    checkTables();
}
/** function checkTables creates the left column with the list of tables, split in free and occupied
 */
function  checkTables() {
    $("#freeTable").empty();
    $("#occupiedTable").empty();
    $.ajax({
            url: "staffRequest.php?",
            type: "POST",
            data: {
                "action" : "tables"
            },
            dataType : "json",
            success : function (json) {
                if (json.hasOwnProperty("error")) {
                    jsonError(json);
                    return;
                }
                let occupied=[];
                //save the json response in a more useful array
                for (let occupiedTable of json.response.occupied) {
                    occupied.push(occupiedTable.tavolo);
                }
                for (let i = 1; i <= json.response.total ; i++) {
                    let table = $("<li id='table"+ i +"'>Tavolo "+ i +"</li>");
                    if (occupied.includes(String(i))){
                        //add it as an occupied Table
                        $("#occupiedTable").append(table);
                        //show table order on click
                        table.on("click",function (){
                            $("#orderDiv > h1").show();
                            $(".orderAction").fadeOut();
                            isNewOrder = false;
                            $.ajax({
                                    url: "staffRequest.php?",
                                    type: "POST",
                                    data: {
                                        "action" : "order",
                                        "table" : i
                                    },
                                    dataType : "json",
                                    success : loadOrder,
                                    error : ajaxFailed
                                }
                            )});
                    }else{
                        //add it as a free Table
                        $("#freeTable").append(table);
                        //clean the order and get ready for the new table
                        table.on("click",function (){
                            isNewOrder = true;
                            $("#orderDiv > h1").show();
                            $(".orderAction").fadeIn()
                            $("#order").empty();
                            order=[];
                            //set table number
                            $("#tableNum").html(this.id.substring(5));
                        });
                    }
                }
            },
            error : ajaxFailed
        }
    );
}

/**
 * when a table is selected and its order returned show the order list
 * @param json the response from tge AJAX Request
 */
function loadOrder(json){
    $("#order").empty();
    $(".orderAction").on("click",function (){});
    if (json.hasOwnProperty("error")) {
        jsonError(json);
        return;
    }
    $("#tableNum").html(json.info.table)
    for (let response of json.response) {
        $("#order").append("<li>"+ response.quantita + " " + response.categoria + " " + response.nome +"</li>")
    }
}

/**
 * receives every category of food present in the DB, and load it in the navbar
 * @param json the response from tge AJAX Request
 */
function navbarload(json){
    var categoriesNum = 0;
    var navlist = $("#logo");
    for (let jsonElement of json.response) {
        let newCategory = $("<li class='dropdown'>" + jsonElement.categoria  +"<div class='dropdown-content' id="+jsonElement.categoria+"></div></li>");
        newCategory.on("mouseover",openCategory);
        categoriesNum++;
        navlist.after(newCategory.clone(true));//menu categories in navbar
    }
}

function ajaxFailed() {
    console.log("AJAX FAILED!");
    $("#userMessage").attr("class","error");
    $("#userMessage").html("unknown error, contact website administrator");
    setTimeout(function (){$("#userMessage").fadeOut()},10000);
    $(".orderAction").fadeOut();}

function updateTables() {
    
}

/**
 * when a category in the navbar is clicked eventually load it's children and show them
 */
function openCategory(){
    //get the text from the element clicked without his possible children
    var category = String(this.childNodes[0].nodeValue);
    if ($("#"+category).children().length === 0) {
        $(document).ready(function () {
            $.ajax({
                    url: "staffRequest.php?",
                    type: "POST",
                    data: {
                        "action": "menu",
                        "category": category
                    },
                    dataType: "json",
                    success: populateCategory,
                    error: ajaxFailed
                }
            )
        });
    }//don't do useless ajax call if not needed, data are already loaded, but hidden
}

/**
 *  parsing and creation of the category, the request of this category is made in function openCategory
 * @param json the response from tge AJAX Request
 */
function populateCategory(json){
    if (json.hasOwnProperty("error")) {
        jsonError(json);
        return;
    }    //console.log(json);
    var dropMenu = $("#" + json.info.category);
    for (let plate of json.response) {
        let newPlate =$("<a id="+ plate.id +">"+ plate.nome +"</a>");
        newPlate.on("click",addToOrder);
        dropMenu.append(newPlate);
    }
    //fix position, i couldn't find a way without using absolute and JS
    dropMenu.css({top:49 , left:dropMenu.parent().position().left})
}


/**
 * sends the order to the server to be added in the DB, then clean the section dedicated to the order
 */
function addToOrder(){
    if (!isNewOrder){
        $("#userMessage").attr("class","error");
        $("#userMessage").html("selezionare un tavolo prima di inserire piatti nell'ordine");
        setTimeout(function (){$("#userMessage").fadeOut()},10000);
        return;
    }

    //the global variable order is a dictionary having the id of the plate as a key and the quantity as value
    order[this.id]= order.hasOwnProperty(this.id) ? order[this.id] + 1 : 1;
    //emptying the list every time means that the program doesn't have to look in the dom for the quantity
    $("#order").empty();
    for([id, quantity] of Object.entries(order)) {
        //let category = $
        $("#order").append("<li>"+ quantity+ " "  + $("#"+id).html() +"</li>");
    }
}