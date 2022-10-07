$(document).ready( function (){
    $("#log_in_out").on("click",function (){
        $.ajax({
                url: "login.php?",
                type: "POST",
                data: {
                    "login" : "logout"
                },
                dataType : "json",
                success : logoutResponse,
                error : ajaxFailed
            }

        )
    })
    checkOrders();
    setInterval(checkOrders,1000);
});

function ajaxFailed(){
    function ajaxFailed(xhr, ajaxOptions, thrownError) {
        console.log("error" + xhr.responseJSON);
    }
}
function logoutResponse(json){
    if (json.hasOwnProperty("error")) {
        console.log(json.error); //this is a debug case, if json returns an error something strange happened
        return;
    }

    window.location.href = "index.php";
}

/**
 * send an AJAX request to get every unclompleted irder in the db
 */
function checkOrders(){
    $.ajax({
            url: "staffRequest.php?",
            type: "POST",
            data: {
                "action" : "orders"
            },
            dataType : "json",
            success : handleResponse,
            error : ajaxFailed
        }

    );
}

function handleResponse(json){
    if (json.hasOwnProperty("error")){
        console.log(json.error);
        return;
    }
    if (json.info.action === "delete"){
        $("#" + json.info.id).remove();
    }else
    //refresh order list
    for (let key in json.response) {
        newOrder(json.response[key]);
    }
}
function newOrder(data){
    //check all orders inside #orders, if there is an id not present in the page show the new order
    for (let order of $("#orders").children('div')) {
        if (order.id == data[0].id){
            return;
        }
    }
    $("#orders").append("<div class='order' id=" + data[0].id +">" +
        "<h1>Tavolo: " + data[0].tavolo + "</h1>" +
        "<table><thead><th>Categoria</th><th>Piatto</th><th>Quantità</th></thead><tbody></tbody></table> " +
        "<button>Completato <img src='img/tick.jpeg' alt='tick'></button>" +
        "</div>")

    for (let datum of data) {
        $("#"+datum.id + " > table > tbody:last-child").append("<tr><td>" +datum.categoria+ "</td><td>"+datum.nome+"</td><td>"+datum.quantita+"</td></tr>");
    }
    $("#"+data[0].id + " > button").on("click",function (){
        $.ajax({
                url: "staffRequest.php?",
                type: "POST",
                data: {
                    "action" : "delete",
                    "id" : data[0].id
                },
                dataType : "json",
                success : handleResponse,
                error : ajaxFailed
            }

        )
    });
}
