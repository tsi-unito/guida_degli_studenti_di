$(document).ready(function (){
    $("#results").hide();
    $("#errMsg").hide();
    $("input[name='submit']").on("click",requestActor);
    $('#sumbitLogout').on("click",function (){
        //to logout send a request to the php file in order to close the session
        window.location.href = "/common.php?logout=true";
    });
});

function requestActor(){
    let names = "firstname=" + $(this).siblings("input[name='firstname']").val() + "&lastname=" + $(this).siblings("input[name='lastname']").val();
    $("#results").hide();
    if ($(this).parent().parent().attr("id") === "searchall"){
        $.ajax({
            url:"common.php?"+names+"&all=true",
            type:"GET",
            datatype: "json",
            success: showResults,
            error: ajaxFailed
            }
        );
    }else {
        //check if a user is searching for every movie bacon made with himself
        if (names === "firstname=Kevin&lastname=Bacon"){
            $("#errMsg").html("you're trying to search every movie kevin bacon has made with himself");
            return;
        }
        $.ajax({
                url:"common.php?"+names,
                type:"GET",
                datatype: "json",
                success: showResults,
                error: ajaxFailed
            }
        );
    }
}

function ajaxFailed(){
    $("#errMsg").html("we have problem connecting to our database");
    $("#errMsg").show();
}

function checkJsonError(json){
    //the server returned an errorCode, show the user what's the problem
    if (json.error.errCode === "noActor"){
        $("#errMsg").html("no actor named " + json.info.firstname + " " +json.info.lastname + " in our database");
    }else if (json.error.errCode === "authError"){
        $("#errMsg").html("we have problem connecting to our database");
    }else if (json.error.errCode === "noParam"){
        $("#errMsg").html("wrong parameters in the GET request");
    } else if (json.error.errCode === "noMovies"){
        $("#errMsg").html("Actor " + json.info.firstname + " " +json.info.lastname + " has made no movie with Kevin Bacon");
    }
    $("#errMsg").show();

}

function showResults(json){
    //clean the result table in case of previous query
    $("#list > tbody").empty();
    if (json.hasOwnProperty('error')) {
        checkJsonError(json);
        return;
    }

    $("#results").show();
    $("#errMsg").html("");
    $("#firstN").html(json.info.firstname);
    $("#lastN").html(json.info.lastname);

    //populate the table with the result from the server
    let i = 0;
    json.data.forEach(function(obj) {
        i++;//okay che siamo programmatori, ma la gente normale inizia a contare da 1
        $('#list > tbody').append('<tr><td>'+i+'</td><td>'+obj.name+'</td><td>'+obj.year+'</td></tr>');
    });

}
