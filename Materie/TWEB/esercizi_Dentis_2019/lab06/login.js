$(document).ready( function () {
    $('input[name="submit"]').on('click', sendData);
    $('#sumbitLogout').hide();
});

//send username and password to the server
function sendData(){
    $.ajax({
        url:"login.php?",
        type:"POST", //i'm never, ever sending password as a get request
        data:{
            "username" : $("input[name='username']").val(),
            "password" : $("input[name='password']").val()
        },
        datatype: "json",
        success: handleResponse,
        error: ajaxFailed
    })
}

//according to the server response grant access or show an error
function handleResponse(json){
    if (json.hasOwnProperty('error')){
        console.log(json.error.message);
        if (json.error.errCode === 'wrongCredentials')
            $("#errMsg").html("wrong username or password");
        else
            $("#errMsg").html("we have problem connecting to our database");
        $("#errMsg").show();
        return;
    }
    if (json.sessionInfo.session === true){
        window.location.replace("/index.php");
    }
}

function ajaxFailed(){
    $("#errMsg").html("we have problem connecting to our database");
    $("#errMsg").show();
}