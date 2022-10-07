$(document).ready( function () {
    $('#signUpBox').hide();
    $("#log_in_out").on("click",function () {
        $('#signUpBox').fadeIn();
        $('input[name="submitSign"]').on("click",register);
    })
    $('input[name="submitLogin"]').on("click",login);
});

function register() {
    //client side controls
    for (let field of $(this).siblings("ul").children().children()) {
        if (field.value === "") {
            $(".error:last").html("Please compile every field");
            return;
        }
    }

    $.ajax({
        url: "login.php?",
        type: "POST",
        data: {
            "login" : "new",
            "username" : $("li>input[name='username']").val(),
            "password" : $("li>input[name='password']").val(),
            "name" : $("li>input[name='name']").val(),
            "role" : $("li>input[name='role']:checked").val()
        },
        dataType : "json",
        success : handleResponse,
        error : ajaxFailed
        }

    )

}

function login(){
    for (let sibling of $(this).siblings()) {
        if (sibling.value === "") {
            $(".error:first").html("Please compile every field");
            return
        }
    }
        $.ajax({
            url: "login.php?",
            type: "POST",
            data: {
                "login" : "existing",
                "username" : $("#loginBox>input[name='username']").val(),
                "password" : $("#loginBox>input[name='password']").val(),
            },
            dataType : "json",
            success : handleResponse,
            error : ajaxFailed
        }

    )
}

function ajaxFailed(xhr, ajaxOptions, thrownError) {
    console.log("error" + xhr.responseJSON);
    error(xhr.responseJSON);
}

function error(json){
    var errorP;
    if (json.info.login === "new" )
        errorP = $(".error:last");
    else
        errorP = $(".error:first");

    errorP.html(json.error.detail)

}

function handleResponse(json){
    if (json.hasOwnProperty("error")) {
        error(json);
        return;
    }

    window.location.href = json.response.role === "cook" ? "kitchen.php" : "serving.php";
}


