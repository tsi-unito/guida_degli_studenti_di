<?php
// check if user is connected, else redirect
session_start();
if (!isset($_SESSION["username"])) {
    header("Location: /login.php");
    die();
}

include("top.html"); ?>

        <script src="bacon.js" type="text/javascript"></script>
    </head>
    <body>

        <?php include("banner.html");?>

            <h1>The One Degree of Kevin Bacon</h1>
            <p>Type in an actor's name to see if he/she was ever in a movie with Kevin Bacon!</p>
            <p><img id="kevinphoto" src="img/kevin_bacon.jpg" alt="Kevin Bacon"></p>

            <!-- ErrMsg: actor not in our database or actor with no films in our data base -->
            <div id="errMsg"></div>
            <!-- results: no error, results (all movies with Kevin Bacon) follow here -->
            <div id="results">
                <h2>Results for <span id="firstN"></span> <span id="lastN"></span></h2>
                <table id="list">
                    <thead>
                    <!-- i didn't like the caption......it looks cleaner without it -->
                        <tr>
                            <th>#</th>
                            <th>Name</th>
                            <th>Year</th>
                        </tr>
                    </thead>
                    <tbody>

                    </tbody>
                </table>
            </div>              

<?php include("bottom.html");?>
