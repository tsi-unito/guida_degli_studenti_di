<?php include "top.html";
// get the data from the post request and write it on file
$userRow =implode(",",$_POST) . "\n";
file_put_contents("singles.txt",$userRow,FILE_APPEND);?>
<p>
    <strong>Thank you!</strong>
</p>
<p>
    Welcome to NerdLuv, <?= $_POST['name'] //then show the name?>
</p>
<p>
    Now <a href="matches.php">log in to see
        your matches!</a>
</p>
<?php include "bottom.html"; ?>