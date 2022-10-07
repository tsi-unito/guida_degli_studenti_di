<?php include "top.html";
/* copying the file in memory to have a faster and easy access to data */
/* data follow this structure: name, sex, age, personality, OS, minAge, maxAge*/
$file = file("singles.txt",FILE_IGNORE_NEW_LINES);
$i=0;
$userId=-1;
foreach ($file as $line){
    $userData[$i]=explode(",",$line);
    if (strcmp($userData[$i][0], $_GET['name'])==0){
        $userId=$i;
    }
    $i++;
}
/* function to check if a given user is a suitable match for the user searching*/
function checkMatch ($user, $match){
    if ($user[0]==$match[0]) //user name equal match name
        return false;
    if ($user[1]==$match[1]) //user gender equal to match gender
        return false;
    if ($match[2] < $user[5] || $match[2] >$user[6]) //check match age
        return false;
    if ($user[4] != $match[4])//check if they use the same OS
        return false;
    for ($i=0;$i<4;$i++) //personality check
        if ($match[3][$i] == $user[3][$i])
            return true;
    return false;
}
?>
<p>
    <strong>Matches for <?= $_GET['name'] ?></strong>
</p>

<?php
    /* for each match of the  searching user a new "div match" is created and the characteristic are shown */
    foreach ($userData as $match){
        if (!checkMatch($userData[$userId],$match))
            continue;
?>
    <div class="match">
        <p>
            <img src="http://www.cs.washington.edu/education/courses/cse190m/12sp/homework/4/user.jpg" alt="user image">
            <?= $match[0]?>
            <ul>
                <li>
                    <strong>gender:</strong>
                </li>
                <li> <?= $match[1]?></li>
                <li>
                    <strong>age:</strong>
                </li>
                <li> <?= $match[2]?></li>
                <li>
                    <strong>type:</strong>
                </li>
                <li> <?= $match[3]?></li>
                <li>
                    <strong>OS:</strong>
                </li>
                <li> <?= $match[4]?></li>
            </ul>
        </p>

    </div>
<?php
    }
?>


<?php include "bottom.html"; ?>