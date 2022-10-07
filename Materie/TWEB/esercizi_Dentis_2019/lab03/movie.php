<!-- Lorenzo Dentis, matricola 914833, Reti e Sistemi Informatici
php document of the Rancid tomatoe's page
Esercitazione 3: creare i file per un finto sito di recensioni cinematografiche chiamato Rancid Tomatoes con la recensione di diversi film, le cui informazioni sono salvate in file di testo.-->
<?php
$movie=$_GET['film'];
$info=file($movie . '/info.txt');

/* a scraper,giving the movie title return an array of every review stored in the folder of said movie */
function scrapeReviews ($title){
    $i=0;
    $reviewFiles = glob($title . "/review*.txt");
    foreach ($reviewFiles as $reviewFile) {
        $reviewArray[$i] =  file($reviewFile);
        $i++;
    }
    return $reviewArray;
}
?>
<!DOCTYPE html>
<html lang="en">
<head>
    <title>Rancid Tomatoes</title>

    <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
    <link href="movie.css" type="text/css" rel="stylesheet">
    <link rel="icon" href="https://courses.cs.washington.edu/courses/cse190m/11sp/homework/2/rotten.gif">
</head>

<body>
<div id="banner">
    <img src="http://www.cs.washington.edu/education/courses/cse190m/11sp/homework/2/banner.png" alt="Rancid Tomatoes">
</div>
<h1><?=$info[0] . ' (' . trim($info[1]).')' //movie title and release year ?></h1>
<div id="mainBox">
    <div id="infoColumn">
        <img src="<?= $movie . '/overview.png' ?>" alt="general overview">

        <dl id="info">
            <?php /*given the overview file prints every definition with the following format DEFINITION:DESCRIPTION */
            $overview = file( $movie . '/overview.txt');
            foreach ($overview as $line){
                $definition= explode(":",$line);
                ?>
                <dt><?= $definition[0] ?></dt>
                <dd><?= $definition[1] ?></dd>
                <?php
            }
            ?>
        </dl>
    </div>
    <div id="evaluation">
        <?php
        /*check score of the movie (3rd row of info.txt file) and change the source of the image accordngly*/
        $rottenImg = intval($info[2])>=60 ? $rottenImg='freshbig.png' : $rottenImg='rottenbig.png';
        ?>
        <img src="http://www.cs.washington.edu/education/courses/cse190m/11sp/homework/2/<?= $rottenImg ?>" alt="Rotten">
        <?= $info[2] . '%' ?>
    </div>
    <div id="allReviews">
        <div id="firstReviews">
            <?php
            /*the following code is almost the same for the 2 column (div id=firstReviews and id=secondReview)
            cycles trough the reviews, for a maximum of five of them. for every reviews modifies the image accordingly, then prints the review body and the reviewer's information */
            $reviews = scrapeReviews($movie);
            $reviewsNumber= count($reviews) > 10 ? 10 : count($reviews);
            for ($i=0 ; $i < 5 && $i < $reviewsNumber/2; $i++){
                /*img substitution*/
                $reviews[$i][1]=str_replace("\n","",$reviews[$i][1]);
                $imageReview= $reviews[$i][1] == 'ROTTEN' ? 'rotten.gif' : 'fresh.gif';
                ?>
                <div class="reviewBox">
                    <p class="review">
                        <img src="http://www.cs.washington.edu/education/courses/cse190m/11sp/homework/2/<?= $imageReview ?>" alt="Rotten">
                        <q><?= $reviews[$i][0] ?></q>
                    </p>
                    <p class="reviewer">
                        <img src="http://www.cs.washington.edu/education/courses/cse190m/11sp/homework/2/critic.gif" alt="Critic">
                        <?= $reviews[$i][2] ?> <br>
                        <span><?= $reviews[$i][3] ?></span>
                    </p>
                </div>
            <?php }
            ?>
        </div>

        <div id="secondReviews">
            <?php /*same cycle as firstReview, but cycling from the half to the last review (or the tenth)*/
            $reviews = scrapeReviews($movie);
            for ($i=ceil($reviewsNumber/2) ; $i < 10 && $i < count($reviews); $i++){
                $reviews[$i][1]=str_replace("\n","",$reviews[$i][1]);
                $imageReview= $reviews[$i][1] == 'ROTTEN' ? 'rotten.gif' : 'fresh.gif';
                ?>
                <div class="reviewBox">
                    <p class="review">
                        <img src="http://www.cs.washington.edu/education/courses/cse190m/11sp/homework/2/<?= $imageReview ?>" alt="Rotten">
                        <q><?= $reviews[$i][0] ?></q>
                    </p>
                    <p class="reviewer">
                        <img src="http://www.cs.washington.edu/education/courses/cse190m/11sp/homework/2/critic.gif" alt="Critic">
                        <?= $reviews[$i][2] ?> <br>
                        <span><?= $reviews[$i][3] ?></span>
                    </p>
                </div>
            <?php }
            ?>
        </div>
    </div>
    <p id="footer">(1-<?= $i ?>) of <?=count($reviews)  //updates the number of reviews according to the actual number?> </p>
</div>
<div id="validators">
    <a href="http://validator.w3.org/check/referer">
        <img width="88" src=
        "https://upload.wikimedia.org/wikipedia/commons/b/bb/W3C_HTML5_certified.png "
             alt="Valid HTML5!">
    </a> <br>
    <a href="http://jigsaw.w3.org/css-validator/check/referer"><img src="http://jigsaw.w3.org/css-validator/images/vcss" alt="Valid CSS!"></a>
</div>
</body>
</html>
