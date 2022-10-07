window.onload = () => {
    freeTile = [3,3];
    dispose();
    setMovable();
    document.getElementById("shufflebutton").onclick = shuffle;
};

let freeTile;
let movable = []; //makes a lot easier to change css and check if those tiles can be moved

function shuffle(){
    let cicles = Math.random() * 100, tileIndex;
    //single moves, random
    for (let i = 0; i <cicles ; i++) {
        tileIndex = parseInt(Math.random() * movable.length);
        move.call(movable[tileIndex],null,true);
    }
}

/**
 * set an array of movable tiles, containing the tiles around the blank space
 */
function setMovable(){ //looks bad....but it's fast and makes everything else safe and easy
    for (let i = 0; i <movable.length ; i++) {
        movable[i].className="";
    }
    movable = [];
    let i = 0;
    let tile = document.getElementById("tile_" + (freeTile[0] + 1) + "_" + freeTile[1]);
    if (tile){
        movable[i] = tile;
        tile.className = "movable"
        i++;
    }
    tile = document.getElementById("tile_" + (freeTile[0] - 1) + "_" + freeTile[1]);
    if (tile){
        movable[i] = tile;
        tile.className = "movable"
        i++;
    }
    tile = document.getElementById("tile_" + freeTile[0] + "_" +  (freeTile[1] + 1));
    if (tile){
        movable[i] = tile;
        tile.className = "movable"
        i++;
    }
    tile = document.getElementById("tile_" + freeTile[0] + "_" +  (freeTile[1] - 1));
    if (tile){
        movable[i] = tile;
        tile.className = "movable"
        i++;
    }
}

function dispose(){
    let pieces=document.getElementById("puzzlearea").children;
    let positionColumn=0;
    let positionRow=0;
    //for every tiles set the id, the position and the background. then link the onlick handler
    for (let piece of pieces) {
        if (positionColumn > 3) {
            positionColumn = 0;
            positionRow++;
        }
        piece.style.left = (100 * positionColumn) + "px";
        piece.style.top = (100 * positionRow) + "px";
        piece.id = "tile_" + positionRow + "_" + positionColumn;
        piece.style.backgroundImage = "url('backgroundScaled.png')";
        piece.style.backgroundPosition = (-100 * positionColumn) + "px " + (-100 * positionRow) + "px";
        piece.onclick = move;

        positionColumn++;

    }
}

/** called on a tiles check if it's movable.
 * if so moves it in the only possible place, then reset the movable tiles
 * last but not least checks if the tiles are in order calling the function checkVictory
 */

function move(mouseEvent,isShuffling = false){
    let positionString=this.id.split("_");
    let pos = [parseInt(positionString[1]), parseInt(positionString[2])];
    let displacement = [freeTile[0]-pos[0],freeTile[1]-pos[1]];
    if(movable.includes(this)){
        if (Math.abs(displacement[0]) === 1){//move on y
            pos[0] += displacement[0];
            freeTile[0] -= displacement[0];
            this.style.left = (100 * pos[1]) + "px";
            this.style.top = (100 * pos[0]) + "px";
            this.id = "tile_" + pos[0] + "_" + pos[1];
        } else { //move on x
            pos[1] += displacement[1];
            freeTile[1] -= displacement[1];
            this.style.left = (100 * pos[1]) + "px";
            this.style.top = (100 * pos[0]) + "px";
            this.id = "tile_" + pos[0] + "_" + pos[1];
        }
    }
    setMovable();
    //don't check for victory if the tiles are just shuffling
    if (!isShuffling)
        checkVictory();
}
// every move checks if the player has the tiles in the correct order, and celebrate the victory
function checkVictory(){
    let pieces=document.getElementById("puzzlearea").children;
    let positionColumn=0;
    let positionRow=0;
    let id = "";
    for (let piece of pieces) {
        if (positionColumn > 3) {
            positionColumn = 0;
            positionRow++;
        }
        id = "tile_" + positionRow + "_" + positionColumn;
        if (piece.id !== id)
            return null;
        positionColumn++;
    }

    paragraph = document.createElement("p");
    paragraph.innerText = "Congratulazioni hai vinto";
    paragraph.id = "winner"
    document.getElementsByTagName("body")[0].insertBefore(paragraph,document.getElementById("w3c"));

    setTimeout(function(){
        window.location.replace("https://www.youtube.com/watch?v=dQw4w9WgXcQ");
    }, 5000);
}