var paper;
var bukvonesisFontName = 'Devonshire';
var fontSize = 300;

function drawLetter(letter, xPos) {
    paper.text(
	xPos,
	paper.height / 2,
	letter).attr({ 'font-family' : bukvonesisFontName,
                       'font-size' : fontSize,
                       'fill' : 'black',
                       'text_anchor' : 'start'
                     });
};

function animate() {
    var xPos = paper.canvas.offsetLeft + 100;
    $.each(devonshire, function( key, value ) {
	drawLetter(key, xPos);
	xPos += 100;
    });
}

$(function(){
    paper = new Raphael("bukvonesis", 1140, 715);
    animate();
});
