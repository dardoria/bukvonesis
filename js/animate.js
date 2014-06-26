var scale = 0.12;
var xScale = scale;
var yScale = -1 * scale;
var xOffset = 600 * scale;
var yOffset = 1000 * scale;
var delay = 1000;


$(function() {
  var canvas = document.getElementById('bukvonesis');
  paper.setup(canvas);
  //animateSingleLetter('a');
  animateAllLetters();

  var update = function() {
    paper.view.draw();
  };

  createjs.Ticker.setFPS(20);
  createjs.Ticker.addEventListener('tick', update);
});

function makePath(generation, x_offset, y_offset) {
  var letterPath = new paper.CompoundPath();

  $.each(generation, function(index, coords) {
    var path = new paper.Path();

    if (coords.length == 6) {
      path.add(new paper.Point(scaleX(coords[0]) + x_offset, scaleY(coords[1]) + y_offset));
      path.quadraticCurveTo(new paper.Point(scaleX(coords[2]) + x_offset, scaleY(coords[3]) + y_offset),
			    new paper.Point(scaleX(coords[4]) + x_offset, scaleY(coords[5]) + y_offset));
    } else {
      path.add(new paper.Point(scaleX(coords[0]) + x_offset, scaleY(coords[1]) + y_offset),
	       new paper.Point(scaleX(coords[2]) + x_offset), scaleY(coords[3]) + y_offset);
    }
    letterPath.addChild(path);
  });

  letterPath.strokeColor = 'black';

  return letterPath;
}

function animate(letterPath, generations, x_offset, y_offset) {
  var children_length = letterPath.children.length;

  for (var child_index = 0; child_index < children_length; child_index++) {
    var child = letterPath.children[child_index];

    var segments_length = child.segments.length;
    for (var segment_index = 0; segment_index < segments_length; segment_index++) {
      var segment = child.segments[segment_index];
      var pathTween = createjs.Tween.get(segment.point, {loop:true});

      var generations_length = generations.length;
      for (var generation_index = 0; generation_index < generations_length; generation_index++) {

        var generation = generations[generation_index];
        var generation_length = generation.length;

        var coords = generation[child_index];

	if (coords.length == 6) { //curve
	  if (segment_index == 0) {
	    pathTween.to({ x: scaleX(coords[0]) + x_offset,
			   y: scaleY(coords[1]) + y_offset},
			 delay, createjs.Ease.quadOut);
	  } else {
	    pathTween.to({x: scaleX(coords[4]) + x_offset,
			  y: scaleY(coords[5]) + y_offset},
			 delay, createjs.Ease.quadOut);
	  }
	} else { //straight line
	  if (segment_index == 0) {
	    pathTween.to({x: scaleX(coords[0]) + x_offset,
			  y: scaleY(coords[1]) + y_offset},
			 delay, createjs.Ease.quadOut);
	  } else {
	    pathTween.to({x: scaleX(coords[2]) + x_offset,
			  y: scaleY(coords[3]) + y_offset},
			 delay, createjs.Ease.quadOut);
	  }
	}
      }
      pathTween.wait(3500);
    }
  }
}

function animateSingleLetter(letter) {
  var generations = devonshire[letter];
  var letterPath = makePath(generations[0], 0);
  animate(letterPath, generations, 0);
  return letterPath;
}

function animateAllLetters() {
  var x_offset = 0;
  var y_offset = 0;
  var letter_index = 0;
  var letters_per_row = 7;
  var row = 1;
  var y_space = 90;

  $.each(devonshire, function(letter, generations) {
    y_offset = yOffset * row;
    var letterPath = makePath(generations.slice(-1)[0], x_offset, y_offset);
    animate(letterPath, generations.slice(1), x_offset, y_offset);
    x_offset += xOffset;
    letter_index++;
    if (letter_index % letters_per_row == 0) {
      row++;
      x_offset = 0;
    }
   });
}

function scaleX(coords) {
  return coords * xScale;
}

function scaleY(coords) {
  return coords * yScale;
}
