$(function() {
  var canvas = document.getElementById('bukvonesis');
  paper.setup(canvas);

  //animateSingleLetter('n');
  animateAllLetters();
  var update = function() {
    paper.view.draw();
  };

  createjs.Ticker.setFPS(10);
  createjs.Ticker.addEventListener('tick', update);
});

function makePath(generation, offset) {
  var letterPath = new paper.CompoundPath();

  $.each(generation, function(index, coords) {
    var path = new paper.Path();

    if (coords.length == 6) {
      path.add(new paper.Point(coords[0], coords[1]));
      path.quadraticCurveTo(new paper.Point(coords[2], coords[3]),
			    new paper.Point(coords[4], coords[5]));
    } else {
      path.add(new paper.Point(coords[0], coords[1]),
	       new paper.Point(coords[2], coords[3]));
    }
    path.position.x += offset;
    letterPath.addChild(path);
  });

  letterPath.strokeColor = 'black';
  return letterPath;
}

function animate(letterPath, generations, offset) {
   $.each(letterPath.children, function(child_index, child) {
     $.each(child.segments, function(segment_index, segment) {
       var pathTween = createjs.Tween.get(segment.point, {loop:true});
       $.each(generations, function(generation_index, generation) {
	 $.each(generation, function(coords_index, coords) {
	   if (child_index == coords_index) {
	       if (coords.length == 6) {
		 if (segment_index == 0) {
		   pathTween.to({x: coords[0] + offset, y: coords[1]},
				1000, createjs.Ease.quadOut);
		   } else {
		   pathTween.to({x: coords[4] + offset, y: coords[5]},
				1000, createjs.Ease.quadOut);
		   }
	       } else {
		 pathTween.to({x: coords[segment_index + 0] + offset, y: coords[segment_index + 1]},
			      1000, createjs.Ease.quadOut);
	       }
	     }
	 });
       });
       pathTween.wait(1500);
     });
   });
}

function animateSingleLetter(letter) {
  var generations = devonshire[letter];
  var letterPath = makePath(generations[0], 0);
  animate(letterPath, generations.slice(1));
}

function animateAllLetters() {
  var offset = 0;

  $.each(devonshire, function(letter, generations) {
     var letterPath = makePath(generations[0], offset);
     animate(letterPath, generations.slice(1), offset);
     offset += 300;
   });
}
