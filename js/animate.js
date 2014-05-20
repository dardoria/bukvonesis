$(function() {
  var canvas = document.getElementById('bukvonesis');
  paper.setup(canvas);

  var offset = 0;

  $.each(devonshire, function(letter, generations) {
    var letterPath = makePath(generations[0], offset);
    animate(letterPath, generations.slice(1));
    offset += 300;
  });

  paper.view.draw();
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

function animate(letterPath, generations) {
   $.each(letterPath.children, function(index, child) {
     createjs.Tween.get(child, {loop:true})
	 .to({position:{x:0}});
   });
}
