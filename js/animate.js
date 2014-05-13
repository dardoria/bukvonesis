$(function() {
    var canvas = document.getElementById('bukvonesis');
    paper.setup(canvas);

    var paths = [];
    var offset = 0;

    //$.each(devonshire, function(key, trials) {
	var key = 'b';
	paths[key] = [];
	var trials = devonshire[key];
	//$.each(trials, function(trial, value) {

	    var trial = trials.length-1;
	    var value = trials[trial];
	    paths[key][trial] = [];

	    $.each(value, function(index, coords) {
		var path = new paper.Path();
		paths[key][trial].push(path);
		path.strokeColor = new paper.Color(1/(index+1), 0, 0);

		if (coords.length == 6) {
		    path.add(new paper.Point(coords[0], coords[1]));
		    path.quadraticCurveTo(new paper.Point(coords[2], coords[3]),
					  new paper.Point(coords[4], coords[5]));
		} else {
		    path.add(new paper.Point(coords[0], coords[1]),
			     new paper.Point(coords[2], coords[3]));
		}
		path.position.x += offset;
	    });

	//});
	offset += 300;
    //});

    paper.view.draw();
    console.log(paths);
});
