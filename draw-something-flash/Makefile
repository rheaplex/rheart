all: draw_something.swf

draw_something.swf: point.as polyline.as turtle.as drawing.as draw_something.as
	mtasc -swf draw_something.swf -main -header 600:600:30 \
		point.as \
		polyline.as \
		turtle.as \
		drawing.as \
		draw_something.as
