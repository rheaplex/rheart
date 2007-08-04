/*  drawing.as - Drawing around a skeleton with a pen.
    Copyright (C) 2004-5  Rhea Myers rhea@myers.studio
  
    This file is part of draw-something flash.
    
    draw-something flash is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 3 of the License, or
    (at your option) any later version.
    
    draw-something flash is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.
    
    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

class drawing
{
    static var pen_distance : Number = 5.0;
    static var pen_distance_fuzz : Number = 1.5; 
    static var pen_forward_step : Number = 2.0;
    static var pen_turn_step : Number = 1.0;

    static var max_points_guard : Number = 5000;

    var skeleton : polyline;
    var pen : turtle;

    var first_point : point;
    var point_count : Number;

    function drawing (width : Number, height : Number, num_points : Number)
    {
	point_count = 0;

	make_skeleton (width, height, num_points);
	make_pen ();

	first_point = pen.position;
    }

    function make_skeleton (width : Number, height : Number, 
			    num_points : Number)
    {
	// Inset skeleton 2 * the pen distance from the edge to avoid cropping
	var inset : Number = pen_distance * 2;
	skeleton = new polyline ();
	skeleton.random_points_in_bounds (inset, inset, width - (inset * 2),
					  height - (inset * 2), num_points);
    }

    function make_pen ()
    {
	var top_left : point = skeleton.top_leftmost_point ();
	top_left.y -= pen_distance;
	pen = new turtle (top_left, pen_distance, pen_distance_fuzz,
			  pen_forward_step, pen_turn_step);
    }

    function should_finish ()
    {
	return (point_count > max_points_guard) || 
	    ((point_count > 4) &&
	     (pen.position.x - first_point.x < pen.forward_step) &&
	     (pen.position.y - first_point.y < pen.forward_step));
    }

    function next_pen_distance ()
    {
	return skeleton.distance_to_point (pen.next_point_would_be ());
    }

    function next_pen_too_far ()
    {
	return (Math.random () * pen.distance_fuzz) <
	    (pen.distance - next_pen_distance (skeleton));
    }

    function next_pen_too_close ()
    {
	return (Math.random () * pen.distance_fuzz) <
	    (next_pen_distance (skeleton) - pen.distance);
    }

    function ensure_next_pen_far_enough ()
    {
	while (next_pen_too_close (skeleton))
	    pen.left ();
    }

    function ensure_next_pen_close_enough ()
    {
	while (next_pen_too_far (skeleton))
	    pen.right ();
    }

    function adjust_next_pen ()
    {
	ensure_next_pen_far_enough (skeleton);
	ensure_next_pen_close_enough (skeleton);
    }

    function to_next_point ()
    {
	adjust_next_pen (skeleton);
	pen.forward ();
	point_count ++;
    }
}