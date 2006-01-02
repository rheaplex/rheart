/*  polyline.as - A polyline.
    Copyright (C) 2004-5  Rhea Myers rhea@myers.studio
  
    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.
  
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

class polyline 
{
    var points : Array;

    function polyline () 
    {
	this.points = new Array ();
    }

    function add_point (p)
    {
	points.push (p);
    }

    function random_points_in_bounds (x : Number, y : Number,
				      width : Number, height : Number, 
				      count : Number)
    {
	for (var i : Number = 0; i < count; i++)
	    {
		var x_pos = Math.random () * width;
		var y_pos = Math.random () * height;
		var p : point = new point (x + x_pos, y + y_pos);
		add_point (p);
	    }
    }

    // http://www.gamasutra.com/features/20000210/lander_l3.htm

    function nearest_point_on_line (a : point, b : point, c : point)
    {
	// SEE IF a IS THE NEAREST POINT - ANGLE IS OBTUSE
	var dot_ta = (c.x - a.x) * (b.x - a.x) + 
	    (c.y - a.y) * (b.y - a.y);
	if (dot_ta <= 0) // IT IS OFF THE AVERTEX
      	    {
		return new point (a.x , a.y);
	    }
	var dot_tb : Number = (c.x - b.x) * (a.x - b.x) + 
	    (c.y - b.y) * (a.y - b.y);
	// SEE IF b IS THE NEAREST POINT - ANGLE IS OBTUSE
	if (dot_tb <= 0)
      	    {
		return new point (b.x, b.y);
	    }
	// FIND THE REAL NEAREST POINT ON THE LINE SEGMENT
	// BASED ON RATIO
	var nearest_x : Number = a.x + ((b.x - a.x) * dot_ta) / 
	    (dot_ta + dot_tb);
	var nearest_y : Number = a.y + ((b.y - a.y) * dot_ta) / 
	    (dot_ta + dot_tb);
	return new point (nearest_x, nearest_y);
    }

    function distance_from_line_to_point (a : point, b : point, c : point)
    {
	var nearest : point = nearest_point_on_line (a, b, c);
	return nearest.distance_to_point (c);
    }

    function distance_to_point (p : point)
    {
	var distance_to_poly = Number.MAX_VALUE;
	for (var i : Number = 1; i < points.length; i++)
	    {
		var a: point = points[i - 1];
		var b : point = points[i];
		var d : Number =
		    distance_from_line_to_point (a, b, p);
		if (d < distance_to_poly)
		    {
			distance_to_poly = d;
		    }
	    }

	return distance_to_poly;
    }

    function top_leftmost_point ()
    {
	var top_leftmost = points[0];
	for (var i : Number = 1; i < points.length; i++)
	    {
		if ((points[i].y <= top_leftmost.y) ||
		    ((points[i].y == top_leftmost.y) &&
		     ((points[i].x < top_leftmost.x)))) 
		    {
			top_leftmost = points[i];
		    }
	    }

	return new point (top_leftmost.x, top_leftmost.y);
    }

}