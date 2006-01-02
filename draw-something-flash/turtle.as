/*  turtle.as - A classic computer graphics 'turtle'.
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

class turtle
{
    static var degrees_to_radians = (Math.PI * 2.0) / 360.0;

    var distance : Number;
    var distance_fuzz : Number; 
    var forward_step : Number;
    var turn_step : Number;

    var position : point;
    var direction : Number;

    function turtle (position : point, distance : Number, 
		     distance_fuzz : Number, forward_step : Number,
		     turn_step : Number)
    {
	this.direction = 90.0;

	this.position = position;
	this.distance = distance;
	this.distance_fuzz = distance_fuzz;
	this.forward_step = forward_step;
	this.turn_step = turn_step;
    }

    function left ()
    {
	direction -= turn_step;
    }

    function right ()
    {
	direction += turn_step;
    }

    function forward ()
    {
	position = next_point_would_be ();
    }

    function next_point_would_be ()
    {
	var x : Number = position.x +
	    (forward_step * Math.sin (direction * degrees_to_radians));
	var y : Number = position.y +
	    (forward_step * Math.cos (direction * degrees_to_radians));
	return new point (x, y);
    }
}