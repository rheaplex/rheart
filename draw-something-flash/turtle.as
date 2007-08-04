/*  turtle.as - A classic computer graphics 'turtle'.
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

class turtle
{
    static var degrees_to_radians = (Math.PI * 2.0) / 360.0;

    var forward_step : Number;
    var turn_step : Number;

    var position : point;
    var direction : Number;

    function turtle (position : point, forward_step : Number, 
		     turn_step : Number)
    {
	this.direction = 90.0;

	this.position = position;
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