/*  point.as - A 2D point.
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

class point 
{
    var x : Number;
    var y : Number;

    function point (x : Number, y : Number) 
    {
	this.x = x;
	this.y = y;
    }

    function distance_to_point (p : point)
    {
	return Math.sqrt (Math.pow (p.x - x, 2) +
			  Math.pow (p.y - y, 2));
    }
}