/*  point.as - A 2D point.
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