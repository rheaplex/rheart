/*  draw_something.as - Main program code.
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

class draw_something
{
    static var num_points : Number = 12;

    static var instance : draw_something = null;

    var drawing_in_progress : drawing = null;
    var show_skeleton : Boolean = false;

    var delay_start : Number = 0;
    var delay_duration = false;

    function draw_something () 
    {
	_root.createEmptyMovieClip ("skeleton", _root.getNextHighestDepth ());
	_root.createEmptyMovieClip ("outline", _root.getNextHighestDepth ());
	new_drawing ();
    }

    function debug_show_skeleton ()
    {
	debug_hide_skeleton ();
	_root.skeleton.lineStyle(1, 0x0000FF, 100);
	_root.skeleton.moveTo (drawing_in_progress.skeleton.points[0].x, 
			       drawing_in_progress.skeleton.points[0].y);
	for (var i : Number = 1; 
	     i < drawing_in_progress.skeleton.points.length; 
	     i++)
	    {
		_root.skeleton.
		    lineTo (drawing_in_progress.skeleton.points[i].x, 
			    drawing_in_progress.skeleton.points[i].y);
	    }
    }

    function debug_hide_skeleton ()
    {
	_root.skeleton.clear ();
    }

    function debug_toggle_skeleton ()
    {
	show_skeleton = ! show_skeleton;
	if (show_skeleton)
	    debug_show_skeleton ();
	else
	    debug_hide_skeleton ();
    }

    function onKeyDown () {}

    function onKeyUp ()
    {
	if (Key.getAscii () == 97) // a
	    debug_toggle_skeleton ();
    }

    function new_drawing ()
    {
	drawing_in_progress = new drawing (Stage.width,
					   Stage.height,
					   num_points);

	if (show_skeleton)
	    debug_show_skeleton ();
	
       	_root.outline.clear ();
	
	_root.outline.lineStyle(2, 0x000000, 100);
	_root.outline.moveTo (drawing_in_progress.pen.position.x,
			      drawing_in_progress.pen.position.y);
    }

    function onEnterFrame ()
    {
	if (delay_duration) 
	    {
		if (delay_duration <= ((getTimer() - delay_start) / 1000)) 
		    {
			// Time is up!
			delay_duration = false;
			new_drawing ();
		    }
	    }
	else
	    {
		drawing_in_progress.to_next_point ();
		_root.outline.lineTo (drawing_in_progress.pen.position.x, 
				      drawing_in_progress.pen.position.y);
		
		if (drawing_in_progress.should_finish ())
		    {
			delay_start = getTimer ();
			delay_duration = 10;    
		    }
	    }
    }

    // entry point
    static function main (mc) 
    {
	instance = new draw_something();
	Key.addListener (instance);
	_root.outline.focusEnabled = true;
	Selection.setFocus ("_root");
	_root.onEnterFrame = 
	    function ()
	    {
		draw_something.instance.onEnterFrame();
	    }
    }
}