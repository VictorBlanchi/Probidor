# Probidor


# Protocol 

The server (frontend) sends commands to the client (backend) : 

Start a new game : 
> { "command": "make_game", "rows": 9, "columns": 9, "wall_count": 8 }

Move the pawn : 
> { "command": "move_pawn", "direction": "up" }
Valid directions : "up", "down", "left", "right"

Place a wall : 
> { "command": "place_wall", "orientation": "horizontal", "coordinates": [ row, col ] }
Valid orientations : "horizontal", "vertical" 
The coordinates go from 0 to row (inclusive) and 0 to columns (inclusive). They indicate the upper-left corner of the wall.

To each command, the client answers :
> { "answer": "ok" }
> { "answer": "error", "reason": ...}
The reason can be any string.