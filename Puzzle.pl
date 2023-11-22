

read_File(Infile, Outfile):-
        open(Infile, read, Stream),
        read_lines_find_size(Stream, not_in_list, [], Puzzles_out),
        nth1(3,Puzzles_out,Puzzle_1),
        Puzzle_1 = [Line1 | Puzzle_rest],
        connect_Puzzle(Line0, Line1, Puzzle_rest),

       solve_Puzzle(Puzzle_1, Solution),

        close(Stream),
        open(Outfile, write, Stream_Out, [encoding(utf8)]),
         writePuzzleDone(Puzzle_1,Stream_Out),
         close(Stream_Out)
        .

solve_Puzzle(Puzzle,Solution):-
    maplist(unnamed_line,Puzzle),

    borders(Puzzle),




    maplist(valid_Line,Puzzle),

    maplist(unifyLine,Puzzle),

    preventCircles(Puzzle).

color(Line):-
   % maplist(cornerByWhite,Line),
    maplist(noStraightLinesToBlack,Line).

unnamed_line(Line):-
    maplist(unnamed_tile,Line).

unnamed_tile(Tile):-
    Tile = [Type1,Left1,Down1,Up1,Right1, Tile_Left,Tile_Down,Tile_Up,Tile_Right, _],
    (  Tile_Down == [] ->
    true
    ;
    Tile_Down = [Type2,Left2,Down2,Up2,Right2, Tile_Left2,Tile_Down2,Tile_Up2,Tile_Right2, _],
    Down1 = Up2
    ),
    (  Tile_Right == [] ->
        true
        ;
        Tile_Right = [Type3,Left3,Down3,Up3,Right3, Tile_Left3,Tile_Down3,Tile_Up3,Tile_Right3, _],
        Right1 = Left3
        ).
unifyLine(Line):-
    maplist(unifyTiles,Line).

unifyTiles([_,true,_,_,true,[_,_,_,_,true,_,_,_,_,Link2],_,_,[_,true,_,_,_,_,_,_,_,Link3],Link1]):-
    Link1 = Link2,
    Link3 = Link1.

unifyTiles([_,true,_,true,_,[_,_,_,_,true,_,_,_,_,Link2],_,[_,_,true,_,_,_,_,_,_,Link3],_,Link1]):-
    Link1 = Link2,
    Link3 = Link1.
    
unifyTiles([_,true,true,_,_,[_,_,_,_,true,_,_,_,_,Link2],[_,_,_,true,_,_,_,_,_,Link3],_,_,Link1]):-
    Link1 = Link2,
    Link3 = Link1.

unifyTiles([_,_,true,_,true,_,[_,_,_,true,_,_,_,_,_,Link2],_,[_,true,_,_,_,_,_,_,_,Link3],Link1]):-
    Link1 = Link2,
    Link3 = Link1.

unifyTiles([_,_,_,true,true,_,_,[_,_,true,_,_,_,_,_,_,Link2],[_,true,_,_,_,_,_,_,_,Link3],Link1]):-
    Link1 = Link2,
    Link3 = Link1.

unifyTiles([_,_,true,true,_,_,[_,_,_,true,_,_,_,_,_,Link2],[_,_,true,_,_,_,_,_,_,Link3],_,Link1]):-
    Link1 = Link2,
    Link3 = Link1.


%unifyTiles([_,_,true,_,_,_,_,[_,_,true,_,_,_,_,_,_,Link2],_,Link1]):-
    %writeln("unifying below->above"),
    %Link1 = Link2.
%unifyTiles([_,_,_,true,_,_,[_,_,_,true,_,_,_,_,_,Link2],_,_,Link1]):-
    %writeln("unifying above->below"),
    %Link1 = Link2.
%unifyTiles([_,_,_,_,true,_,_,_,[_,true,_,_,_,_,_,_,_,Link2],Link1]):-
    %writeln("unifying right->left"),
    %Link1 = Link2.

verifyUnification([_,true,_,_,true,[_,_,_,_,true,_,_,_,_,Link2],_,_,[_,true,_,_,_,_,_,_,_,Link3],Link1]):-
    Link1 == Link2,
    Link3 == Link1.

verifyUnification([_,true,_,true,_,[_,_,_,_,true,_,_,_,_,Link2],_,[_,_,true,_,_,_,_,_,_,Link3],_,Link1]):-
    Link1 == Link2,
    Link3 == Link1.

verifyUnification([_,true,true,_,_,[_,_,_,_,true,_,_,_,_,Link2],[_,_,_,true,_,_,_,_,_,Link3],_,_,Link1]):-
    Link1 == Link2,
    Link3 == Link1.

verifyUnification([_,_,true,_,true,_,[_,_,_,true,_,_,_,_,_,Link2],_,[_,true,_,_,_,_,_,_,_,Link3],Link1]):-
    Link1 == Link2,
    Link3 == Link1.

verifyUnification([_,_,_,true,true,_,_,[_,_,true,_,_,_,_,_,_,Link2],[_,true,_,_,_,_,_,_,_,Link3],Link1]):-
    Link1 == Link2,
    Link3 == Link1.

verifyUnification([_,_,true,true,_,_,[_,_,_,true,_,_,_,_,_,Link2],[_,_,true,_,_,_,_,_,_,Link3],_,Link1]):-
    Link1 == Link2,
    Link3 == Link1.

verifyUnification(["e",false,false,false,false|_]).

unifyTiles([_,false,false,false,false|_]).
borders(Puzzle):-
    last(Puzzle,Last_line),

    nth0(0,Puzzle,First_Line),
    maplist(illegal_Down,Last_line),
    maplist(illegal_Up,First_Line),
    process_subList_border(Puzzle).

read_lines_find_size(Stream,not_in_list,Puzzles_in, Puzzles_out):-
    read_line_to_string(Stream,Line),
    (Line == end_of_file ->

    Puzzles_out = Puzzles_in


    ;
    (is_new_puzzle_line(Line) ->
    get_size(Line,Size),
    split_string(Size, "x","", [W,H]),
    number_string(Width, W),
    number_string(Height, H),
    read_lines_to_list(Stream, in_list, Width,Height, [], Puzzle),

    append(Puzzles_in,[Puzzle], Puzzles_in2),
    read_lines_find_size(Stream, not_in_list, Puzzles_in2,Puzzles_out)
    ;
    read_lines_find_size(Stream, not_in_list, [], Puzzles_out)
    ),

    true

    ).

read_lines_to_list(Stream, in_list, Width,Height, Creation_list,Puzzle):-
    length(Creation_list,CurrentHeight),

    (CurrentHeight < Height ->
        read_line_to_string(Stream, Line),
        process_line(Line,List_Line),

        append(Creation_list,[List_Line],Appended_List),
        (Line == end_of_file -> true ;
        read_lines_to_list(Stream, in_list, Width,Height, Appended_List,  Puzzle)
        )
    ;
    Puzzle = Creation_list
    )
    .

process_line(Line,List):-

    split_string(Line, " ", "", Split_Line),
    maplist(extended_element, Split_Line,List)
    .

extended_element("_", ["e", _, _, _, _, _, _, _, _, _]).
extended_element("*", ["*", _, _, _, _, _, _, _, _, _]).
extended_element("o", ["o", _, _, _, _, _, _, _, _, _]).
is_new_puzzle_line(Line):-
    sub_string(Line, 0 , _, _, "size ").

get_size(Line,Size):-
    sub_string(Line, 5, _, 0, Size).

get_elements_from_tile(Tile,[Type,Left,Down,Up,Right]):-
    Tile = [Type,Left,Down,Up,Right, _, _, _, _, _].

valid_Line(Line):-
    maplist(validTile,Line).


cornerByWhite(["o",false,true,true,false,_,[_,false,false,true,true|_]|_]).
cornerByWhite(["o",false,true,true,false,_,[_,true,false,true,false|_]|_]).


validTile(["*",true,true,false,false,[_,true,false,false,true|_],[_,false,true,true,false|_]|_]).
validTile(["*",true,false,true,false,[_,true,false,false,true|_],_,[_,false,true,true,false|_]|_]).
validTile(["*",false,true,false,true,_,[_,false,true,true,false|_],_,[_,true,false,false,true|_]|_]).
validTile(["*",false,false,true,true,_,_,[_,false,true,true,false|_],[_,true,false,false,true|_]|_]).
validTile(["o",true,false,false,true,[_,false,false,true,true|_]|_]).
validTile(["o",true,false,false,true,[_,false,true,false,true|_]|_]).
validTile(["o",true,false,false,true,_,_,_,[_,true,true,false,false|_]|_]).
validTile(["o",true,false,false,true,_,_,_,[_,true,false,true,false|_]|_]).
validTile(["o",false,true,true,false,_,_,[_,true,true,false,false|_]|_]).
validTile(["o",false,true,true,false,_,_,[_,false,true,false,true|_]|_]).
validTile(["o",false,true,true,false,_,[_,false,false,true,true|_]|_]).
validTile(["o",false,true,true,false,_,[_,true,false,true,false|_]|_]).
validTile(["e",true,true,false,false|_]).
validTile(["e",true,false,true,false|_]).
validTile(["e",false,true,true,false|_]).
validTile(["e",true,false,false,true|_]).
validTile(["e",false,true,false,true|_]).
validTile(["e",false,false,true,true|_]).
validTile(["e",false,false,false,false|_]).

validTile(["Ø",false,false,false,false|Rest]).

process_subList_border([]).
process_subList_border([Sublist|Sublists]):-

    last(Sublist,Tile),
     nth0(0,Sublist,Tile2),
    illegal_Right(Tile),
    illegal_Left(Tile2),

    process_subList_border(Sublists).

illegal_Down(Tile):-
    get_elements_from_tile(Tile,[Type,Left,Down,Up,Right]),
    Down = false.

illegal_Up(Tile):-
    get_elements_from_tile(Tile,[Type,Left,Down,Up,Right]),
    Up = false.
illegal_Right(Tile):-
    get_elements_from_tile(Tile,[Type,Left,Down,Up,Right]),
    Right = false.
illegal_Left(Tile):-
    get_elements_from_tile(Tile,[Type,Left,Down,Up,Right]),
    Left = false.
write_Puzzle(Puzzle):-

    maplist(write_line,Puzzle).
write_line(Line):-
    maplist(write_tile,Line),
    writeln("").

write_tile(Tile):-
    get_elements_from_tile(Tile,[Type,Left,Down,Up,Right]),
    write(Type),
    write(" "),
    write(Left),
    write(" "),
    write(Down),
    write(" "),
    write(Up),
    write(" "),
    write(Right),
    write(" ").

writePuzzleDone(Puzzle, Stream):-
    maplist(writeLineDone(Stream),Puzzle).
writeLineDone(Stream,Line):-
    maplist(writeTileDone(Stream),Line),
    write(Stream,"\n").
writeTileDone(Stream,["*"|_]):-
    write(Stream,"\u253C").
 writeTileDone(Stream,["o",true,false,false,true|_]):-
    write(Stream,"\u2568").
 writeTileDone(Stream,["o",false,true,true,false|_]):-
    write(Stream,"\u2561").
writeTileDone(Stream,["e",true,true,false,false|_]):-
    write(Stream,"\u2510").
writeTileDone(Stream,["e",true,false,true,false|_]):-
    write(Stream,'\u2518').
writeTileDone(Stream,["e",true,false,false,true|_]):-
    write(Stream,'\u2500').
writeTileDone(Stream,["e",false,true,true,false|_]):-
    write(Stream,'\u2502').
writeTileDone(Stream,["e",false,true,false,true|_]):-
    write(Stream,"\u250C").
writeTileDone(Stream,["e",false,false,true,true|_]):-
    write(Stream,"\u2514").
writeTileDone(Stream,["e",false,false,false,false|_]):-
    write(Stream," ").

noStraightLinesToBlack(["*",true,true,false,false,[_,true,false,false,true|_],[_,false,true,true,false|_]|_]).
noStraightLinesToBlack(["*",true,false,true,false,[_,true,false,false,true|_],_,[_,false,true,true,false|_]|_]).
noStraightLinesToBlack(["*",false,true,false,true,_,[_,false,true,true,false|_],_,[_,true,false,false,true|_]|_]).
noStraightLinesToBlack(["*",false,false,true,true,_,_,[_,false,true,true,false|_],[_,true,false,false,true|_]|_]).
noStraightLinesToBlack(["o"|_]).
noStraightLinesToBlack(["e"|_]).


cornerByWhite(["o",true,false,false,true,[_,false,false,true,true|_]|_]).
cornerByWhite(["o",true,false,false,true,[_,false,true,false,true|_]|_]).
cornerByWhite(["o",true,false,false,true,_,_,_,[_,true,true,false,false|_]|_]).
cornerByWhite(["o",true,false,false,true,_,_,_,[_,true,false,true,false|_]|_]).
cornerByWhite(["o",false,true,true,false,_,_,[_,true,true,false,false|_]|_]).
cornerByWhite(["o",false,true,true,false,_,_,[_,false,true,false,true|_]|_]).
cornerByWhite(["o",false,true,true,false,_,[_,false,false,true,true|_]|_]).
cornerByWhite(["o",false,true,true,false,_,[_,true,false,true,false|_]|_]).
cornerByWhite(["*"|_]).
cornerByWhite(["e"|_]).

connect_Puzzles(Line1,Line2,[]):-

    connect_Line(Line1,Line2,Line3,[]).

connect_Puzzle(Line1,Line2,[Line3|Puzzle_rest]):-

    connect_Line(Line1,Line2,Line3,[]),
    (Puzzle_rest == [] ->
    connect_Puzzles(Line2,Line3,[])
    ;
    connect_Puzzle(Line2,Line3,Puzzle_rest)
    )
   .
connect_Lines([Tile1],[Tile2],[Tile3],Tile):-
    (nonvar(Tile3) ->
         true
         ;
         Tile3 = []
         ),
        (nonvar(Tile1) ->
         true
         ;
         Tile1 = []
         ),

    connect_Tile(Tile1,Tile3,Tile,[],Tile2)
   .
connect_Line([Tile1|Line1], [Tile2, Tile222|Line2], [Tile3|Line3],Tile22):-

    (nonvar(Tile3) ->
        true
        ;
         Tile3 = []
        ),
    (nonvar(Tile1) ->
      true
       ;
        Tile1 = []
      ),
    connect_Tile(Tile1,Tile3,Tile22,Tile222,Tile2),


    append([Tile222],Line2,Line_2),

    (Line2 == [] ->
    connect_Lines(Line1,Line_2,Line3,Tile2)
    ;
     connect_Line(Line1,Line_2,Line3,Tile2)
    )
    .

getFirstLink([[Tile|RestOfLine]|RestOfPuzzle],FirstLink):-
    (Tile \= ["e",false,false,false,false|_]->
    Tile = [Type1,Left1,Down1,Up1,Right1, Tile_Left,Tile_Down,Tile_Up,Tile_Right, FirstLink]
    ;
    getFirstLink([[RestOfLine]|RestOfPuzzle])).

getFirstLink([[]|RestOfPuzzle]):-
    getFirstLink(RestOfPuzzle).

test(Tile,FirstLink):-

(Tile \= ["e",false,false,false,false|_]->
    Tile = [Type1,Left1,Down1,Up1,Right1, Tile_Left,Tile_Down,Tile_Up,Tile_Right, Link],

    Link == FirstLink

    ;

    true
    )

    .
preventCircles(Puzzle):-
    getFirstLink(Puzzle, FirstLink),
    writeln(FirstLink),
    foreach(member(Line,Puzzle),foreach(member(Tile,Line),test(Tile,FirstLink)
    )).

connect_Tile(Tile_up,Tile_down,Tile_Left,Tile_right,Tile_Main):-

    Tile_Main = [_,_,_,_,_,Tile_Left,Tile_down,Tile_up,Tile_right,_].




circle_Tile([Tile|Line], Tile2):-
 (eTile(Tile) ->
 circle_Tile(Line,Tile2)
 ;
 Tile = [_,_,_,_,_,_,_,_,_,Link3] ,
  Tile2 = [_,_,_,_,_,_,_,_,_,Link2],
  Link3 == Link2,
  circle_Tile(Line,Tile)
 ) .


eTile(["e",false,false,false,false|_]).