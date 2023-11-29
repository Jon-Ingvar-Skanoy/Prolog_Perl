%:- use_module(library(clpfd)).
run:-
    writeln("test"),
    current_prolog_flag(argv, [_,Infile,Outfile|_]),

    read_File(Infile, Outfile).



read_File(Infile, Outfile):-
        open(Infile, read, Stream),
        read_lines_find_size(Stream, not_in_list, [], Puzzles_out),
       writeln(Infile),
       writeln(Outfile),
       maplist(connect_Puzzles,Puzzles_out),
       maplist(solve_Puzzle, Puzzles_out),

        close(Stream),
        open(Outfile, write, Stream_Out, [encoding(utf8)]),
         write(Stream_Out,'puzzles '),
         length(Puzzles_out,NrPuzzles),
         write(Stream_Out,NrPuzzles),
         write(Stream_Out,"\n"),
         maplist(write_Puzzles(Stream_Out),Puzzles_out),
         close(Stream_Out)
      .
connect_Puzzles(Puzzle):-
    Puzzle = [Line1 | Puzzle_rest],
   connect_Puzzle(_, Line1, Puzzle_rest).


solve_Puzzle(Puzzle):-
    maplist(unnamed_line,Puzzle),

    borders(Puzzle),
    threeWhitePuzzle(Puzzle),
    maplist(color,Puzzle),



    maplist(valid_Line,Puzzle),

   % maplist(unifyLine,Puzzle),

    preventCircles(Puzzle).
write_Puzzles(Stream,Puzzle):-
    writePuzzleDone(Puzzle,Stream).

color(Line):-
   maplist(cornerByWhite,Line),
    maplist(noStraightLinesToBlack,Line).

unnamed_line(Line):-
    maplist(unnamed_tile,Line).
% [_ | Tile_Left,Tile_down,Tile_up,Tile_right, Link]
unnamed_tile(Tile):-
    Tile = [_,_,Down1,_,Right1, _,Tile_Down,_,Tile_Right, _],
    (  Tile_Down == [] ->
    true
    ;
    Tile_Down = [_,_,_,Up2,_, _,_,_,_, _],
    Down1 = Up2
    ),
    (  Tile_Right == [] ->
        true
        ;
        Tile_Right = [_,Left3,_,_,_, _,_,_,_, _],
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
unifyTiles([_,false,false,false,false|_]).

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


borders(Puzzle):-
    last(Puzzle,Last_line),

    nth0(0,Puzzle,First_Line),
    maplist(illegal_Down,Last_line),
    maplist(illegal_Up,First_Line),
    process_subList_border(Puzzle).

read_lines_find_size(Stream,not_in_list,Puzzles_in, Puzzles_out):-
    read_line_to_string(Stream,Line),
    writeln(Line),
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
         write("   "),
         writeln(Line),
        append(Creation_list,[List_Line],Appended_List),
        (Line == end_of_file -> true ;
        read_lines_to_list(Stream, in_list, Width,Height, Appended_List,  Puzzle)
        )
    ;
    Puzzle = Creation_list
    )
    .

process_line(Line,List):-
    split_string(Line, "", "\s\t\n", [N_Line]),
    split_string(N_Line, " ", "", Split_Line),
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

%validTiles(Tile):-
 %   member(Tile, [["*",true,true,false,false,[_,true,false,false,true|_],[_,false,true,true,false|_]|_],
  %  ["*",true,false,true,false,[_,true,false,false,true|_],_,[_,false,true,true,false|_]|_],
   % ["*",false,true,false,true,_,[_,false,true,true,false|_],_,[_,true,false,false,true|_]|_],
    %["*",false,false,true,true,_,_,[_,false,true,true,false|_],[_,true,false,false,true|_]|_],
    %["o",true,false,false,true,[_,false,false,true,true|_]|_],
    %["o",true,false,false,true,[_,false,true,false,true|_]|_],
    %["o",true,false,false,true,_,_,_,[_,true,true,false,false|_]|_],
    %["o",true,false,false,true,_,_,_,[_,true,false,true,false|_]|_],
    %["o",false,true,true,false,_,_,[_,true,true,false,false|_]|_],
    %["o",false,true,true,false,_,_,[_,false,true,false,true|_]|_],
    %["o",false,true,true,false,_,_,[_,false,true,false,true|_]|_],
    %["o",false,true,true,false,_,[_,true,false,true,false|_]|_],
    %["e",true,true,false,false|_],
    %["e",true,false,true,false|_],
    %["e",false,true,true,false|_],
    %["e",true,false,false,true|_],
    %["e",false,true,false,true|_],
    %["e",false,true,false,true|_],
    %["e",false,false,false,false|_]
    %]),
    %write_tile(Tile).


validTile(["*"|_]).
validTile(["o"|_]).
validTile(["e",true,true,false,false, Tile_Left,Tile_down,Tile_up,Tile_right, Link]):-
    get_link(Tile_Left,Left_Link),
    get_link(Tile_Right,Right_Link),
    Link = Left_Link,
    Link = Right_Link.
validTile(["e",true,false,true,false, Tile_Left,Tile_down,Tile_up,Tile_right, Link]):-
    get_link(Tile_Left,Link2),
    get_link(Tile_up,Link3),
    Link = Link2,
    Link = Link3.
validTile(["e",false,true,true,false, Tile_Left,Tile_down,Tile_up,Tile_right, Link]):-
    get_link(Tile_down,Link2),
    get_link(Tile_up,Link3),
    Link = Link2,
    Link = Link3.
validTile(["e",true,false,false,true,Tile_Left,Tile_down,Tile_up,Tile_right, Link]):-
     get_link(Tile_Left,Link2),
     get_link(Tile_right,Link3),
     Link = Link2,
     Link = Link3.
validTile(["e",false,true,false,true,Tile_Left,Tile_down,Tile_up,Tile_right, Link]):-
     get_link(Tile_down,Link2),
     get_link(Tile_right,Link3),
     Link = Link2,
     Link = Link3.
validTile(["e",false,false,true,true,Tile_Left,Tile_down,Tile_up,Tile_right, Link]):-
     get_link(Tile_up,Link2),
     get_link(Tile_right,Link3),
     Link = Link2,
     Link = Link3.
validTile(["e",false,false,false,false|_]).


process_subList_border([]).
process_subList_border([Sublist|Sublists]):-

    last(Sublist,Tile),
     nth0(0,Sublist,Tile2),
    illegal_Right(Tile),
    illegal_Left(Tile2),

    process_subList_border(Sublists).

illegal_Down(Tile):-
    get_elements_from_tile(Tile,[_,_,Down,_,_]),
    Down = false.

illegal_Up(Tile):-
    get_elements_from_tile(Tile,[_,_,_,Up,_]),
    Up = false.
illegal_Right(Tile):-
    get_elements_from_tile(Tile,[_, _, _, _,Right]),
    Right = false.
illegal_Left(Tile):-
    get_elements_from_tile(Tile,[_, Left, _, _, _]),
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
    write(Stream,"size "),
    length(Puzzle,Height),
    nth1(1,Puzzle, Line),
    length(Line,Width),
    write(Stream,Width),
    write(Stream,'x'),
    write(Stream,Height),
    write(Stream,"\n"),
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
% ["e",true,false,false,true,Tile_Left,Tile_down,Tile_up,Tile_right, Link]
% ["*",true,true,false,false,[_,true,false,false,true|Left_Rest],[_,false,true,true,false|Down_rest],_,_,Link]
noStraightLinesToBlack(["*",true,true,false,false,[_,true,false,false,true|Left_Rest],[_,false,true,true,false|Down_rest],_,_,Link]):-
    last(Left_Rest,Left_Link),
    last(Down_rest,Down_Link),
    Link = Left_Link,
    Link= Down_Link.
noStraightLinesToBlack(["*",true,false,true,false,[_,true,false,false,true|Rest1],_,[_,false,true,true,false|Rest2],_,Link]):-
        last(Rest1,Left_Link),
        last(Rest2,Down_Link),
        Link = Left_Link,
        Link= Down_Link.
noStraightLinesToBlack(["*",false,true,false,true,_,[_,false,true,true,false|Rest1],_,[_,true,false,false,true|Rest2],Link]):-
            last(Rest1,Left_Link),
            last(Rest2,Down_Link),
            Link = Left_Link,
            Link= Down_Link.
noStraightLinesToBlack(["*",false,false,true,true,_,_,[_,false,true,true,false|Rest1],[_,true,false,false,true|Rest2],Link]):-
            last(Rest1,Left_Link),
            last(Rest2,Down_Link),
            Link = Left_Link,
            Link= Down_Link.
noStraightLinesToBlack(["o"|_]).
noStraightLinesToBlack(["e"|_]).

% ["e",true,false,false,true,Tile_Left,Tile_down,Tile_up,Tile_right, Link]

cornerByWhite(["o",true,false,false,true,[_,false,false,true,true|Rest1],_,_,Tile_Right,Link]):-
            last(Rest1,Left_Link),
            last(Tile_Right,Down_Link),
            get_elements_from_tile(Tile_Right,[Type,Left,Down,Up,Right]),
            Right = true,
            Link = Left_Link,
            Link = Down_Link.
cornerByWhite(["o",true,false,false,true,[_,false,true,false,true|Rest1],_,_,Tile_Right,Link]):-
            last(Rest1,Left_Link),
            last(Tile_Right,Down_Link),
            get_elements_from_tile(Tile_Right,[Type,Left,Down,Up,Right]),
            Right = true,
            Link = Left_Link,
            Link = Down_Link.
cornerByWhite(["o",true,false,false,true,Tile_Left,_,_,[_,true,true,false,false|Rest1],Link]):-
    last(Rest1,Left_Link),
    last(Tile_Left,Down_Link),
    Link = Left_Link,
    Link = Down_Link.
cornerByWhite(["o",true,false,false,true,Tile_Left,_,_,[_,true,false,true,false|Rest1],Link]):-
    last(Rest1,Left_Link),
    last(Tile_Left,Down_Link),
    Link = Left_Link,
    Link = Down_Link.

cornerByWhite(["o",false,true,true,false,_,Tile_Down,[_,true,true,false,false|Rest1], _,Link]):-
    last(Rest1,Left_Link),
    last(Tile_Down,Down_Link),
    get_elements_from_tile(Tile_Down,[Type,Left,Down,Up,Right]),
    Down = true,
    Link = Left_Link,
    Link = Down_Link.
cornerByWhite(["o",false,true,true,false,_,Tile_Down,[_,false,true,false,true|Rest1], _, Link]):-
    last(Rest1,Left_Link),
    last(Tile_Down,Down_Link),
    get_elements_from_tile(Tile_Down,[Type,Left,Down,Up,Right]),
    Down = true,
    Link = Left_Link,
    Link = Down_Link.
cornerByWhite(["o",false,true,true,false,_,[_,false,false,true,true|Rest1],Tile_up,_, Link]):-
    last(Rest1,Left_Link),
    last(Tile_up,Down_Link),
    Link = Left_Link,
    Link = Down_Link.
cornerByWhite(["o",false,true,true,false,_,[_,true,false,true,false|Rest1],Tile_up,_, Link]):-
    last(Rest1,Left_Link),
    last(Tile_up,Down_Link),
    Link = Left_Link,
    Link = Down_Link.
cornerByWhite(["*"|_]).
cornerByWhite(["e"|_]).

connect_Puzzles(Line1,Line2,[]):-

    connect_Line(Line1,Line2,_,[]).

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
    Tile = [_, _, _, _, _, _, _, _, _, FirstLink]
    ;
    getFirstLink([RestOfLine|RestOfPuzzle],FirstLink)).

getFirstLink([[]|RestOfPuzzle]):-
    getFirstLink(RestOfPuzzle).

test(Tile,FirstLink):-

(Tile \= ["e",false,false,false,false|_]->
    Tile = [_,_,_,_,_, _,_,_,_, Link],

    Link == FirstLink

    ;

    true
    )

    .
preventCircles(Puzzle):-
    %writeln(22),
    getFirstLink(Puzzle, FirstLink),
    %writeln(FirstLink),
    foreach(member(Line,Puzzle),foreach(member(Tile,Line),test(Tile,FirstLink)
    )).

connect_Tile(Tile_up,Tile_down,Tile_Left,Tile_right,Tile_Main):-

    Tile_Main = [_,_,_,_,_,Tile_Left,Tile_down,Tile_up,Tile_right,_].

get_link(Tile,Link):-
    Tile = [_, _, _, _, _, _, _, _, _, Link].


threeWhite(["o",false,true,true,false,["o",false,true,true,false|_],_,_,["o",false,true,true,false|_]|_]).
threeWhite(["o",true,false,false,true,_,["o",true,false,false,true|_],["o",true,false,false,true|_]|_]).

threeWhite(["o",_,_,_,_,_,["e"|_],["o"|_]|_]).
threeWhite(["o",_,_,_,_,_,["e"|_],["*"|_]|_]).
threeWhite(["o",_,_,_,_,_,["e"|_],["e"|_]|_]).
threeWhite(["o",_,_,_,_,_,["*"|_],["o"|_]|_]).
threeWhite(["o",_,_,_,_,_,["*"|_],["e"|_]|_]).
threeWhite(["o",_,_,_,_,_,["*"|_],["*"|_]|_]).
threeWhite(["o",_,_,_,_,_,["o"|_],["e"|_]|_]).
threeWhite(["o",_,_,_,_,_,["o"|_],["*"|_]|_]).

threeWhite(["o",_,_,_,_,["e"|_],_,_,["o"|_]|_]).
threeWhite(["o",_,_,_,_,["e"|_],_,_,["*"|_]|_]).
threeWhite(["o",_,_,_,_,["e"|_],_,_,["e"|_]|_]).
threeWhite(["o",_,_,_,_,["*"|_],_,_,["o"|_]|_]).
threeWhite(["o",_,_,_,_,["*"|_],_,_,["e"|_]|_]).
threeWhite(["o",_,_,_,_,["*"|_],_,_,["*"|_]|_]).
threeWhite(["o",_,_,_,_,["o"|_],_,_,["e"|_]|_]).
threeWhite(["o",_,_,_,_,["o"|_],_,_,["*"|_]|_]).


threeWhite(["e"|_]).
threeWhite(["*"|_]).

%adjacentBlack(["*",false,true,false,true,["*",true,_,_,false|_],_,["*",_,false,true,_|_]|_]).
%adjacentBlack(["*",true,true,false,false,_,_,["*",_,false,true,_|_],["*",false,_,_,true|_]|_]).
%adjacentBlack(["*",true,false,true,false,_,["*",_,true,false,_|_],_,["*",false,_,_,true|_]|_]).
%adjacentBlack(["*",false,false,true,true,["*",true,_,_,false|_],["*",_,true,false,_|_],|_]).

adjacentHorizontalBlack(["*",true,_,_,false,_,_,_,["*",false,_,_,true|_]|_]).
adjacentHorizontalBlack(["*",_,_,_,_,_,_,_,["e"|_]|_]).
adjacentHorizontalBlack(["*",_,_,_,_,_,_,_,["o"|_]|_]).
adjacentHorizontalBlack(["*",_,_,_,_,_,_,_,[]|_]).

adjacentHorizontalBlack(["e"|_]).
adjacentHorizontalBlack(["o"|_]).

adjacentVerticalBlack(["*",_,false,true,_,["*",_,true,false,_|_]|_]).
adjacentVerticalBlack(["*",_,_,_,_,_,["e"|_]|_]).
adjacentVerticalBlack(["*",_,_,_,_,_,["o"|_]|_]).
adjacentVerticalBlack(["*",_,_,_,_,_,[]|_]).

adjacentVerticalBlack(["e"|_]).
adjacentVerticalBlack(["o"|_]).

blackTwoRightWhite(["*",true,_,_,false,_,_,_,[_,_,_,_,_,_,_,_,["o",_,_,_,_,_,_,_,["o"|_]|_]|_]|_]).
blackTwoRightWhite(["*",_,_,_,_,_,_,_,[_,_,_,_,_,_,_,_,["o",_,_,_,_,_,_,_,["e"|_]|_]|_]|_]).
blackTwoRightWhite(["*",_,_,_,_,_,_,_,[_,_,_,_,_,_,_,_,["o",_,_,_,_,_,_,_,["*"|_]|_]|_]|_]).
blackTwoRightWhite(["*",_,_,_,_,_,_,_,[_,_,_,_,_,_,_,_,["o",_,_,_,_,_,_,_,[]|_]|_]|_]).
blackTwoRightWhite(["*",_,_,_,_,_,_,_,[_,_,_,_,_,_,_,_,["e"|_]|_]|_]).
blackTwoRightWhite(["*",_,_,_,_,_,_,_,[_,_,_,_,_,_,_,_,["*"|_]|_]|_]).
blackTwoRightWhite(["*",_,_,_,_,_,_,_,[_,_,_,_,_,_,_,_,[]|_]|_]).
blackTwoRightWhite(["*",_,_,_,_,_,_,_,[]|_]).
blackTwoRightWhite(["o"|_]).
blackTwoRightWhite(["e"|_]).

blackTwoUpWhite(["*",_,true,false,_,_,_,[_,_,_,_,_,_,_,["o",_,_,_,_,_,_,["o"|_]|_]|_]|_]).
blackTwoUpWhite(["*",_,_,_,_,_,_,[_,_,_,_,_,_,_,["o",_,_,_,_,_,_,["e"|_]|_]|_]|_]).
blackTwoUpWhite(["*",_,_,_,_,_,_,[_,_,_,_,_,_,_,["o",_,_,_,_,_,_,["*"|_]|_]|_]|_]).
blackTwoUpWhite(["*",_,_,_,_,_,_,[_,_,_,_,_,_,_,["o",_,_,_,_,_,_,[]|_]|_]|_]).
blackTwoUpWhite(["*",_,_,_,_,_,_,[_,_,_,_,_,_,_,["e"|_]|_]|_]).
blackTwoUpWhite(["*",_,_,_,_,_,_,[_,_,_,_,_,_,_,["*"|_]|_]|_]).
blackTwoUpWhite(["*",_,_,_,_,_,_,[_,_,_,_,_,_,_,[]|_]|_]).
blackTwoUpWhite(["*",_,_,_,_,_,_,[]|_]).
blackTwoUpWhite(["o"|_]).
blackTwoUpWhite(["e"|_]).

blackTwoDownWhite(["*",_,false,true,_,_,[_,_,_,_,_,_,["o",_,_,_,_,_,["o"|_]|_]|_]|_]).
blackTwoDownWhite(["*",_,_,_,_,_,[_,_,_,_,_,_,["o",_,_,_,_,_,["e"|_]|_]|_]|_]).
blackTwoDownWhite(["*",_,_,_,_,_,[_,_,_,_,_,_,["o",_,_,_,_,_,["*"|_]|_]|_]|_]).
blackTwoDownWhite(["*",_,_,_,_,_,[_,_,_,_,_,_,["o",_,_,_,_,_,[]|_]|_]|_]).
blackTwoDownWhite(["*",_,_,_,_,_,[_,_,_,_,_,_,["e"|_]|_]|_]).
blackTwoDownWhite(["*",_,_,_,_,_,[_,_,_,_,_,_,["*"|_]|_]|_]).
blackTwoDownWhite(["*",_,_,_,_,_,[_,_,_,_,_,_,[]|_]|_]).
blackTwoDownWhite(["*",_,_,_,_,_,[]|_]).
blackTwoDownWhite(["o"|_]).
blackTwoDownWhite(["e"|_]).

blackTwoLeftWhite(["*",false,_,_,true,[_,_,_,_,_,["o",_,_,_,_,["o"|_]|_]|_]|_]).
blackTwoLeftWhite(["*",_,_,_,_,[_,_,_,_,_,["o",_,_,_,_,["e"|_]|_]|_]|_]).
blackTwoLeftWhite(["*",_,_,_,_,[_,_,_,_,_,["o",_,_,_,_,["*"|_]|_]|_]|_]).
blackTwoLeftWhite(["*",_,_,_,_,[_,_,_,_,_,["o",_,_,_,_,[]|_]|_]|_]).
blackTwoLeftWhite(["*",_,_,_,_,[_,_,_,_,_,["e"|_]|_]|_]).
blackTwoLeftWhite(["*",_,_,_,_,[_,_,_,_,_,["*"|_]|_]|_]).
blackTwoLeftWhite(["*",_,_,_,_,[_,_,_,_,_,[]|_]|_]).
blackTwoLeftWhite(["*",_,_,_,_,[]|_]).
blackTwoLeftWhite(["o"|_]).
blackTwoLeftWhite(["e"|_]).

blackTwoWhite(Tile):-
    blackTwoDownWhite(Tile),
    blackTwoUpWhite(Tile),
    blackTwoLeftWhite(Tile),
    blackTwoRightWhite(Tile).

threeWhiteLine(Line):-
    maplist(threeWhite,Line),
    maplist(adjacentHorizontalBlack,Line),
    maplist(adjacentVerticalBlack,Line),
    maplist(blackTwoWhite,Line).

threeWhitePuzzle(Puzzle):-
    maplist(threeWhiteLine,Puzzle).

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
:- run.
%:- halt.