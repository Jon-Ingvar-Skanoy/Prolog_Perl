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
    threeWhitePuzzle(Puzzle),!,
    maplist(color,Puzzle),



%    Puzzle = [Line1, Line2, Line3, Line4, Line5, Line6, Line7, Line8, Line9, Line10, Line11, Line12, Line13, Line14, Line15],
 %  Puzzle1 = [Line7, Line8, Line6, Line9, Line5, Line10, Line11, Line4, Line12, Line3, Line13, Line2, Line14, Line1, Line15],


    maplist(valid_Line,Puzzle),
   preventCircles(Puzzle).
write_Puzzles(Stream,Puzzle):-
    writePuzzleDone(Puzzle,Stream).

color(Line):-
   maplist(noStraightLinesToBlack,Line),
   maplist(cornerByWhite,Line).

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
noStraightLinesToBlack(["o"|_]).
noStraightLinesToBlack(["e"|_]).
noStraightLinesToBlack(["*",true,true,false,false,[_,true,false,false,true,_,_,_,_,Link],[_,false,true,true,false,_,_,_,_,Link1],_,_,Link]).
noStraightLinesToBlack(["*",true,false,true,false,[_,true,false,false,true,_,_,_,_,Link],_,[_,false,true,true,false,_,_,_,_,Link],_,Link]).
noStraightLinesToBlack(["*",false,true,false,true,_,[_,false,true,true,false,_,_,_,_,Link2],_,[_,true,false,false,true,_,_,_,_,Link1],Link]).
noStraightLinesToBlack(["*",false,false,true,true,_,_,[_,false,true,true,false,_,_,_,_,Link],[_,true,false,false,true,_,_,_,_,Link1],Link]).


cornerByWhite(["*"|_]).
cornerByWhite(["e"|_]).

cornerByWhite(["o",true,false,false,true,[_,false,_,_,true,_,_,_,_,Link],_,_,[_,_,_,_,true,_,_,_,_,Link1],Link]).
cornerByWhite(["o",true,false,false,true,[_,_,_,_,_,_,_,_,_,Link],_,_,[_,true,_,_,false,_,_,_,_,Link1],Link]).
cornerByWhite(["o",false,true,true,false,_,[_,_,true,_,_,_,_,_,_,Link1],[_,_,true,false,_,_,_,_,_,Link], _, Link]).
cornerByWhite(["o",false,true,true,false,_,[_,_,false,true,_,_,_,_,_,Link1],[_,_,_,_,_,_,_,_,_,Link],_, Link]).



validTile(["*"|_]).
validTile(["o"|_]).
% [_,_,_,_,_,_,_,_,_,Link]
validTile(["e",false,true,true,false, Tile_Left,_,[_,_,_,_,_,_,_,_,_,Link],Tile_right, Link]).
validTile(["e",true,false,false,true,[_,_,_,_,_,_,_,_,_,Link],Tile_down,Tile_up,_,Link]).
validTile(["e",true,true,false,false, [_,_,_,_,_,_,_,_,_,Link],Tile_down,Tile_up,Tile_right, Link]).
validTile(["e",true,false,true,false, [_,_,_,_,_,_,_,_,_,Link],Tile_down,[_,_,_,_,_,_,_,_,_,Link],Tile_right, Link]).


validTile(["e",false,true,false,true,Tile_Left,_,Tile_up,_, Link]).
validTile(["e",false,false,true,true,Tile_Left,Tile_down,[_,_,_,_,_,_,_,_,_,Link],_, Link]).
validTile(["e",false,false,false,false|_]).


process_subList_border([]).
process_subList_border([Sublist|Sublists]):-

    last(Sublist,Tile),
     nth0(0,Sublist,Tile2),
    illegal_Right(Tile),
    illegal_Left(Tile2),

    process_subList_border(Sublists).

illegal_Down(Tile):-
    Tile = [_,_,Down,_,_,_,_,UpTile,_,_],
    blackOneFromDownBorder(UpTile),
    Down = false.

illegal_Up(Tile):-
    Tile = [_,_,_,Up,_,_,DownTile,_,_,_],
    blackOneFromUpBorder(DownTile),
    Up = false.
illegal_Right(Tile):-
    get_elements_from_tile(Tile,[_, _, _, _,Right]),
    Right = false.
illegal_Left(Tile):-
    get_elements_from_tile(Tile,[_, Left, _, _, _]),
    Left = false.

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


% ["e",true,false,false,true,Tile_Left,Tile_down,Tile_up,Tile_right, Link]



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



threeWhiteVertical(["o",true,false,false,true,_,["o",true,false,false,true|_],["o",true,false,false,true|_]|_]).

threeWhiteVertical(["o",_,_,_,_,_,["e"|_],["o"|_]|_]).
threeWhiteVertical(["o",_,_,_,_,_,["e"|_],["*"|_]|_]).
threeWhiteVertical(["o",_,_,_,_,_,["e"|_],["e"|_]|_]).
threeWhiteVertical(["o",_,_,_,_,_,["e"|_],[]|_]).
threeWhiteVertical(["o",_,_,_,_,_,["*"|_],["o"|_]|_]).
threeWhiteVertical(["o",_,_,_,_,_,["*"|_],[]|_]).
threeWhiteVertical(["o",_,_,_,_,_,["*"|_],["e"|_]|_]).
threeWhiteVertical(["o",_,_,_,_,_,["*"|_],["*"|_]|_]).
threeWhiteVertical(["o",_,_,_,_,_,["o"|_],["e"|_]|_]).
threeWhiteVertical(["o",_,_,_,_,_,["o"|_],["*"|_]|_]).
threeWhiteVertical(["o",_,_,_,_,_,["o"|_],[]|_]).
threeWhiteVertical(["o",_,_,_,_,_,[],["e"|_]|_]).
threeWhiteVertical(["o",_,_,_,_,_,[],["o"|_]|_]).
threeWhiteVertical(["o",_,_,_,_,_,[],["*"|_]|_]).

threeWhiteVertical(["e"|_]).
threeWhiteVertical(["*"|_]).

threeWhiteHorizontal(["o",false,true,true,false,["o",false,true,true,false|_],_,_,["o",false,true,true,false|_]|_]).

threeWhiteHorizontal(["o",_,_,_,_,["e"|_],_,_,["o"|_]|_]).
threeWhiteHorizontal(["o",_,_,_,_,["e"|_],_,_,["*"|_]|_]).
threeWhiteHorizontal(["o",_,_,_,_,["e"|_],_,_,[]|_]).
threeWhiteHorizontal(["o",_,_,_,_,["e"|_],_,_,["e"|_]|_]).
threeWhiteHorizontal(["o",_,_,_,_,["*"|_],_,_,["o"|_]|_]).
threeWhiteHorizontal(["o",_,_,_,_,["*"|_],_,_,["e"|_]|_]).
threeWhiteHorizontal(["o",_,_,_,_,["*"|_],_,_,["*"|_]|_]).
threeWhiteHorizontal(["o",_,_,_,_,["*"|_],_,_,[]|_]).
threeWhiteHorizontal(["o",_,_,_,_,["o"|_],_,_,["e"|_]|_]).
threeWhiteHorizontal(["o",_,_,_,_,["o"|_],_,_,["*"|_]|_]).
threeWhiteHorizontal(["o",_,_,_,_,["o"|_],_,_,[]|_]).
threeWhiteHorizontal(["o",_,_,_,_,[],_,_,["o"|_]|_]).
threeWhiteHorizontal(["o",_,_,_,_,[],_,_,["*"|_]|_]).
threeWhiteHorizontal(["o",_,_,_,_,[],_,_,["e"|_]|_]).

threeWhiteHorizontal(["e"|_]).
threeWhiteHorizontal(["*"|_]).



threeWhite(Tile):-
 threeWhiteVertical(Tile),
 threeWhiteHorizontal(Tile).


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

adjacentVerticalBlack(["*",_,false,true,_,_, ["*",_,true,false,_|_]|_]).
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

blackOneFromRightBorder(["*",true,_,_,false|_]).
blackOneFromRightBorder(["o"|_]).
blackOneFromRightBorder(["e"|_]).

blackOneFromLeftBorder(["*",false,_,_,true|_]).
blackOneFromLeftBorder(["o"|_]).
blackOneFromLeftBorder(["e"|_]).

blackOneFromUpBorder(["*",_,true,false,_|_]).
blackOneFromUpBorder(["o"|_]).
blackOneFromUpBorder(["e"|_]).

blackOneFromDownBorder(["*",_,false,true,_|_]).
blackOneFromDownBorder(["o"|_]).
blackOneFromDownBorder(["e"|_]).

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
 ).






% [_, _, _, _, _, _, _, _, _, Link]

validTile_control(Tile):-
    Tile = [Type,Left,Down,Up,Right, Tile_Left,Tile_down,Tile_up,Tile_right, _],
    (nonvar(Left) ; nonvar(Right) ; nonvar(Down) ; nonvar(Up) ),
    validTile_control(Tile_right).
validTile_control([]).
validTile_control(Tile):-
    Tile = [Type,Left,Down,Up,Right, Tile_Left,Tile_down,Tile_up,Tile_right, _],
    (var(Left) ; var(Down)  ; var(Right) ;var(Up)),
    validTile_control(Tile_right).

eTile(["e",false,false,false,false|_]).
:- run.
%:- halt.
