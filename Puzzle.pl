

read_File(Infile):-
        open(Infile, read, Stream),
        read_lines_find_size(Stream, not_in_list, [], Puzzles_out),
        nth0(0,Puzzles_out,Puzzle_1),

        Puzzle_1 = [Line1| Puzzle_rest],
        connect_Puzzle(Line0,Line1,Puzzle_rest),

      % solve_Puzzle(Puzzle_1, Solution),
       write_Puzzle(Puzzle_1),
      %  close(Stream),
        true
        .

solve_Puzzle(Puzzle,Solution):-
    nth0(0,Puzzle,Line),
    nth0(0,Line,Tile),

    %illegalTurn(Tile),
    %not(notCrowded(Tile)),
    unnamed(Puzzle),
    borders(Puzzle),
    single_tile_rules(Puzzle).

single_tile_rules([]).
single_tile_rules([Line1|Puzzle_rest]):-
    maplist(illegalTurn,Line1),
    %maplist(validTile,Line1),

    single_tile_rules(Puzzle_rest)
    .

unnamed(Puzzle):-
    [Line1 | Puzzle_rest] = Puzzle,
    unnamed_down(Line1,Puzzle_rest).


unnamed_down([]).

unnamed_down([],[]).

unnamed_down(Tile).





unnamed_down_tile([],[]).

unnamed_down_tile([Tile1|Line1],[Tile2|Line2]):-
    get_elements_from_tile(Tile1,[Type1,Left1,Down1,Up1,Right1]),
    get_elements_from_tile(Tile2,[Type2,Left2,Down2,Up2,Right2]),
    Down1 = Up2,

    unnamed_down_tile(Line1,Line2).

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

illegalTurn(Tile):-
    get_elements_from_tile(Tile,[Type,Left,Down,Up,Right]),


    (Type = "o" ->
    Left = Right,
    Up = Down,
    dif(Left,Up)
    ;
    true
    ),

    (Type = "*" ->
      dif(Left,Right),
      dif(Up,Down)


    ;
    true
    ).


validTile(Tile):-
    get_elements_from_tile(Tile,[Type,Left,Down,Up,Right]),
    Left == true; Left == false,
       Right == true; Right == false,
       Down == true; Down == false,
       Up == true; Up == false,
    (
        [Left,Right,Down,Up] == [true,true,false,false]
        ;
        [Left,Right,Down,Up] == [true,false,true,false]
        ;
        [Left,Right,Down,Up] == [true,false,false,true]
        ;
        [Left,Right,Down,Up] == [false,true,true,false]
        ;
        [Left,Right,Down,Up] == [false,true,true,false]
        ;
        [Left,Right,Down,Up] == [false,false,true,true]

        )
    .


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

writePuzzleDone(Puzzle):-
    maplist(writeLineDone,Puzzle).
writeLineDone(Line):-
    mapList(writeTileDone,Line),
    writeLn('').
writeTileDone(['*'|_]):-
    write('┼').
 writeTileDone(['o',true,false,false,true|_]):-
    write('╨').
 writeTileDone(['o',false,true,true,false|_]):-
    write('╡').
writeTileDone(['e',true,true,false,false|_]):-
    write('┐').
writeTileDone(['e',true,false,true,false|_]):-
    write('┘').
writeTileDone(['e',true,false,false,true|_]):-
    write('─').
writeTileDone(['e',false,true,true,false|_]):-
    write('│').
writeTileDone(['e',false,false,false,false|_]):-
    write(' ').

connect_Puzzles(Line1,Line2,[]):-
    writeln("d"),
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
    connect_Tile(Tile1,Tile3,Tile,[],Tile2)
   .
connect_Line([Tile1|Line1], [Tile2, Tile222|Line2], [Tile3|Line3],Tile22):-


    connect_Tile(Tile1,Tile3,Tile22,Tile222,Tile2),


    append([Tile222],Line2,Line_2),

    (Line2 == [] ->
    connect_Lines(Line1,Line_2,Line3,Tile2)
    ;
     connect_Line(Line1,Line_2,Line3,Tile2)
    )
    .

connect_Tile(Tile_up,Tile_down,Tile_Left,Tile_right,Tile_Main):-

    Tile_Main = [_,_,_,_,_,Tile_Left,Tile_down,Tile_up,Tile_right,_].
