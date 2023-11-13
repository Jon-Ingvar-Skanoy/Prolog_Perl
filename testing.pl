

read_File(Infile):-
        open(Infile, read, Stream),
        read_lines_find_size(Stream, not_in_list, [], Puzzles_out),
        nth0(0,Puzzles_out,Puzzle_1),
        solve_Puzzle(Puzzle_1, Solution),
        close(Stream).

solve_Puzzle(Puzzle,Solution):-
    nth0(0,Puzzle,Line),
    nth0(0,Line,Tile),

    illegalTurn(Tile),
    not(notCrowded(Tile)).


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

extended_element(Element, [Element, _, _, _, _]).

is_new_puzzle_line(Line):-
    sub_string(Line, 0 , _, _, "size ").

get_size(Line,Size):-
    sub_string(Line, 5, _, 0, Size).

get_elements_from_tile(Tile,[Type,Left,Down,Up,Right]):-
    Tile = [Type,Left,Down,Up,Right].

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
      dif(Up,Down),

      writeln(Tile)
    ;
    true
    ).


notCrowded(Tile):-
    get_elements_from_tile(Tile,[Type,Left,Down,Up,Right]),
    Left = true,
    Right = true,
    Down = false,
    Up = false,

    (
    [Left,Right,Down,Up] == [true,true,true,true]
    ;
    [Left,Right,Down,Up] == [true,true,true,false]
    ;
    [Left,Right,Down,Up] == [true,true,false,true]
    ;
    [Left,Right,Down,Up] == [false,false,true,true]
    ;
    [Left,Right,Down,Up] == [false,false,false,true]
    ;
    [Left,Right,Down,Up] == [false,false,true,false]
    ;
    [Left,Right,Down,Up] == [false,true,false,false]
    ;
    [Left,Right,Down,Up] == [true,false,false,false]
    ),
    writeln(Tile)

    .

