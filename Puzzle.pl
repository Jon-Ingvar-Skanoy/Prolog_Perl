
run:- read_File("unsolved.txt").
read_File(Infile):-
        open(Infile, read, Stream),
        read_lines_find_size(Stream, not_in_list, [], Puzzles_out),
        nth0(0,Puzzles_out,Puzzle_1),
     %   solve_Puzzle(Puzzle_1, Solution),
       % close(Stream),
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
    writeln(Line1),
    single_tile_rules(Puzzle_rest)
    .

unnamed(Puzzle):-
    [Line1 | Puzzle_rest] = Puzzle,
    unnamed_down(Line1,Puzzle_rest).


unnamed_down([]).

unnamed_down([],[]).

unnamed_down(Line1,[Line2|Sublist]):-

    unnamed_down_tile(Line1,Line2),

    length(Sublist,Len),

    (Len == 0 -> true;
    unnamed_down(Line2,Sublist)
    )
    .



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
    read_lines_to_list(Stream, "","", Width,Height, [], Puzzle),

    append(Puzzles_in,[Puzzle], Puzzles_in2),
    read_lines_find_size(Stream, not_in_list, Puzzles_in2,Puzzles_out)
    ;
    read_lines_find_size(Stream, not_in_list, [], Puzzles_out)
    ),

    true

    ).

read_lines_to_lists(Stream, Line1,Line2, Width,Height, Creation_list,Puzzle):-

    Line1 \= "",
    Line2 \= "",
    length(Creation_list,CurrentHeight),

    (CurrentHeight < (Height-1) ->
        read_line_to_string(Stream, Line3),

        process_line(Line1,Line2,Line3,List_Line),

        append(Creation_list,[List_Line],Appended_List),
        (Line == end_of_file -> true ;

        read_lines_to_lists(Stream, Line2,Line3, Width,Height, Appended_List,  Puzzle)
        )
    ;
    process_line(Line1,Line2,[],List_Line),
    append(Creation_list,[List_Line],Appended_List),

    writeln("Puzzle:"),
    writePuzzle(Appended_List),
    Puzzle = Appended_List
    )
    .


read_lines_to_list(Stream, "","", Width,Height, Creation_list,Puzzle):-
    length(Creation_list,CurrentHeight),
        read_line_to_string(Stream, Line2),
        read_line_to_string(Stream, Line3),
        process_line([],Line2,Line3,List_Line),
        append(Creation_list,[List_Line],Appended_List),
        (Line3 == end_of_file -> true ;
        read_lines_to_lists(Stream, Line2,Line3, Width,Height, Appended_List,  Puzzle)
        )
    .

process_line(Line1,Line2,Line3,List):-

    split_string(Line2, " ", "", Split_Line),
    maplist(extended_element, Split_Line,List)


    .
extended_element(Element, [Element, _, _, _, _, _, _, _, _]).
extended_element(Element,Left,Down,Up,Right, [Element, _, _, _, _, Left,Down,Up,Right]).

is_new_puzzle_line(Line):-
    sub_string(Line, 0 , _, _, "size ").

get_size(Line,Size):-
    sub_string(Line, 5, _, 0, Size).

get_elements_from_tile(Tile,[Type,Left,Down,Up,Right]):-
    Tile = [Type,Left,Down,Up,Right,_,_,_,_].

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
       Up == true; Up == false
   % (
    %    [Left,Right,Down,Up] == [true,true,false,false]
     %   ;
      %  [Left,Right,Down,Up] == [true,false,true,false]
      %  ;
      %  [Left,Right,Down,Up] == [true,false,false,true]
      %  ;
      %  [Left,Right,Down,Up] == [false,true,true,false]
      %  ;
      %  [Left,Right,Down,Up] == [false,true,true,false]
      %  ;
       % [Left,Right,Down,Up] == [false,false,true,true]
%
 %       )
    .


process_subList_border([]).
process_subList_border([Sublist|Sublists]):-

    last(Sublist,Tile),
     nth0(0,Sublist,Tile2),
    illegal_Right(Tile),
    illegal_Left(Tile2),

    process_subList_border(Sublists).

writePuzzle(Puzzle):-
    maplist(writeln,Puzzle).


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

