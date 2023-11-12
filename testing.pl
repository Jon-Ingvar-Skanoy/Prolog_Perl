

read_File(Infile):-
        open(Infile, read, Stream),
        read_lines_find_size(Stream, not_in_list, Puzzles),
        close(Stream).

read_lines_find_size(Stream,not_in_list, Puzzles):-
    read_line_to_string(Stream,Line),
    (Line == end_of_file -> true ;
    (is_new_puzzle_line(Line) ->
    get_size(Line,Size),
    split_string(Size, "x","", [W,H]),
    number_string(Width, W),
    number_string(Height, H),
    writeln(Width),
    writeln(Height),
    read_lines_to_list(Stream, in_list, Width,Height, [],  Puzzles)
    ;
    read_lines_find_size(Stream, not_in_list, Puzzles)
    ),

    true

    ).
read_lines_to_list(Stream, in_list, Width,Height, Creation_list,Puzzles):-
    read_line_to_string(Stream, Line),
    writeln(Line),
    (Line == end_of_file -> true ;
    read_lines_find_size(Stream, not_in_list, Puzzles)
    ).


is_new_puzzle_line(Line):-
    sub_string(Line, 0 , _, _, "size ").

get_size(Line,Size):-
    sub_string(Line, 5, _, 0, Size).