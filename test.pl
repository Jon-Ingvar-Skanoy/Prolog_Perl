inputFile('unsolved.txt').
outputFile('solved.txt').

puzzle(rows).
dead(zombie).
dead(creeper).
dead(skeleton).
dead(god).
dead([rome,sparta,persia,byzantium]).
add(A,B,A+B).

readFile(FileName) :-
    setup_call_cleanup(
        open(FileName, read, Stream),
        readLines(Stream, not_in_list, [], 0, 0),
        close(Stream)
    ).

readLines(Stream, in_list, AccumulatedList, Width, Height) :-
    length(AccumulatedList, CurrentHeight),
    (CurrentHeight < Height ->
        read_line_to_string(Stream, Line),
        Line \= end_of_file,
        processLine(Line, Stream, AccumulatedList, UpdatedList, Width),
        readLines(Stream, in_list, UpdatedList, Width, Height)
    ;
        writeln(AccumulatedList),
        (CurrentHeight =:= Height -> readLines(Stream, not_in_list, [], 0, 0) ; true)
    ).


readLines(Stream, not_in_list, _, _, _) :-
    read_line_to_string(Stream, Line),
    (Line == end_of_file -> true ;
     (is_dimension_string(Line) ->
        extract_dimensions(Line, Dimensions),
        split_string(Dimensions, "x", "", [W, H]),
        number_string(Width, W),
        number_string(Height, H),
        readLines(Stream, in_list, [], Width, Height)
      ;
        writeln(Line),
        readLines(Stream, not_in_list, [], 0, 0)
     )
    ).

processLine(Line, _, AccumulatedList, UpdatedList, Width) :-
    split_string(Line, " ", "", SplitLine),
    maplist(atom_string, Atoms, SplitLine),
    length(Atoms, LineWidth),
    (LineWidth =:= Width ->
        append(AccumulatedList, [Atoms], UpdatedList)
    ;
        writeln('Line does not match the expected width.'),
        UpdatedList = AccumulatedList
    ).

is_dimension_string(Line) :-
    sub_string(Line, 0, _, _, "size ").

extract_dimensions(Line, Dimensions) :-
    sub_string(Line, _, _, 0, Dimensions).







