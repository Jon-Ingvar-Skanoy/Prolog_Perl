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
        readLines(Stream),
        close(Stream)
    ).

readLines(Stream) :-
    read_line_to_string(Stream, Line),
    Line \= end_of_file,
    processLine(Line),
    readLines(Stream).
readLines(_).

% Process each line, check if it's a dimension string, and create a list if it is
processLine(Line) :-
    (is_dimension_string(Line) ->
        extract_dimensions(Line, Dimensions),
        create_list(Dimensions, List),
        writeln(List)
    ;
        writeln(Line)  % Else, print the line as is.
    ).

% Check if the line is a dimension string (e.g., "size 7x5")
is_dimension_string(Line) :-
    sub_string(Line, 0, _, _, "size ").

% Extract dimensions from the line
extract_dimensions(Line, Dimensions) :-
    sub_string(Line, _, _, 0, Dimensions).

% Parses the dimension string and generates the list
create_list(DimensionString, List) :-
    split_string(DimensionString, "x", "", [W, H]),
    number_string(Width, W),
    number_string(Height, H),
    generate_2d_list(Width, Height, List).

% Generates a 2D list of Width x Height
generate_2d_list(Width, Height, List) :-
    findall(Row, (between(1, Height, _), length(Row, Width)), List).



