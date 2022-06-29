-module(learn_erlang).
-export([hello/0, average/2]).

% Prints "Hello, World!"
hello() -> io:format("Hello, World!~n"). % ~n is newline

% Returns the average of two numbers
average(Number1, Number2) -> (Number1+Number2) / 2. % / is division

% Calculates the slopes of a line given two points.
slope(X1, Y1, X2, Y2) ->    % (X1, Y1), (X2, Y2) are points
    DeltaX = X2 - X1,       % DeltaX is the difference between X1 and X2
    DeltaY = Y2 - Y1,       % DeltaY is the difference between Y1 and Y2
    DeltaY / DeltaX.        % DeltaY / DeltaX is the slope of the line

% Adds N number of numbers together. Different arities are supported.
add(N1, N2) -> N1 + N2. % Add two numbers.
add(N1, N2, N3) -> N1 + N2 + N3.    % Add three numbers.
add(N1, N2, N3, N4) -> N1 + N2 + N3 + N4.   % Add four numbers.

% Divide two numbers, with two clauses, one for regualar division, and one for division by zero.
divide(_N1, 0) -> 0; % Division by zero is not allowed.
divide(N1, N2) -> N1 / N2. % Regular division.

% Demonstration of the 'when' keyword, also known as a guard, or 'case statement' in other languages.
% The 'when' keyword is used to specify a clause that will be executed if a condition is true.
letter_grade(Grade) when Grade >= 90 -> "A";    % If the grade is greater than or equal to 90, return "A".
letter_grade(Grade) when Grade >= 80 -> "B";    % If the grade is greater than or equal to 80, return "B".
letter_grade(Grade) when Grade >= 70 -> "C";    % If the grade is greater than or equal to 70, return "C".
letter_grade(Grade) when Grade >= 60 -> "D";    % If the grade is greater than or equal to 60, return "D".
letter_grade(Grade) -> "F". % If none of the above conditions are true, return "F".

% Demonstration of recursion using a factorial function.
factorial(N) when N =< 1 -> 1; % If N is less than or equal to 1, return 1.
factorial(N) -> N * factorial(N-1). % Otherwise, return N * factorial(N-1).

% Demostration of tail recursion.
factorial(N) -> factorial_tail(N, 1). % factorial_tail(N, 1) is the base case.
factorial(0, Result) -> Result; % factorial_tail(0, Result) is the recursive case.
factorial(N, Result) -> factorial(N-1, Result * N). % factorial_tail(N-1, Result * N) is the recursive case.

% Demonstration of lambda functions and anonymous functions.
% Lambda functions are functions that are defined without a name.
% Anonymous functions are functions that are defined without a name and are used in a function call.
encrypt(Value, Cipher_Function) -> Cipher_Function(Value). % Cipher_Function(Value) is the function call.

test_encrypt() ->   % Test the encrypt function.
    Cipher1 = fun (X) X + 1 end, % Cipher1 is a lambda function.
    Cipher2 = fun (X) (X * 2) - 3 end, % Cipher2 is a lambda function.
    11 = encrypt(10, Cipher1), % 11 = Cipher1(10) = 10 + 1 = 11.
    17 = encrypt(10, Cipher2), % 17 = Cipher2(10) = (10 * 2) - 3 = 17.
    3.0 encrypt(1000, fun math:log10/1), % 3.0 = math:log10(1000) = math:log10(1000) / 1 = 3.0.
    ok.

% Demonstration of lists.
playing_with_lists() ->
   L1 = [1, 2, 3, 4],
   ok.

% Demonstration of pattern matching and prepending.
playing_with_lists() ->
    L1 = [1, 2, 3], % L1 is a list with three elements.
    L2 = [0|L1], % Prepend 0 to L1.
    [0, 1, 2, 3] = L2, % Pattern matching on L2.
    ok.

% Demonstration of further pattern matching.
playing_with_lists() ->
    L1 = [1, 2, 3, 4], % L1 is a list with four elements.
    [A | _] = L1, % A is the first element of L1.
    1 = A, % Pattern matching on A.

    L2 = [2, 4, 6, 8], % L2 is a list with four elements.
    [B, C | _] = L2, % B is the first element of L2, and C is the second element of L2.
    2 = B, % Pattern matching on B.
    4 = C, % Pattern matching on C.
    ok.

% Demonstration of using pattern matching to print the first element of a list.
display_first([]) ->
    io:format("No elements in the list.~n"); % Print "No elements in the list."
display_first([First|_Rest]) -> % Display the first element of a list.
    io:format("First: ~p~n", [First]).  % ~p is a placeholder for a list.

% Driver function for display_first.
playing_with_lists() ->
    L1 = [1, 2, 3, 4], % L1 is a list with four elements.
    display_first(L1), % Display the first element of L1.
    display_first([]), % Display the first element of an empty list.
    ok.

% Display all of the elements of the list.
display_all([]) ->
    io:format("End of the list.~n"); % Print "End of the list."
display_all(First|Rest) -> % Display the first element of a list.
    io:format("~p ", [First]), % ~p is a placeholder for a list.
    display_all(Rest). % Display the rest of the list.

% Driver function for display_all.
playing_with_lists() ->
    L1 = [1, 2, 3, 4], % L1 is a list with four elements.
    display_all(L1), % Display all of the elements of L1.
    display_all([]), % Display all of the elements of an empty list.
    ok.

% Demonstration of list comprehension using 'lists:seq'.
playing_with_lists() ->
    L1 = lists:seq(1, 10), % L1 is a list of the numbers 1 through 10.
    [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] = L1, % Pattern matching on L1.

    L2 = [N || N <- lists:seq(1, 10)], % L2 is a list of the numbers 1 through 10.
    [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] = L2, % Pattern matching on L2.

    L3 = [N * 3 || N <- lists:seq(1, 10)], % L3 is a list of the numbers 1 through 10 multiplied by 3.
    [3, 6, 9, 12, 15, 18, 21, 24, 27, 30] = L3, % Pattern matching on L3.

    L4 = [N * 3 || N <- lists:seq(1, 10), N rem 2 == 0], % L4 is a list of the numbers 1 through 10 multiplied by 3, but only those that are even.
    [6, 12, 18, 24, 30] = L4, % Pattern matching on L4.

    ok.