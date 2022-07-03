-module(syntax_sampler).
-export([main/0]).


% Main Driver Function.
main() ->
    display(),
    initialize_variable(),
    initialize_atom(),
    guard_demo(5),
    io:format("~nN is ~p.~n", [recurse(5)]),
    say_hello(italian),
    list(20),
    pattern_matching(1, 2),
    map_list(5),
    filter_list(100),
    foldl_list(1000).


% Display output to console.
display() -> io:format("~nThis displays to the console.~n").

% This function initializes two variables and adds them.
initialize_variable() ->
    X = 1,      % Initialize variable X.
    Y = 2,      % Initialize variable Y.
    Z = X + Y,  % Add X and Y.
    io:format("~n  X: ~p~n+ Y: ~p~n------~n  Z: ~p~n", [X, Y, Z]).  % Display X, Y, and Z.

% This function initializes an atom.
initialize_atom() ->
    io:format("~n~p~n", ['Hello World']).   % Print the atom.

% Guard in function parameters.
guard_demo(X) when X /= 1 ->                % X is not 1.
    io:format("~nX is ~p, not 1.~n", [X]).  % X is not 1.

% Recursion.
recurse(N) when N =< 1 -> 1;        % Base case.
recurse(N) -> N * recurse(N - 1).   % Recursive case.

% This function takes an atom as a parameter and prints
% a greeting based on which language atom is passed in.
say_hello(X) -> % X is an atom.
    case X of   
        french -> io:format("~nBonjour~n");                 % X is french.
        english -> io:format("~nHello~n");                  % X is english.
        german -> io:format("~nHallo~n");                   % X is german.
        italian -> io:format("~nCiao~n");                   % X is italian.
        japanese -> io:format("~nKonichiwa~n");             % X is japanese.
        spanish -> io:format("~nHola~n");                   % X is spanish.
        mandarin -> io:format("~nNi Hao~n");                % X is mandarin.
        afrikaans -> io:format("~nHallo~n");                % X is afrikaans.
        arabic -> io:format("~nMarhaba~n");                 % X is arabic.
        bulgarian -> io:format("~nZdravstvuyte~n");         % X is bulgarian.
        chinese -> io:format("~nNi Hao~n");                 % X is chinese.
        czech -> io:format("~nAhoj~n");                     % X is czech.
        danish -> io:format("~nHej~n");                     % X is danish.
        dutch -> io:format("~nHallo~n");                    % X is dutch.
        estonian -> io:format("~nTere~n");                  % X is estonian.
        finnish -> io:format("~nHei~n");                    % X is finnish.
        french_canadian -> io:format("~nBonjour~n");        % X is french_canadian.
        greek -> io:format("~nEllo~n");                     % X is greek.
        hebrew -> io:format("~nShalom~n");                  % X is hebrew.
        hindi -> io:format("~nNamaskar~n");                 % X is hindi.
        hungarian -> io:format("~nSzia~n");                 % X is hungarian.
        swedish -> io:format("~nHej~n");                    % X is swedish.
        icelandic -> io:format("~nHalo~n");                 % X is icelandic.
        indonesian -> io:format("~nHalo~n");                % X is indonesian.
        irish -> io:format("~nDia dhuit~n");                % X is irish.
        portuguese -> io:format("~nOlá~n");                 % X is portuguese.
        _ -> io:format("~nI don't know that language~n")    % X is not a language.
    end.

% Demonstration of lists.
list(X) ->                              % X is the number of elements in the list.
    L1 = lists:seq(1, X),               % Create a list from 1 to X.
    io:format("~nList: ~p~n", [L1]).    % Print the list.

% Pattern matching in function parameters.
pattern_matching(1 = X, 2 = Y) when {X, Y} =:= {1, 2} ->    % Pattern matching.
    io:format("~nX is 1 and Y is 2.~n").                    % Output.

% Example of using lists:map in a lambda expression.
map_list(N) ->                                  % N is the number of elements in the list.
    L1 = lists:seq(1, N),                       % Create a list of numbers from 1 to N.
    L2 = lists:map(fun(X) -> X * 2 end, L1),    % Multiply each element by 2.
    io:format("~nDoubled List: ~p~n", [L2]).    % Print the list.

% Example of using lists:filter in a lambda expression.
filter_list(N) ->                                       % N is the number of elements in the list.
    L1 = lists:seq(1, N),                               % Create a list of numbers from 1 to N.
    L2 = lists:filter(fun(X) -> X rem 2 =:= 0 end, L1), % Filter out all odd numbers.
    io:format("~nFiltered List: ~p~n", [L2]).           % Print the list.

% Example of using lists:foldl in a lambda expression.
foldl_list(N) ->                                        % N is the number of elements in the list.
    L1 = lists:seq(1, N),                               % Create a list of numbers from 1 to N.
    L2 = lists:foldl(fun(X, Y) -> X + Y end, 0, L1),    % Add all elements in the list.
    io:format("~nSum of List: ~p~n", [L2]).             % Print the list.