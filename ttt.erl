
%% Реализовать крестики-нолики

-module(ttt).
-export([start_server/0]).

take_elems(List, Start, Seed, Finish) ->
    if  (Start > Max) -> [];
        (Start < 1) -> [];
        true -> [lists:nth(Start) | take_elems(List, Start + Seed, Max)]
    end. 

zero_list(Dimension) ->
    if  (Dimension < 1) -> [];
        true -> [0 | zero_list(Dimension - 1)]
    end.    


start_server() ->
    spawn(?MODULE, loop_server, [[]]).  
    
loop_server(Rooms) ->
    receive 
        {start_game, UserPID} ->
    end.

% Создать игровую комнату с указанным размером поля
create_room(Dimension) ->
    spawn(?MODULE, wait_room, [Dimension]).

%% Комната ожидает второго игрока
wait_room(Dimension, Streak) ->
    receive 
        {connect} -> play_room(1, zero_list(Dimension * Dimension), Dimension)
    end.

play_room(Turn, Field, Dimension, WinStreak) ->
    receive
        {turn, Turn, Row, Column} ->
            Cell = (Row - 1) * Dimension + Column,
            % Клетку на поле можно занять 
            if  (lists:nth(Cell, Field) == 0) -> 
                    NewField = lists:sublist(Field, Cell - 1) ++ [Turn] ++ lists:sublist(Field, Cell + 1, 9),
                    case (check_field(Cell, NewField)) of
                        {winner} -> game_server ! {winner, self(), Turn};
                        {draw} -> game_server ! {draw, self()};
                        {next_turn} -> 
                            game_server ! {next_turn, self()},
                            play_room((Turn - 1) rem 2 + 1, NewField, Dimension, WinStreak)
                    end;
                % Клетка уже занята
                true ->
                    game_server ! {cell_used, self()}
            end;
        {info} ->
            game_server ! {info, Field},
            play_room(Turn, )
    end.

% Проверка отдельного направления на выигрыш
winning_line([[], _, _, _) -> false; 
winning_line([Head | Tail], WinStreak, LastValue, CurStreak) ->
    if  (Head == 0) -> winning_line(Tail, WinStreak, 0, 1);
        (Head == LastValue) -> 
            if  (CurStreak + 1 == WinStreak) -> true;
                true -> winning_line(Tail, WinStreak, LastValue, CurStreak + 1)
            end; 
        true -> winning_line(Tail, WinStreak, Head, 1) 
    end.

% Проверка заполненности игрового поля
full_field([]) -> true;
full_field([Head | Tail]) ->
    if  (Head == 0) -> false;
        true -> full_field(Tail)
    end.
    

% Проверка игрового поля на наличие победителя
check_field(Row, Column, Field, Dimension, WinStreak) ->
    Cell = (Row - 1) * Dimension + Column,
    % Проверка горизонтальной линии
    Start = max((Row - 1) * Dimension + 1, Cell - WinStreak + 1),
    Finish = min(Row * Dimension, Cell + WinStreak - 1),
    Line1 = take_elems(Field, Start, 1, Finish),
    if  (winning_line(Line1, WinStreak, 0, 0)) -> {winner};
        true ->
            % Проверка вертикальной линии
            Start = max(Cell - (WinStreak - 1) * Dimension, Column),
            Finish = min(Cell + (WinStreak - 1) * Dimension, Dimension * Dimension - (Dimension - Column)),
            Line2 = take_elems(Field, Start, Dimension, Finish),
            if  (winning_line(Line2, WinStreak, 0, 0)) -> {winner};
                true ->
                    % Проверка главной диагонали
                    DeltaTop = min(Row - 1, Column - 1),
                    Start = max(Cell - (WinStreak - 1) * (Dimension + 1), Cell - DeltaTop * (Dimension + 1)),
                    DeltaBottom = min(Dimension - Row, Dimension - Column),
                    Finish = min(Cell + (WinStreak - 1) * (Dimension + 1), Cell + DeltaBottom * (Dimension + 1)),
                    Line3 = take_elems(Field, Start, Dimension, Finish),
                    if  (winning_line(Line3, WinStreak, 0, 0)) -> {winner};
                        true ->
                            % Побочная диагональ
                            DeltaTop = min(Dimension - Column, Row - 1),
                            Start = max(Cell - (WinStreak - 1) * (Dimension - 1), Cell - DeltaTop * (Dimension - 1)),
                            DeltaBottom = min(Column - 1, Dimension - Row),
                            Finish = min(Cell + (WinStreak - 1) * (Dimension - 1), Cell + DeltaBottom * (Dimension - 1)),
                            Line4 = take_elems(Field, )
                            if  (winning_line(Line4, WinStreak, 0, 0)) -> {winner};
                                true -> 
                                    case (full_field(Field)) of
                                        true -> {draw};
                                        false -> {next_turn}
                                    end
                            end
                    end
            end
    end.
    