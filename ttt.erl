
%% Реализовать крестики-нолики

-module(ttt).
-export([start_server/0, loop_server/2, new_player/0, loop_player/1, start_game/1, wait_room/2, loop_room/4, 
    make_turn/3, winning_line/4, full_field/1, check_field/5, game_field/1, take_elems/4, zero_list/1, mess/2, print_field/2]).


mess(Debug, Message) ->
    case Debug of
        true -> io:format("~s~n", [Message])
    end.

% Взять элементы из списка от Start до Finish с шагом Delta 
take_elems(List, Start, Delta, Finish) ->
    if  (Start > Finish) -> [];
        (Start < 1) -> [];
        true -> [lists:nth(Start, List) | take_elems(List, Start + Delta, Delta, Finish)]
    end. 

% Создание пустого списка из N элементов
zero_list(N) ->
    if  (N < 1) -> [];
        true -> [0 | zero_list(N - 1)]
    end.    

% Распечатка игрового поля
print_field(Field, Dimension) -> print_field(Field, Dimension, Dimension).
print_field(Field, Dimension, 0) -> 
    io:format("~n"),
    case Field of
        [_ | _] -> print_field(Field, Dimension, Dimension);
        [] -> io:format("~n")
    end;
print_field([H | T], Dimension, Left) -> 
    case H of
        1 -> Char = "X";
        2 -> Char = "O";
        0 -> Char = "_"
    end,
    io:format("~s", [Char]),
    print_field(T, Dimension, Left - 1). 

% Новый игрок на сервере
new_player() -> spawn(?MODULE, loop_player, [true]).

% Процесс игрока
loop_player(Debug) ->
    mess(Debug, "Loop player"),
    MyPID = self(),
    receive 
        % Запуск новой игры
        {start_game} -> 
            global:send(game_server, {start_game, MyPID}),
            receive 
                {game_already_started} -> io:format("You've already started your game. You'd rather lead yourselft to victory.~n");
                {new_room_created} ->
                    io:format("New game (5x5, streak of 3) created. Waiting for the second player...~n"),
                    receive 
                        {second_player_joined} -> io:format("Second player joined.~nYou play X and the game starts now!~n")
                    end;
                {room_found} ->
                    io:format("Existing game found. You play O and the game starts now!~n"),
                    % Ожидание действий первого игрока
                    io:format("Waiting for the first player to make his first turn...~n"),
                    receive
                        {opponent, Row1, Column1} -> io:format("Your opponent marked (~B;~B)~n", [Row1, Column1])
                    end,
                    receive
                        {loser} -> io:format("You've lost this game...~n");
                        {draw} -> io:format("It's a draw(((~n");
                        {next_turn} -> io:format("Make your turn now:~n")
                    end 
            end;
        % Просмотр игрового поля
        {game_field} -> 
            global:send(game_server, {game_field, MyPID}),
            receive 
                {no_room} -> io:format("You have to start your game first.~n");
                {Field, Dimension} -> print_field(Field, Dimension)
            end;
        % Игрок делает ход
        {turn, Row, Column} -> 
            global:send(game_server, {turn, MyPID, Row, Column}),
            receive 
                {no_room} -> io:format("You have to start your game first.~n");
                {wrong_turn} -> io:format("It's not your turn now!~n");
                {cell_marked} -> io:format("Cell already marked~n"); 
                {wait_turn} ->
                    io:format("Nice turn! Waiting for opponent...~n"),
                    receive
                        {opponent, Row2, Column2} -> io:format("Your opponent marked (~B;~B)~n", [Row2, Column2])
                        % {X, Y, Z} -> io:format("Received ~w ~w ~w~n", [X, Y, Z])
                    end,
                    receive
                        {loser} -> io:format("You've LOST this game...~n");
                        {draw} -> io:format("It's a DRAW(((~n");
                        {next_turn} -> io:format("Make your turn now:~n")
                    end;
                {winner} -> io:format("You're the WINNER!!!~n");
                {draw} -> io:format("It's a DRAW(((~n")
            end
    end,
    loop_player(Debug).

% Команды игрока
start_game(PlayerPID) -> PlayerPID ! {start_game}.
game_field(PlayerPID) -> PlayerPID ! {game_field}.
make_turn(PlayerPID, Row, Column) -> PlayerPID ! {turn, Row, Column}.  

% Запуск игрового сервера
start_server() ->
    global:register_name(game_server, spawn(?MODULE, loop_server, [[], false])).  

% Цикл работы сервера
loop_server(Rooms, Debug) ->
    mess(Debug, "Loop Server"),
    receive 
        % Запуск новой игры
        {start_game, PlayerPID} ->
            % Занят ли игрок?
            case lists:filter(fun({_, Player1, Player2}) -> (Player1 =:= PlayerPID) or (Player2 =:= PlayerPID) end, Rooms) of
                [] ->
                    mess(Debug, "Starting new game"),
                    % Поиск созданных комнат
                    WaitingRooms = lists:filter(
                        fun({_, _, Player2}) -> Player2 =:= null end,
                        Rooms
                    ),
                    case WaitingRooms of 
                        [] -> 
                            % Создание новой комнаты
                            NewRoom = create_room(5),
                            PlayerPID ! {new_room_created},
                            NewRooms = Rooms ++ [{NewRoom, PlayerPID, null}];
                        [{Room, Player1, _} | _] ->
                            % Подключение к текущей
                            Player1 ! {second_player_joined},
                            PlayerPID ! {room_found},
                            NewRooms = lists:keyreplace(Room, 1, Rooms, {Room, Player1, PlayerPID}),
                            io:format("P1 = ~w, P2 = ~w.~n", [Player1, PlayerPID]),
                            Room ! {connect}
                    end;
                [_| _] -> 
                    mess(Debug, "Game already started"),
                    PlayerPID ! {game_already_started},
                    NewRooms = Rooms
            end;
        % Запрос состояния игрового поля
        {game_field, PlayerPID} -> 
            RoomState = lists:filter(fun({_, Player1, Player2}) -> (Player1 =:= PlayerPID) or (Player2 =:= PlayerPID) or (Player2 =:= null) end, Rooms),
            case RoomState of
                % Игрок находится вне комнаты
                [] -> 
                    PlayerPID ! {no_room};
                % Комната найдена
                [{Room, _, _} | _] ->
                    Room ! {game_field},
                    receive
                        {Field, Dimension} -> PlayerPID ! {Field, Dimension}
                    end
            end,
            NewRooms = Rooms;
        % Игрок делает ход
        {turn, PlayerPID, Row, Column} ->
            RoomState = lists:filter(fun({_, Player1, Player2}) -> (Player1 =:= PlayerPID) or (Player2 =:= PlayerPID) or (Player2 =:= null) end, Rooms),
            case RoomState of
                % Игрок находится вне комнаты
                [] ->
                    mess(Debug, "No Room"), 
                    PlayerPID ! {no_room},
                    NewRooms = Rooms;
                % Комната найдена
                [{Room, Player1, Player2} | _] ->
                    case PlayerPID of
                        Player1 -> Turn = 1, OtherPlayer = Player2;
                        Player2 -> Turn = 2, OtherPlayer = Player1
                    end,
                    Room ! {turn, Turn, Row, Column},
                    receive
                        {wrong_turn} ->
                            PlayerPID ! {wrong_turn},
                            NewRooms = Rooms;
                        {cell_marked} -> 
                            PlayerPID ! {cell_marked},
                            NewRooms = Rooms;
                        % Ответ в случае корректного хода
                        {ValidStatus} ->
                            OtherPlayer ! {opponent, Row, Column},
                            case ValidStatus of
                                winner ->
                                    PlayerPID ! {winner},
                                    OtherPlayer ! {loser},
                                    % Удаление комнаты
                                    NewRooms = lists:keydelete(Room, 1, Rooms);
                                draw ->
                                    PlayerPID ! {draw},
                                    OtherPlayer ! {draw},
                                    NewRooms = lists:keydelete(Room, 1, Rooms);
                                next_turn ->
                                    OtherPlayer ! {next_turn},
                                    PlayerPID ! {wait_turn},
                                    NewRooms = Rooms
                            end
                    end
            end            
    end,
    loop_server(NewRooms, Debug).

% Создать игровую комнату с указанным размером поля. Первый игрок - X, второй - O
create_room(Dimension) ->
    spawn(?MODULE, wait_room, [Dimension, 3]).

%% Комната ожидает второго игрока
wait_room(Dimension, WinStreak) ->
    receive 
        {connect} -> loop_room(1, zero_list(Dimension * Dimension), Dimension, WinStreak)
    end.

% Процесс игровой комнаты
loop_room(CurTurn, Field, Dimension, WinStreak) ->
    %io:format("CurTurn = ~w~n", [CurTurn]),
    receive
        {turn, Turn, Row, Column} ->
            % io:format("Got turn~n"),
            % Ходит правильный игрок
            if  (Turn =:= CurTurn) ->
                    Cell = (Row - 1) * Dimension + Column,
                    % Клетку на поле можно занять 
                    case lists:nth(Cell, Field) of
                        0 -> 
                            NewField = lists:sublist(Field, Cell - 1) ++ [Turn] ++ lists:sublist(Field, Cell + 1, Dimension * Dimension),
                            %io:format("It's 0~n"),
                            case (check_field(Row, Column, NewField, Dimension, WinStreak)) of
                                {winner} ->
                                    %io:format("winner~n"); 
                                    global:send(game_server, {winner});
                                {draw} -> 
                                    %io:format("draw~n");
                                    global:send(game_server, {draw});
                                {next_turn} -> 
                                    global:send(game_server, {next_turn}),
                                    %io:format("next turn~n"),
                                    loop_room(CurTurn rem 2 + 1, NewField, Dimension, WinStreak)
                            end;
                        % Клетка уже занята
                        1 ->
                            %io:format("cell marked~n"),
                            global:send(game_server, {cell_marked}),
                            loop_room(CurTurn, Field, Dimension, WinStreak);
                        2 -> 
                            %io:format("cell marked~n"),
                            global:send(game_server, {cell_marked}),
                            loop_room(CurTurn, Field, Dimension, WinStreak)
                    end;
                true ->
                    %io:format("wrong turn~n"), 
                    global:send(game_server, {wrong_turn}),
                    loop_room(CurTurn, Field, Dimension, WinStreak)
            end;
        {game_field} ->
            %io:format("game field: ~w~n", [Field]),
            global:send(game_server, {Field, Dimension}),
            loop_room(CurTurn, Field, Dimension, WinStreak)
    end.

% Проверка отдельного направления на выигрыш
winning_line([], _, _, _) -> false; 
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
    Start1 = max((Row - 1) * Dimension + 1, Cell - WinStreak + 1),
    Finish1 = min(Row * Dimension, Cell + WinStreak - 1),
    Line1 = take_elems(Field, Start1, 1, Finish1),
    case winning_line(Line1, WinStreak, 0, 0) of
        true -> {winner};
        false ->
            % Проверка вертикальной линии
            Start2 = max(Cell - (WinStreak - 1) * Dimension, Column),
            Finish2 = erlang:min(Cell + (WinStreak - 1) * Dimension, Dimension * Dimension - (Dimension - Column)),
            Line2 = take_elems(Field, Start2, Dimension, Finish2),
            case winning_line(Line2, WinStreak, 0, 0) of
                true -> {winner};
                false ->
                    % Проверка главной диагонали
                    DeltaTop3 = min(Row - 1, Column - 1),
                    Start3 = max(Cell - (WinStreak - 1) * (Dimension + 1), Cell - DeltaTop3 * (Dimension + 1)),
                    DeltaBottom3 = min(Dimension - Row, Dimension - Column),
                    Finish3 = min(Cell + (WinStreak - 1) * (Dimension + 1), Cell + DeltaBottom3 * (Dimension + 1)),
                    Line3 = take_elems(Field, Start3, Dimension + 1, Finish3),
                    case winning_line(Line3, WinStreak, 0, 0) of
                        true -> {winner};
                        false ->
                            % Побочная диагональ
                            DeltaTop4 = min(Dimension - Column, Row - 1),
                            Start4 = max(Cell - (WinStreak - 1) * (Dimension - 1), Cell - DeltaTop4 * (Dimension - 1)),
                            DeltaBottom4 = min(Column - 1, Dimension - Row),
                            Finish4 = min(Cell + (WinStreak - 1) * (Dimension - 1), Cell + DeltaBottom4* (Dimension - 1)),
                            Line4 = take_elems(Field, Start4, Dimension - 1, Finish4),
                            case winning_line(Line4, WinStreak, 0, 0) of 
                                true -> {winner};
                                false -> 
                                    case (full_field(Field)) of
                                        true -> {draw};
                                        false -> {next_turn}
                                    end
                            end
                    end
            end
    end.
    