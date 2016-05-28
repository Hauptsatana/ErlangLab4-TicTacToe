-module(ttt_room).
-behaviour(gen_server).

%% Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([zero_list/1, take_elems/4]).
-export([start_game/4, winning_line/4, full_field/1, check_field/5]).

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

% Запуск новой игры
start_game(Player1, Player2, Dimension, WinStreak) ->
    RoomID = list_to_atom("Room" ++ integer_to_list(erlang:system_time())),
    gen_server:start_link({global, RoomID}, ?MODULE, [RoomID, [Player1, Player2], Dimension, WinStreak], []),
    RoomID.

% Запуск новой игры
init([RoomID, [Player1, Player2], Dimension, WinStreak]) ->
    {ok, [RoomID, [Player1, Player2], Dimension, WinStreak, zero_list(Dimension*Dimension), 1]}.

terminate(normal, [RoomID]) -> 
    ttt_server:end_game(RoomID),
    {ok}.

% Запрос информации о поле
handle_call({field_info}, _From, [RoomID, Players, Dimension, WinStreak, Field, CurTurn]) ->
    {reply, {Field, Dimension}, [RoomID, Players, Dimension, WinStreak, Field, CurTurn]};
    
handle_call({make_turn, PlayerID, Row, Column}, _From, [RoomID, Players, Dimension, WinStreak, Field, CurTurn]) ->
    % Игрок ходит по очереди
    NextTurn = (CurTurn rem 2) + 1,
    CurentPlayer = lists:nth(CurTurn, Players),  
    AnotherPlayer = lists:nth(NextTurn, Players),
    % io:format("!!! NextTurn = ~w, AnotherPlayer = ~w~n", [NextTurn, AnotherPlayer]),
    case lists:nth(CurTurn, Players) of
        PlayerID ->
            Cell = (Row - 1) * Dimension + Column,
            % Клетку на поле можно занять 
            case lists:nth(Cell, Field) of
                0 -> 
                    ttt_player:notify(AnotherPlayer, {opponents_turn, Row, Column}),  % Оповестить противника о сделанном ходе
                    NewField = lists:sublist(Field, Cell - 1) ++ [CurTurn] ++ lists:sublist(Field, Cell + 1, Dimension * Dimension),
                    %io:format("It's 0~n"),
                    case (check_field(Row, Column, NewField, Dimension, WinStreak)) of
                        {winner} ->
                            %io:format("winner~n");
                            ttt_player:notify(AnotherPlayer, {loser}),
                            {stop, normal, {winner}, [RoomID]};
                        {draw} -> 
                            %io:format("draw~n");
                            %ttt_server:end_game(self(), {draw}),
                            ttt_player:notify(AnotherPlayer, {draw}),
                            {stop, normal, {draw}, [RoomID]};
                        {next_turn} ->
                            ttt_player:notify(AnotherPlayer, {make_turn}),
                            {reply, {opponents_turn}, [RoomID, Players, Dimension, WinStreak, NewField, NextTurn]} 
                    end;
                % Клетка уже занята
                1 ->
                    %io:format("cell marked~n"),
                    %ttt_player:notify(PlayerPID, {cell_marked}),
                    {reply, {cell_marked}, [RoomID, Players, Dimension, WinStreak, Field, CurTurn]};
                2 -> 
                    %io:format("cell marked~n"),
                    %ttt_player:notify(PlayerPID, {cell_marked}),
                    {reply, {cell_marked}, [RoomID, Players, Dimension, WinStreak, Field, CurTurn]}
            end;
        CurentPlayer ->
            %io:format("wrong turn~n"), 
            %ttt_player:notify(PlayerPID, {wrong_turn}),
            {reply, {wrong_turn}, [RoomID, Players, Dimension, WinStreak, Field, CurTurn]}
    end.

handle_cast(_Request, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

code_change(_OldCode, State, _Extra) -> {ok, State}.

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