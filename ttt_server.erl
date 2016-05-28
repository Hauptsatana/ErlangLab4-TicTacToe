-module(ttt_server).
-behaviour(gen_server).

%% Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start_server/0, end_game/1]).

% Запуск сервера
start_server() ->
    ServerPID = gen_server:start_link({global, game_server}, ?MODULE, [], []),
    io:format("!!! Server started~n"),
    ServerPID.

% Окончание игры    
end_game(RoomID) ->
    gen_server:call({global, game_server}, {end_game, RoomID}).
    
init(_Args) -> {ok, []}.

terminate(_Reason, State) -> {ok, State}.

% Матчмейкинг по списку игроков
find_awaiting(PlayerID, Dimension, WinStreak, Players) ->
    % Ожидающие игроки с такими же параметрами игры
    AwaitingPlayers = lists:map(
        fun({APlayer, _, _, _}) -> APlayer end,
        lists:filter(
            fun({APlayer, ADimension, AWinStreak, ARoom}) -> 
                (APlayer =/= PlayerID) and (ADimension =:= Dimension) and (AWinStreak =:= WinStreak) and (ARoom =:= null)
            end, 
            Players)
    ),
    case AwaitingPlayers of 
        [] -> 
            % Добавление нового игрока в лобби
            NewPlayers = Players ++ [{PlayerID, Dimension, WinStreak, null}],
            %io:format("!!! Players = ~w~n", [NewPlayers]),
            {reply, {awaiting_for_game}, NewPlayers};
        [Opponent | _] ->
            %io:format("!!! Opponent found~n"),
            % Создание новой игровой сессии
            NewRoom = ttt_room:start_game(Opponent, PlayerID, Dimension, WinStreak),                    
            NewPlayers1 = lists:keyreplace(PlayerID, 1, Players, {PlayerID, Dimension, WinStreak, NewRoom}),
            NewPlayers2 = lists:keyreplace(Opponent, 1, NewPlayers1, {Opponent, Dimension, WinStreak, NewRoom}),
            
            %io:format("!!! Players (new room) = ~w~n", [NewPlayers2]),
            
            ttt_player:game_found(Opponent, NewRoom, 1),
            ttt_player:notify(Opponent, {make_turn}),
            {reply, {game_found, NewRoom, 2}, NewPlayers2}
    end.
    
    
handle_call({create_game, PlayerID, Dimension, WinStreak}, _PlayerPID, Players) ->
    % Не присутствует ли игрок уже в лобби
    case lists:keyfind(PlayerID, 1, Players) of
        {PlayerID, _, _, ARoom} -> 
            % Игрок создает новую игру
            if  (ARoom =:= null) ->
                    NewPlayers = lists:keyreplace(PlayerID, 1, Players, {PlayerID, Dimension, WinStreak, null}),
                    %io:format("!!! Player changed~n"),
                    find_awaiting(PlayerID, Dimension, WinStreak, NewPlayers);
                true ->
                    %io:format("!!! Player is present~n"),
                    {reply, {game_already_started}, Players}
            end;
        false ->
            NewPlayers = lists:keystore(PlayerID, 1, Players, {PlayerID, Dimension, WinStreak, null}),
            find_awaiting(PlayerID, Dimension, WinStreak, NewPlayers)
    end;

handle_call({end_game, RoomID}, _From, Players) ->
    NewPlayers = lists:map(
        fun({APlayer, ADimension, AWinStreak, ARoom}) ->
            if  (ARoom =:= RoomID) -> {APlayer, ADimension, AWinStreak, null};
                true -> {APlayer, ADimension, AWinStreak, ARoom}
            end
        end,
        Players
    ),
    %io:format("!!! New Players = ~w", [NewPlayers]),
    {reply, ok, NewPlayers};
    
handle_call(_Request, _From, State)  -> {reply, ok, State}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.  

code_change(_OldCode, State, _Extra) -> {ok, State}.  
     
    