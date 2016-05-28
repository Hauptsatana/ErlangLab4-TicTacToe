-module(ttt_player).
-behaviour(gen_server).

%% Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([new_player/0, create_game/3, game_found/3, make_turn/3, field_info/1, notify/2]).

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

% Создать игрока
new_player() ->
    PlayerID = list_to_atom("User" ++ integer_to_list(erlang:system_time())),
    gen_server:start_link({global, PlayerID}, ?MODULE, [PlayerID], []),
    PlayerID.
    
create_game(PlayerID, Dimension, WinStreak) ->
    gen_server:call({global, PlayerID}, {create_game, Dimension, WinStreak}).
    
% Комната найдена - начать игру 
game_found(PlayerID, RoomID, Turn) ->
    gen_server:call({global, PlayerID}, {game_found, RoomID, Turn}).
    
% Сделать ход
make_turn(PlayerID, Row, Column) ->
    gen_server:call({global, PlayerID}, {make_turn, Row, Column}).

% Запросить состояние поля    
field_info(PlayerID) ->
    gen_server:call({global, PlayerID}, {field_info}).
    
% Оповестить
notify(PlayerID, Message) ->
    gen_server:call({global, PlayerID}, {notify, Message}).

init([PlayerID]) -> {ok, [PlayerID, {idle}]}.

terminate(_Reason, State) -> {ok, State}.

handle_call({create_game, Dimension, WinStreak}, _From, [PlayerID, Status]) ->
    case gen_server:call({global, game_server}, {create_game, PlayerID, Dimension, WinStreak}) of 
        {awaiting_for_game} -> 
            io:format("Your request is send to server. We're doing our best to find a game for you.~n"),
            {reply, ok, [PlayerID, {awaiting, Dimension, WinStreak}]};
        {game_already_started} -> 
            io:format("Sorry, you've already started a game.~n"),
            {reply, ok, [PlayerID, Status]};
        {game_found, RoomID, Turn} ->
            io:format("Game found! You are Player~B~n", [Turn]),
            {reply, ok, [PlayerID, {ingame, RoomID}]}
    end;
    
handle_call({game_found, RoomID, Turn}, _From, [PlayerID, _Status]) ->
    io:format("Game found! You are Player~B~n", [Turn]),
    {reply, ok, [PlayerID, {ingame, RoomID}]};
    
handle_call({notify, Message}, _From, [PlayerID, Status]) ->
    NewStatus = case Message of
        {opponents_turn, Row, Column} -> 
            io:format("Your oppenent marked (~B;~B)~n", [Row, Column]), Status;
        {loser} ->
            io:format("You LOOSE this game...~n"), {idle};
        {draw} ->
            io:format("It's a DRAW...~n"), {idle};
        {make_turn} ->
            io:format("It's your turn now.~n"), Status
    end,
    {reply, ok, [PlayerID, NewStatus]};
    
handle_call({make_turn, Row, Column}, _From, [PlayerID, {ingame, RoomID}]) ->
    NewStatus = case gen_server:call({global, RoomID}, {make_turn, PlayerID, Row, Column}) of 
        {cell_marked} ->
            io:format("This cell is already marked~n"), {ingame, RoomID};
        {wrong_turn} ->
            io:format("It's not your turn now!~n"), {ingame, RoomID};
        {winner} ->
            io:format("You WIN this game!~n"), {idle};
        {draw} ->
            io:format("IT's a DRAW...~n"), {idle};
        {opponents_turn} ->
            io:format("Nice turn! Please, wait for your opponent...~n"), {ingame, RoomID}
    end,
    {reply, ok, [PlayerID, NewStatus]};

handle_call({field_info}, _From, [PlayerID, {ingame, RoomID}]) ->
    {Field, Dimension} = gen_server:call({global, RoomID}, {field_info}),
    print_field(Field, Dimension),
    {reply, ok, [PlayerID, {ingame, RoomID}]};
    
 handle_call({field_info}, _From, State) ->
     io:format("State : ~w~n", [State]).
    
handle_cast(_Request, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

code_change(_OldCode, State, _Extra) -> {ok, State}.
    
    