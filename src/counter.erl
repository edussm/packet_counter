-module(counter).
-behaviour(gen_server).

% Gen Server Callbacks
-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2,
         start_link/1
         ]).
        
% API
-export([
        receivePacket/1,
        addWhiteList/1,
        removeWhiteList/1,
        showWhiteList/0
        ]).

-record(state, {domains = #{} :: map(), white_list = [<<"algar.br">>]}).

start_link(_) ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    %[ maps:put(D, 0, M) || D  <- Domains ],
    {ok, #state{domains = #{} }}.

handle_call({domains}, _From, State) ->
    {reply, maps:keys(State#state.domains), State};

%handle_call({counter}, _From, State) ->
%    {reply, State#state.counter, State};

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast({count, Domain, Packet}, #state{domains = Domains, white_list = WhiteList} = State) ->
    case lists:member(Domain, WhiteList) of 
        false ->   
            D = case maps:is_key(Domain, Domains) of
                true ->
                    Counter = maps:get(Domain, Domains),
                    Domains#{Domain => Counter + byte_size(Packet)};
                _ -> 
                    Domains#{Domain => byte_size(Packet)}
                end,
                io:format("Count Domains: ~p~n", [D]),
            {noreply, State#state{domains = D}};
        _ -> 
            {noreply, State}
        end;

handle_cast({add_white_list, Domain}, #state{white_list = WhiteList} = State) ->  
    case lists:member(Domain, WhiteList) of
        false ->
            io:format("Add White List: ~p~n", [Domain]),
            {noreply, State#state{white_list = [Domain | WhiteList]}} 
        end;

handle_cast({remove_white_list, Domain}, #state{white_list = WhiteList} = State) ->  
    io:format("Rm White List: ~p de ~p ~n", [Domain, WhiteList]),
    {noreply, State#state{white_list = lists:delete(Domain, WhiteList)}};

handle_cast({show_white_list}, #state{white_list = WhiteList} = State) ->  
    io:format("White List: ~p ~n", [WhiteList]),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({_Sock, _Addr, _Port, Packet}, #state{} = State) ->
    {Domain, _Data} = extractDomain(Packet),
    gen_server:cast(self(), {count, Domain, Packet}),
    {noreply, State};

handle_info(_Event, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

receivePacket(Packet) ->
    {Domain, _Data} = extractDomain(Packet),
    gen_server:cast(counter, {count, Domain, Packet}).

addWhiteList(Domain) ->
    gen_server:cast(counter, {add_white_list, Domain}).

removeWhiteList(Domain) ->
    gen_server:cast(counter, {remove_white_list, Domain}).

showWhiteList() ->
    gen_server:cast(counter, {show_white_list}).

extractDomain(Packet) ->
    [Domain | Data] = string:split(removeProto(Packet), <<"/">>),
    {Domain, Data}.

removeProto(<<$h, $t, $t, $p, $s, $:, $/, $/, Payload/binary>>) -> 
    Payload;    
removeProto(<<$h, $t, $t, $p, $:, $/, $/, Payload/binary>>) -> 
    Payload;
removeProto(<<Payload/binary>>) -> 
    Payload.
