-module(packet_counter).
-behaviour(gen_server).

%% API
-export([start_link/1, create_channel/1, show_channels/0]).
%create channel

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {channels::map()}).

start_link(_) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
   {ok, #state{channels=#{}}}.

handle_call(stop, _From, State) ->
   {stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({add_channel, Id, CounterPid, UdpPid, Port}, #state{channels = Channels} = State) ->
    {noreply, State#state{channels = Channels#{Id => {UdpPid, CounterPid, Port}}}};

handle_cast({show_channels}, #state{channels = Channels} = State) ->
    io:format("Channels: ~p ~n", [Channels]),
    {noreply, State};

handle_cast(_Msg, State) ->
   {noreply, State}.

handle_info(_Info, State) ->
   {noreply, State}.

terminate(_Reason, _State) ->
   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.

create_channel(Id) ->
    %Criar o contador
    {ok, CounterPid} = counter:start_link([]),
    {ok, UdpPid, Port} = create_udp_channel(CounterPid),
    gen_server:cast(bilhetador, {add_channel, Id, CounterPid, UdpPid, Port}).

create_udp_channel(CounterPid) ->
    create_udp_channel_(CounterPid, 0).

create_udp_channel_(CounterPid, 10) ->
    {error, CounterPid};
create_udp_channel_(CounterPid, Counter) ->
    %io:format("Counter: ~p ~n", [Counter]),
    Port = 10000 + rand:uniform(10000),
    case udp_channel:start_link(Port, 10000, CounterPid) of 
        {ok, UdpPid} -> 
            {ok, UdpPid, Port};
        _ ->
            create_udp_channel_(CounterPid, Counter+1)
        end.

show_channels() ->
    %Criar o contador
    gen_server:cast(bilhetador, {show_channels}).
