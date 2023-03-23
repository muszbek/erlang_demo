%%%-------------------------------------------------------------------
%%% @author tmuszbek <tmuszbek@tmuszbek-VirtualBox>
%%% @copyright (C) 2023, tmuszbek
%%% @doc
%%%
%%% @end
%%% Created : 22 Mar 2023 by tmuszbek <tmuszbek@tmuszbek-VirtualBox>
%%%-------------------------------------------------------------------
-module(message_server).

-behaviour(gen_server).

%% API
-export([start_link/0, reg/1, join/1, join/2, peers/0, chat/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).

-record(state, {name = null}).

%%%===================================================================
%%% API
%%%===================================================================
reg(Name) ->
    gen_server:call(?SERVER, {reg, Name}).

join(Target) ->
    gen_server:cast(?SERVER, {join, Target}).

join(Name, Target) ->
    reg(Name),
    join(Target).

peers() ->
    global:registered_names().

chat(Peer, Message) ->
    gen_server:cast(?SERVER, {chat_send, Peer, Message}).
    
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} |
	  {error, Error :: {already_started, pid()}} |
	  {error, Error :: term()} |
	  ignore.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) -> {ok, State :: term()} |
	  {ok, State :: term(), Timeout :: timeout()} |
	  {ok, State :: term(), hibernate} |
	  {stop, Reason :: term()} |
	  ignore.
init([]) ->
    process_flag(trap_exit, true),
    net_kernel:monitor_nodes(true, [nodedown_reason]),
    logger:notice("Message server started"),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
	  {reply, Reply :: term(), NewState :: term()} |
	  {reply, Reply :: term(), NewState :: term(), Timeout :: timeout()} |
	  {reply, Reply :: term(), NewState :: term(), hibernate} |
	  {noreply, NewState :: term()} |
	  {noreply, NewState :: term(), Timeout :: timeout()} |
	  {noreply, NewState :: term(), hibernate} |
	  {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
	  {stop, Reason :: term(), NewState :: term()}.

handle_call({reg, Name}, _From, State=#state{name=null}) when is_atom(Name) ->
    {Result, RegName} = do_reg(Name),
    {reply, Result, State#state{name=RegName}};

handle_call({reg, Name}, _From, State=#state{name=SomeName}) when is_atom(Name) ->
    {Result, RegName} = re_reg(Name, SomeName),
    {reply, Result, State#state{name=RegName}};

handle_call({reg, Name}, _From, State) ->
    logger:warning("Register name ~p has to be atom!", [Name]),
    {reply, ok, State};

handle_call(Request, From, State) ->
    logger:warning("Unknown call ~p from ~p", [Request, From]),
    {reply, ok, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: term()) ->
	  {noreply, NewState :: term()} |
	  {noreply, NewState :: term(), Timeout :: timeout()} |
	  {noreply, NewState :: term(), hibernate} |
	  {stop, Reason :: term(), NewState :: term()}.

handle_cast({join, Target}, State) ->
    Result = net_kernel:connect_node(Target),
    logger:notice("Joining node ~p succeeded: ~p", [Target, Result]),
    {noreply, State};

handle_cast({chat_send, Peer, Message}, State=#state{name=Name}) ->
    case lists:member(Peer, peers()) of
	true -> global:whereis_name(Peer) ! {chat_receive, Name, Message};
	false -> logger:warning("~p is not a known peer", [Peer])
    end,
    {noreply, State};

handle_cast(Request, State) ->
    logger:warning("Unknown cast ~p", [Request]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: term()) ->
	  {noreply, NewState :: term()} |
	  {noreply, NewState :: term(), Timeout :: timeout()} |
	  {noreply, NewState :: term(), hibernate} |
	  {stop, Reason :: normal | term(), NewState :: term()}.

handle_info({nodeup, _NodeName, _}, State) ->
    {noreply, State};

handle_info({nodedown, _NodeName, _Reason}, State) ->
    {noreply, State};

handle_info({chat_receive, From, Message}, State) ->
    logger:notice("Chat message received from ~p: ~p", [From, Message]),
    {noreply, State};

handle_info(Info, State) ->
    logger:warning("Unknown message ~p", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
		State :: term()) -> any().
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()},
		  State :: term(),
		  Extra :: term()) -> {ok, NewState :: term()} |
	  {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for changing the form and appearance
%% of gen_server status when it is returned from sys:get_status/1,2
%% or when it appears in termination error logs.
%% @end
%%--------------------------------------------------------------------
-spec format_status(Opt :: normal | terminate,
		    Status :: list()) -> Status :: term().
format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================

do_reg(Name) ->
    Result = global:register_name(Name, self(), fun name_clash_resolve/3),
    case Result of
	yes ->
	    logger:notice("Registered as ~p", [Name]),
	    {ok, Name};
	no -> 
	    logger:warning("Cannot register name ~p, already exists on other node", [Name]),
	    {failed, null}
    end.

re_reg(Name, OldName) ->
    case global:whereis_name(Name) of
	undefined ->
	    global:unregister_name(OldName),
	    global:register_name(Name, self()),
	    logger:notice("Re-egistered as ~p", [Name]),
	    {ok, Name};
	Pid when is_pid(Pid) ->
	    logger:warning("Cannot re-register name ~p, already exists on other node", [Name]),
	    {failed, OldName}
    end.

name_clash_resolve(_Name, _Pid, OtherPid) ->
    OtherPid.
