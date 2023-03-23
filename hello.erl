-module(hello).

-export([hello/0, hello/1, listen/0, start_listen/0]).

%% start with explaining this:

%% hello() ->
%%     hello,
%%     ok.

%% hello() ->
%%     io:format("Hello world!~n").

hello() ->
    hello("world").

hello(Subject) ->
    io:format("Hello ~s!~n", [Subject]).

%% Pid = spawn(fun hello:listen/0).
%% Pid ! hello.

%% listen() ->
%%     receive
%% 	hello ->
%% 	    hello();
%% 	{hello, Subject} ->
%% 	    hello(Subject);
%% 	_ -> io:format("Say what?~n")
%%     end,
%%     listen().

listen() ->
    receive
	stop ->
	    io:format("Stopping~n"),
	    stopped;
	Message ->
	    handle(Message),
	    listen()
    end.

handle(Message) ->
    case Message of
	hello ->
	    hello();
	{hello, Subject} ->
	    hello(Subject);
	{Pid, hello} when is_pid(Pid) ->
	    Pid ! world;
	{Pid, hello, Subject} ->
	    Pid ! Subject;
	_ ->
	    io:format("Say what?~n")
    end.

%% hello:start_listen().
%% listener ! hello.
%% listener ! {self(), hello}.
start_listen() ->
    Pid = spawn(fun() -> listen() end),
    register(listener, Pid).
