%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2009 Max Lapshin
%%% @doc        Example of gen_server
%%% @reference  See <a href="http://erlyvideo.org/" target="_top">http://erlyvideo.org/</a> for more information
%%% @end
%%%
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2009 Max Lapshin
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.
%%%
%%%---------------------------------------------------------------------------------------
-module(vkontakte_request).
-author('Max Lapshin <max@maxidoors.ru>').
-behaviour(gen_server).


%% External API
-export([start_link/0, call/5]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).



start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


call(Request, AppId, SecretKey, Method, Args) ->
  gen_server:call(Request, {call, AppId, SecretKey, Method, Args}).
  

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

%%----------------------------------------------------------------------
%% @spec (Port::integer()) -> {ok, State}           |
%%                            {ok, State, Timeout}  |
%%                            ignore                |
%%                            {stop, Reason}
%%
%% @doc Called by gen_server framework at process startup.
%%      Create listening socket.
%% @end
%%----------------------------------------------------------------------


init([]) ->
  {ok, state}.

%%-------------------------------------------------------------------------
%% @spec (Request, From, State) -> {reply, Reply, State}          |
%%                                 {reply, Reply, State, Timeout} |
%%                                 {noreply, State}               |
%%                                 {noreply, State, Timeout}      |
%%                                 {stop, Reason, Reply, State}   |
%%                                 {stop, Reason, State}
%% @doc Callback for synchronous server calls.  If `{stop, ...}' tuple
%%      is returned, the server is stopped and `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------
handle_call({call, AppId, SecretKey, Method, Args}, _From, State) ->
  {ok, Reply} = vkontakte_invoke(AppId, SecretKey, Method, Args),
  {stop, normal, {ok, Reply}, State};
  
handle_call(Request, _From, State) ->
  {stop, {unknown_call, Request}, State}.


%%-------------------------------------------------------------------------
%% @spec (Msg, State) ->{noreply, State}          |
%%                      {noreply, State, Timeout} |
%%                      {stop, Reason, State}
%% @doc Callback for asyncrous server calls.  If `{stop, ...}' tuple
%%      is returned, the server is stopped and `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------
handle_cast(_Msg, State) ->
  {stop, {unknown_cast, _Msg}, State}.

%%-------------------------------------------------------------------------
%% @spec (Msg, State) ->{noreply, State}          |
%%                      {noreply, State, Timeout} |
%%                      {stop, Reason, State}
%% @doc Callback for messages sent directly to server's mailbox.
%%      If `{stop, ...}' tuple is returned, the server is stopped and
%%      `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------
handle_info({'DOWN', process, Client, _Reason}, Server) ->
  {noreply, Server};

handle_info(_Info, State) ->
  {noreply, State}.

%%-------------------------------------------------------------------------
%% @spec (Reason, State) -> any
%% @doc  Callback executed on server shutdown. It is only invoked if
%%       `process_flag(trap_exit, true)' is set by the server process.
%%       The return value is ignored.
%% @end
%% @private
%%-------------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%-------------------------------------------------------------------------
%% @spec (OldVsn, State, Extra) -> {ok, NewState}
%% @doc  Convert process state when code is changed.
%% @end
%% @private
%%-------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


to_list(A) when is_integer(A) -> integer_to_list(A);
to_list(A) when is_float(A) -> integer_to_list(round(A));
to_list(A) when is_list(A) -> A;
to_list(A) when is_binary(A) -> binary_to_list(A).


binary_to_hexbin(L) ->
  list_to_binary(lists:flatten(lists:map(fun(X) -> int_to_hex(X) end, binary_to_list(L)))).

int_to_hex(N) when N < 256 ->
  [hex(N div 16), hex(N rem 16)].

hex(N) when N < 10 ->
  $0+N;
hex(N) when N >= 10, N < 16 ->
  $a + (N-10).


vkontakte_invoke(AppId, SecretKey, Method, Args) ->
  {MegaSec, Sec, Usec} = erlang:now(),
  Timestamp = MegaSec*1000000+Sec,
  Rand = Sec*1000000+Usec,
  Required = [{api_id, AppId}, {v, "2.0"}, {timestamp, Timestamp}, {rand, Rand}, {method, Method}],
  
  UnsignedQuery = lists:keymerge(1, lists:keysort(1, Args), lists:keysort(1, Required)),
  S = lists:foldr(fun({Key, Value}, Acc) -> 
    [[atom_to_list(Key), "=", to_list(Value)] | Acc] 
  end, [SecretKey], UnsignedQuery),
  
  UnsignedBin = iolist_to_binary(S),
  Signature = binary_to_hexbin(erlang:md5(UnsignedBin)),
  Query = lists:keymerge(1, UnsignedQuery, [{sig, Signature}]),
  QueryString = lists:foldl(fun({Key, Value}, Acc) ->
    Acc ++ [atom_to_list(Key), "=", to_list(Value), "&"]
  end, "", Query),
  {ok, Socket} = gen_tcp:connect("api.vkontakte.ru", 80, [binary, {packet, raw}, {active, false}], 1000),
  Request = iolist_to_binary(["GET /api.php?", QueryString, " HTTP/1.1\r\nHost: api.vkontakte.ru\r\n\r\n"]),
  io:format("~s~n", [binary_to_list(Request)]),
  gen_tcp:send(Socket, Request),
  {ok, Reply} = gen_tcp:recv(Socket, 0, 10000),
  io:format("~s~n", [binary_to_list(Reply)]),
  {ok, Reply}.


