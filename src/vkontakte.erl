%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2009 Max Lapshin
%%% @doc        Worker module for plugin example
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
-module(vkontakte).
-author('Max Lapshin <max@maxidoors.ru>').
-behaviour(application).

% Application API
-export([start/2, stop/1, config_change/3]).


% PLUGIN API
-export([start/0, stop/0]).

-export([call/2, call/4]).

-export([test/0, archive/0, reload/0]).

call(Method, Args) ->
  {ok, AppId} = application:get_env(vkontakte, app_id),
  {Secret, Args1} = case Method of
    "secure."++_ -> 
      {ok, SecretKey} = application:get_env(vkontakte, secret_key),
      {SecretKey, Args};
    _ ->
      {ok, ApiKey} = application:get_env(vkontakte, api_key),
      {value, {viewer_id, ViewerId}, Args_} = lists:keytake(viewer_id, 1, Args),
      {{ViewerId, ApiKey}, Args_}
  end,
  call(AppId, Secret, Method, Args1).

call(AppId, SecretKey, Method, Args) ->  
  {ok, Request} = vkontakte_sup:start_request(),
  vkontakte_request:call(Request, AppId, SecretKey, Method, Args).


start() -> 
  ok = application:start(vkontakte),
  case file:path_consult(["priv", "/etc/erlyvideo"], "vkontakte.conf") of
    {ok, Env, _Path} ->     
      [application:set_env(vkontakte, Key, Value) || {Key, Value} <- Env],
      ok;
    {error, enoent} ->
      error_logger:error_msg("No vkontakte.conf found");
    {error, Reason} ->
      error_logger:error_msg("Couldn't load vkontakte.conf: ~p~n", [Reason]),
      ok
  end.
  
  
test() ->
  each_minute:test(),
  mp4_writer:test().


reload() ->
  {ok, Modules} = application:get_key(vkontakte,modules),
  [begin
    code:soft_purge(Module),
    code:load_file(Module)
  end || Module <- Modules].

  

archive() ->
  make:all([load]),
  application:load(vkontakte),
  {ok, Version} = application:get_key(vkontakte,vsn),
  zip:create("vkontakte-"++Version++".ez", ["vkontakte/ebin", "vkontakte/wwwroot"], [{cwd, "../"},{compress,all},{uncompress,all},verbose]).
  
  
%%--------------------------------------------------------------------
%% @spec (Type::any(), Args::list()) -> any()
%% @doc Starts RTMP library
%% @end 
%%--------------------------------------------------------------------

start(_Type, _Args) -> 
  vkontakte_sup:start_link().



%%--------------------------------------------------------------------
%% @spec (Any::any()) -> ok()
%% @doc Stop RTMP library
%% @end 
%%--------------------------------------------------------------------
stop(_S) ->
  ok.


%%--------------------------------------------------------------------
%% @spec (Any::any(),Any::any(),Any::any()) -> any()
%% @doc Reload ErlMedia Application config
%% @end 
%%--------------------------------------------------------------------
config_change(_Changed, _New, _Remove) ->
  ok.
  
stop() -> 
  application:stop(vkontakte),
  application:unload(vkontakte).

