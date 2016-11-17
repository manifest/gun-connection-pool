%% ----------------------------------------------------------------------------
%% The MIT License
%%
%% Copyright (c) 2016 Andrei Nesterov <ae.nesterov@gmail.com>
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to
%% deal in the Software without restriction, including without limitation the
%% rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
%% sell copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
%% IN THE SOFTWARE.
%% ----------------------------------------------------------------------------

-module(gunc_pool).

%% API
-export([
	lock/1,
	lock/2,
	try_lock/1,
	unlock/2,
	child_spec/1
]).

%% Types
-type name() :: atom().

-export_type([name/0]).

%% =============================================================================
%% API
%% =============================================================================

-spec lock(name()) -> pid().
lock(Pool) ->
	poolboy:checkout(Pool, true).

-spec lock(name(), timeout()) -> pid().
lock(Pool, Timeout) ->
	poolboy:checkout(Pool, true, Timeout).

-spec try_lock(name()) -> {ok, pid()} | error.
try_lock(Pool) ->
	case poolboy:checkout(Pool, false, infinity) of
		full -> error;
		Pid  -> {ok, Pid}
	end.

-spec unlock(name(), pid()) -> ok.
unlock(Pool, Pid) ->
	poolboy:checkin(Pool, Pid).

%% We don't want to spawn new connections, we also don't want to respawn
%% existed ones (after the max overflow size is reached), so that we
%% always have `max_overflow` equal to 0. This way, we still can process
%% more than `size` requests by delaying them on `timeout` value using
%% `lock/{1,2}` functions.
-spec child_spec(map()) -> supervisor:child_spec().
child_spec(#{name := Name, size := Size, connection := Conn} = M) ->
	poolboy:child_spec(
		Name,
		[ {worker_module, gunc_pool_conn},
			{name, {local, Name}},
			{size, Size},
			{max_overflow, 0},
			{strategy, maps:get(strategy, M, lifo)} ],
		Conn).
