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

-module(gunc_pool_conn).
-behaviour(poolboy_worker).

%% Poolboy callbacks
-export([
	start_link/1
]).

%% =============================================================================
%% Poolboy callbacks
%% =============================================================================

start_link(Conn) ->
	gun:open(
		validate_host(Conn),
		validate_port(Conn),
		maps:get(options, Conn, #{})).

%% =============================================================================
%% Internal functions
%% =============================================================================

-spec validate_host(map()) -> list().
validate_host(#{host := Val}) when is_list(Val) -> Val;
validate_host(#{host := Val})                   -> throw({invalid_host, Val});
validate_host(_)                                -> throw(missing_host).

-spec validate_port(map()) -> non_neg_integer().
validate_port(#{port := Val}) when is_integer(Val), Val > 0 -> Val;
validate_port(#{port := Val})                               -> throw({invalid_port, Val});
validate_port(_)                                            -> throw(missing_port).
