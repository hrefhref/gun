%% Copyright (c) 2011-2019, Loïc Hoguin <essen@ninenines.eu>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(gun_inet).

-export([getaddrs/3]).

-spec getaddrs(inet:ip_address() | inet:hostname(), [inet:address_family()], timeout())
      -> {ok, [inet:ip_address()]} | {error, inet:posix()}.
getaddrs(Address, Families, Timeout) ->
  Timer = inet:start_timer(Timeout),
  try
    getaddrs(Address, Families, Timer, [], {error, nxdomain})
  after
    _ = inet:stop_timer(Timer)
  end.

getaddrs(_, [], _, [], Error) ->
  Error;
getaddrs(_, [], _, Acc, _) ->
  {ok, lists:reverse(lists:flatten(Acc))};
getaddrs(Address, [Family | Families], Timer, Acc, _) ->
  case inet:getaddrs_tm(Address, Family, Timer) of
    {ok, IPs0} ->
      IPs = [{Family, IP} || IP <- IPs0],
      getaddrs(Address, Families, Timer, [IPs | Acc], undefined);
    Error ->
      getaddrs(Address, Families, Timer, Acc, Error)
  end.

