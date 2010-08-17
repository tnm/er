-module(eru).
-compile(export_all).

dump_all(Server) ->
  dump(Server, <<"*">>).

dump(Server, Pattern) ->
  Keys = er:keys(Server, Pattern),
  [{K, value(Server, K)} || K <- Keys].

value(Server, Key) ->
  Type = er:type(Server, Key),
  case Type of
    string -> er:get(Server, Key);
      list -> er:lrange(Server, 0, inf);
       set -> er:smembers(Server, Key);
      zset -> Length = er:zcard(Server, Key),
              er:zrevrange(Server, Key, 0, Length);
      hash -> er:hgetall_k(Server, Key)
  end.
