-module(er_tests).
-include_lib("eunit/include/eunit.hrl").

-define(E(A, B), ?assertEqual(A, B)).
-define(_E(A, B), ?_assertEqual(A, B)).

redis_setup_clean() ->
  {ok, Cxn} = erldis:connect(),
  ok = er:flushall(Cxn),
  Cxn.

er_basic_commands_test_() ->
  {setup,
    fun redis_setup_clean/0,
    fun(C) -> 
      [
        ?_E(false, er:exists(C, existing)),
        ?_E(ok,    er:set(C, existing, ralph)),
        ?_E(true,  er:exists(C, existing)),
        ?_E([<<"ralph">>], er:get(C, existing)),
        ?_E(1,     er:del(C, existing)),
        ?_E(0,     er:del(C, existing)),
        ?_E(false, er:exists(C, existing)),
        ?_E([],    er:keys(C, "*")),
        ?_E(nil,   er:randomkey(C)),
        ?_assertException(throw,
          {redis_return_status, <<"ERR no such key">>},
          er:rename(C, bob2, bob3)),
        ?_E(ok, er:set(C, bob3, bob3content)),
        ?_assertException(throw,
          {redis_return_status,
            <<"ERR source and destination objects are the same">>},
          er:rename(C, bob3, bob3)),
        ?_E(ok,    er:rename(C, bob3, bob2)),
        ?_E(true,  er:renamenx(C, bob2, bob3)),
        ?_E(ok,    er:set(C, bob4, bob4content)),
        ?_E(false, er:renamenx(C, bob3, bob4)),
        ?_E(2,     er:dbsize(C)),
        ?_E(ok,    er:set(C, expireme, expiremecontent)),
        ?_E(true,  er:expire(C, expireme, 30)),
        ?_E(false, er:expire(C, expireme, 30)),
        ?_E(false, er:expire(C, expireme_noexist, 30)),
        ?_assertMatch(TTL when TTL =:= 29 orelse TTL =:= 30,
          er:ttl(C, expireme))
        % getset,
        % mget,
        % setnx,
        % setex,
        % ttl,
        % ttl,
        % mset,
        % msetnx,
        % incr,
        % incrby,
        % decr,
        % decrby,
        % append,
        % substr,
      ]
    end
  }.

er_lists_commands_test_() ->
  {setup,
    fun redis_setup_clean/0,
    fun(C) -> 
      [
        % []
        ?_E(1,  er:rpush(C, listA, aitem1)),
        % aitem1
        ?_E(2,  er:rpush(C, listA, aitem2)),
        % aitem1, aitem2
        ?_E(3,  er:rpush(C, listA, aitem3)),
        % aitem1, aitem2, aitem3
        ?_E(4,  er:lpush(C, listA, aitem4)),
        % aitem4, aitem1, aitem2, aitem3
        ?_E(5,  er:lpush(C, listA, aitem5)),
        % aitem5, aitem4, aitem1, aitem2, aitem3
        ?_E(6,  er:lpush(C, listA, aitem6)),
        % aitem6, aitem5, aitem4, aitem1, aitem2, aitem3
        ?_E(6, er:llen(C, listA)),
        ?_E([<<"aitem6">>], er:lrange(C, listA, 0, 0)),
        ?_E([<<"aitem6">>, <<"aitem5">>], er:lrange(C, listA, 0, 1)),
        ?_E([], er:lrange(C, listA, 10, 20))
        % ltrim
        % lindex
        % lset
        % lrem
        % lpop
        % rpop
        % blpop
        % brpop
        % rpoplpush
      ]
    end
  }.

er_sets_commands_test_() ->
  {setup,
    fun redis_setup_clean/0,
    fun(C) -> 
      [
        ?_E(true,  er:sadd(C, setA, amember1)),
        ?_E(false, er:sadd(C, setA, amember1)),
        ?_E(true,  er:sadd(C, setA, amember2)),
        ?_E(true,  er:sadd(C, setA, amember3)),
        ?_E(true,  er:srem(C, setA, amember1)),
        ?_E(false, er:srem(C, setA, amember1)),
        ?_assertMatch([M] when M =:= <<"amember2">> orelse M =:= <<"amember3">>,
          er:spop(C, setA)),
        ?_assertMatch([M] when M =:= <<"amember2">> orelse M =:= <<"amember3">>,
          er:spop(C, setA)),
        ?_E([nil], er:spop(C, setA)),
        ?_E(0,     er:scard(C, setA)),
        ?_E(true,  er:sadd(C, setB, bmember1)),
        ?_E(true,  er:sadd(C, setB, bmember2)),
        ?_E(true,  er:sadd(C, setB, bmember3)),
        ?_E(3,     er:scard(C, setB)),
        ?_E(false, er:smove(C, setB, setA, bmembernone)),
        ?_E(true,  er:smove(C, setB, setA, bmember1)),
        ?_E(1,     er:scard(C, setA)),
        ?_E(2,     er:scard(C, setB))
        % sismember
        % sinter
        % sinterstore
        % sunion
        % sunionstore
        % sdiff
        % sdiffstore
        % smembers
        % srandmember
      ]
    end
  }.

er_sorted_sets_commands_test_() ->
  {setup,
    fun redis_setup_clean/0,
    fun(C) -> 
      [
        ?_E(true,  er:zadd(C, zsetA, 10, amember1)),
        ?_E(false, er:zadd(C, zsetA, 10, amember1)),
        ?_E(true,  er:zadd(C, zsetA, 10, amember2)),
        ?_E(true,  er:zadd(C, zsetA, 10, amember3)),
        ?_E(true,  er:zrem(C, zsetA, amember3)),
        ?_E(false, er:zrem(C, zsetA, amembernone)),
        ?_E(20,    er:zincrby(C, zsetA, 10, amember1)),
        ?_E(-20,   er:zincrby(C, zsetA, -40, amember1)),
        ?_E(0,     er:zrank(C, zsetA, amember1)),
        ?_E(1,     er:zrank(C, zsetA, amember2)),
        ?_E(1,     er:zrevrank(C, zsetA, amember1)),
        ?_E(0,     er:zrevrank(C, zsetA, amember2)),
        ?_E(inf,   er:zincrby(C, zsetA, "inf", amember1)),
        ?_E(inf,   er:zincrby(C, zsetA, "inf", amember1)),
        ?_E(1,     er:zrank(C, zsetA, amember1)),  % -20 + inf = top of list
        ?_E(0,     er:zrank(C, zsetA, amember2)),
%        ?_E(nan,   er:zincrby(C, zsetA, "-inf", amember1)),
%        ?_E(nan, er:zincrby(C, zsetA, "-inf", amember1)),  % crashes redis
        ?_E(1,    er:zrank(C, zsetA, amember1)),  % at position inf, it moves up
        ?_E(0,    er:zrank(C, zsetA, amember2))
        % zrange
        % zrevrange
        % zrangebyscore
        % zcard
        % zscore
        % zremrangebyrank
        % zremrangebyscore
        % zunionstore
        % zinterstore
      ]
    end
  }.

er_hashes_commands_test_() ->
  {setup,
    fun redis_setup_clean/0,
    fun(C) -> 
      [
        ?_E(true,  er:hset(C, hashA, fieldA, valueA)),
        ?_E(true,  er:hset(C, hashA, fieldB, valueB)),
        ?_E(true,  er:hset(C, hashA, fieldC, valueC)),
        ?_E(false, er:hset(C, hashA, fieldA, valueA1)),
        ?_E([<<"valueA1">>], er:hget(C, hashA, fieldA)),
        ?_E([nil],   er:hget(C, hashB, fieldZ)),
%        ?_E([<<"valueA1">>, <<"valueC">>, nil],
%                     er:hmget(C, hashA, [fieldA, fieldC, fieldNone])),
        ?_E(ok,    er:hmset(C, hashC, [fieldA, valA, fieldB, valB])),
        ?_E([<<"valA">>], er:hget(C, hashC, fieldA)),
        ?_E([<<"valB">>], er:hget(C, hashC, fieldB)),
        ?_E(12,    er:hincrby(C, hashD, fieldAddr, 12)),
        ?_E(72,    er:hincrby(C, hashD, fieldAddr, 60)),
        ?_E(true,  er:hexists(C, hashD, fieldAddr)), 
        ?_E(false, er:hexists(C, hashD, fieldBddr)), 
        ?_E(false, er:hexists(C, hashZ, fieldZ)),
        ?_E(true,  er:hdel(C, hashD, fieldAddr)),
        ?_E(false, er:hdel(C, hashD, fieldAddr)),
        ?_E(3,     er:hlen(C, hashA)),
        ?_E([<<"fieldA">>, <<"fieldB">>, <<"fieldC">>],
                   er:hkeys(C, hashA)),
        ?_E([<<"valueA1">>, <<"valueB">>, <<"valueC">>],
                   er:hvals(C, hashA)),
        ?_E([<<"fieldA">>, <<"valueA1">>,
             <<"fieldB">>, <<"valueB">>,
             <<"fieldC">>, <<"valueC">>],
                   er:hgetall(C, hashA))
      ]
    end
  }.

er_sorting_commands_test_() ->
  {setup,
    fun redis_setup_clean/0,
    fun(_C) -> 
      [
        % sort
      ]
    end
  }.

er_transactions_commands_test_() ->
  {setup,
    fun redis_setup_clean/0,
    fun(_C) -> 
      [
        % multi
        % exec
        % discard
        % watch
        % unwatch
      ]
    end
  }.

er_pubsub_commands_test_() ->
  {setup,
    fun redis_setup_clean/0,
    fun(_C) -> 
      [
        % subscribe
        % unsubscribe
        % psubscribe
        % punsubscribe
        % publish
      ]
    end
  }.

er_persistence_commands_test_() ->
  {setup,
    fun redis_setup_clean/0,
    fun(_C) -> 
      [
        % save
        % bgsave
        % lastsave
        % shutdown
        % bgrewriteaof
      ]
    end
  }.

er_server_control_commands_test_() ->
  {setup,
    fun redis_setup_clean/0,
    fun(_C) -> 
      [
        % info
        % monitor
        % slaveof
        % config
      ]
    end
  }.

er_return_test_() ->
  {inparallel, 
    [
      ?_E(nil,      er:'redis-return-nil'(nil)),
      ?_E(ok,       er:'redis-return-status'([<<"ok">>])),
      ?_assertException(throw, {redis_return_status, <<"throwed">>},
        er:'redis-return-status'([{error, <<"throwed">>}])),
      ?_E(53,       er:'redis-return-integer'([<<"53">>])),
      ?_E(inf,      er:'redis-return-integer'([<<"inf">>])),
      ?_E('-inf',   er:'redis-return-integer'([<<"-inf">>])),
      ?_E(<<"ok">>, er:'redis-return-single-line'([<<"ok">>])),
      ?_E(ok,       er:'redis-return-bulk'(ok)),
      ?_E(ok,       er:'redis-return-multibulk'(ok)),
      ?_E(ok,       er:'redis-return-special'(ok)),
      ?_E(true,     er:'redis-return-integer-true-false'([<<"1">>])),
      ?_E(false,    er:'redis-return-integer-true-false'([<<"0">>]))
    ]
  }.

