er: erlang redis
================

Status
------
er is production ready.
er is feature complete with redis-2.2 as of antirez/redis@59aee5513d27fdf2d24499f35611093fa0fab3fb 

Code Guide
----------
`src/er.lfe` and `src/erp.lfe` are where redis commands get created.

`er.lfe` creates a module where the first parameter of all commands is the
redis connection (the conenction is either a `er_redis` PID or `er_pool` name):
        {ok, Client} = er_redis:connect().
        er:set(Client, <<"chevron">>, <<"locked">>).

`erp.lfe` creates a parameterized module where the redis connection is carried
through all the commands:
        {ok, Client} = er_redis:connect().
        RedisClient = erp:new(Client).
        RedisClient:set(<<"artist">>, <<"pallett">>).

`er_pool.erl` gives you a centrally managed connection pool of redis clients.
Create a named pool, then use regular `er` commands against it.  If you use
a command requiring exclusive use of a client (b[lr]pop, brpoplpush, subscribe, watch, etc),
the client is taken out of the general pool and reserved for your individual
use. When the client is done with exclusive operations, the client is returned
to the general connection pool.

### Connect and use anonymous pid
        {ok, Client} = er_pool:start_link().
        er:set(Client, italian, greyhound).
        <<"greyhound">> = er:get(Client, italian).

### Connect and use named pool (no need for pid tracking)
        er_pool:start_link(userdb).
        er:set(userdb, hash_value_1, erlang:md5(<<"hash this">>).
        er:lpush(userdb, updates, hash_value_1).

### Blocking Pop
Blocking pop blocks the current process until Timeout (600 seconds below) or until
  an item becomes available.  Blocking operations return the atom nil on timeout.
        er_pool:start_link(workqueue_pool).
        er:blpop(workqueue_pool, blocked_key, 600).

### PubSub
        er_pool:start_link(announcements).
        % Subscribe to key `another`.  Returns a pid and subscribe validation.
        {SubClientPid, [subscribe, <<"another">>, 1]} = er:subscribe(announcements, another).

        % To receive published messages, run a blocking receive.
        % er_next blocks your current process until something is published.
        % You can run er:er_next(SubClientPid) in a loop to consume all published messages.
        [message, <<"bob">>, PublishedMessage] = er:er_next(SubClientPid).

        % Clean up when you are done to avoid leaking processes and sockets:
        SubClientPid ! shutdown.

For per-command usage examples, see `test/er_tests.erl`.

`er_redis.erl` was mainly taken from http://github.com/bmizerany/redis-erl/blob/master/src/redis.erl then
heavily modified to fit my purposes better.  Only 20 to 30 lines of the original file survived.

Since `er` and `erp` perform the same functions only with slightly different
interfaces, most of their code is shared by directly including common files.

See files `include/{utils,utils-macro,redis-cmds,redis-return-types}.lfe`:

* `utils.lfe` - shared utility functions
* `utils-macro.lfe` - includes the macro which generates return value macros
* `redis-cmds.lfe` - all redis commands and their return type handlers
* `redis-return-types.lfe` - an alternate format of specifying redis return types.
  Contains functions for converting redis return values to native types.

Building
--------
Download LFE:
        ./rebar get-deps

Build:
        ./rebar compile

Testing
-------
NB: Tests run against a redis on port 6991.
Running tests WILL DELETE ALL DATA on your port 6991 redis.
        ./rebar compile
        cd test
        erlc *.erl
        erl -pa ../ebin
         1> er_tests:test().
         2> er_concurrency_tests:test().

Next Steps
----------
In no specific order:

* More comprehensive test cases for er/erp
* More features
  * bidirectional mnesia sync?
  * redis as erlang term store vs. redis as generic store
  * generate modules for pre-defined common accessors

When to use er
--------------
Use `er` when you want to access redis from erlang.

`er` converts redis return types to erlang-friendly terms.  redis 1 and 0
  responses are converted to true/false atoms when appropriate.  redis binary
  results are returned as erlang binaries.  redis integer results
  are returned as erlang numbers.

`er` aims to be the most semantically correct erlang redis client with a sane
  translation layer between how redis views the world and how erlang views
  the world.

Contributions
-------------
Want to help?  Patches welcome.

* Poke around in `test/er_tests.erl` and add some missing tests.
* Write something to parse `include/er-cmds.lfe` and output docs for return types.
* Make a nicer interface to pubsub and blocking calls.
* Find a bug.  Fix a bug.
