er : erlang redis
=================

er implements redis commands using LFE.

Status
------
er is in-progress.  The basic redis functionality exists now, but we need to
flesh out functions to wrap return values and make things taste like erlang.

Code Guide
----------
`src/er.lfe` and `src/erp.lfe` are the starting points for LFE redis commands.

`er.lfe` creates a module where the first parameter of all commands is the
redis connection (the conenction is either a er_redis or er_pool):
        {ok, Client} = er_redis:connect().
        er:set(Client, <<"chevron">>, <<"locked">>).

`erp.lfe` creates a parameterized module where the redis connection is carried
through all the commands:
        {ok, Client} = er_redis:connect().
        RedisClient = erp:new(Client).
        RedisClient:set(<<"artist">>, <<"pallett">>).

`er_pool.erl` gives you a centrally managed connection pool of redis clients.
Create the pool then use regular `er` commands against the pool.  If you use
a command requiring exclusive use of a client (b[lr]pop, subscribe, watch, etc),
the client is taken out of the general pool and reserved for your individual
use. When the client is done with its exclusive operations, the client is returned
to the general connection pool.
        {ok, Client} = er_pool:start_link().
        er:set(Client, italian, greyhound).
        <<"greyhound">> = er:get(Client, italian).

        er:blpop(Client, wait_for_me, 600).  % blocks until the 600 second
                                             % timeout or until an item is available.
                                             % returns `nil` on timeout.

        % Subscribe to key `another`.  Returns a pid and subscribe validation.
        {SubClientPid, [subscribe, <<"another">>, 1]} = er:subscribe(Client, another).
        % To receive published messages, run a blocking receive:
        [message, <<"bob">>, PublishedMessage] = er:er_next(SubClientPid).
        % er_next blocks your current process until something is published.
        % You can run er:er_next(SubClientPid) in a loop to consume all published messages.
        % When you are done with your subscription, clean up the SubClientPid process
        % so we avoid leaking processes and sockets:
        SubClientPid ! shutdown.

`er_redis.erl` was mainly taken from http://github.com/bmizerany/redis-erl/blob/master/src/redis.erl then
heavily modified to fit my purposes better.  Only 20 to 30 lines of the original file survived.

Since `er` and `erp` perform the same functions only with slightly different
interfaces, most of their code is shared by directly including common files.

See files `include/{utils,utils-macro,redis-cmds,redis-return-types}.lfe`:

* `utils.lfe` - shared utility functions
* `utils-macro.lfe` - shared macros
* `redis-cmds.lfe` - all redis commands and their return type handlers
* `redis-return-types.lfe` - an alternate format of specifying  redis return types.
  Also contains functions for converting redis return values to native types.

Building
--------

Pull in LFE locally:
        ./rebar get-deps

Build:
        ./rebar compile

Testing
-------
NB: We use the default redis port 6379 for testing.
    Running tests WILL DELETE ALL DATA on your port 6379 redis.
    We do not recommend running your main redis DB on the default port.
        ./rebar compile
        cd test
        erlc *.erl
        erl -pa ../ebin
         1> er_tests:test().
         2> er_concurrency_tests:test().

Next Steps
----------

In no specific order:

* Sync er/erp return values with expected values based on Redis docs
  * (i.e. don't return 0 or 1, return true or false or "added" or "exists" etc)
* Comprehensive test cases for er/erp
* Custom behavior for non-standard commands (blocking pop, pubsub, quit)
* More features
  * bidirectional mnesia sync?
  * redis as erlang term store vs. redis as generic store
  * generate modules for pre-defined common accessors

History
-------
[history story forthcoming]
