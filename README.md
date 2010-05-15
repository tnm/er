er : erlang redis
=================

er implements redis commands using LFE.  We're starting
with a base of erldis and ripping out anything that seems slighly odd,
broken, or inconsistent.

Status
------
er is in-progress.  The basic redis functionality exists now, but we need to
flesh out functions to wrap return values and make things taste like erlang.

Code Guide
----------
src/er.lfe and src/erp.lfe are the starting points for LFE redis commands.

er.lfe creates an er module where the first parameter of all commands is the
redis connection (from erldis:connect):
  {ok, Client} = erldis:connect().
  er:set(Client, <<"chevron">>, <<"locked">>).

erp.lfe creates a parameterized module where the redis connection is carried
through all the commands:
  {ok, Client} = erldis:connect().
  RedisClient = erp:new(Client).
  RedisClient:set(<<"artist">>, <<"pallett">>).

Since er and erp perform the same functions only with slightly different
interfaces, most of their code is shared by directly including common files.
See files include/{utils,utils-macro,redis-cmds,redis-return-types}.lfe
 * utils.lfe - shared utility functions
 * utils-macro.lfe - shared macros
 * redis-cmds.lfe - all redis commands and their return type handlers
 * redis-return-types.lfe - an alternate format of specifying  redis return types.
   Also contains functions for converting redis return values to native types.

Building
--------

Pull in LFE locally if you don't already have it installed:
  ./rebar get-deps

Build:
  ./rebar compile

Testing
-------

  ./rebar compile
  mkdir -p .eunit
  cp ebin/er.beam ebin/erp.beam .eunit/
  ./rebar eunit

Ignore messages about cover and {er,erp}.beam (it's an artifact of LFE).

Testing still mainly covers erldis.  er testing needs to be written to obviate
the working, yet not entirely stable, erldis tests.

Next Steps
----------

In no specific order:
  * Sync er/erp return values with expected values based on Redis docs
    * (i.e. don't return 0 or 1, return true or false or "added" or "exists" etc)
  * Comprehensive test cases for er/erp
  * Custom behavior for non-standard commands (blocking pop, pubsub, quit)
  * Phase out test cases of erldis
  * More features
    * bidirectional mnesia sync?
    * redis as erlang term store vs. redis as generic store
    * generate modules for pre-defined common accessors

erldis : erlang client library for redis
----------------------------------------

Code started by [Valentino Volonghi](http://bitbucket.org/dialtone/) and [Jacob Perkins](http://bitbucket.org/japerk/)
This is a fork from over at [bitbucket](http://bitbucket.org/japerk/erldis).
Then it was cloned to [cstar](http://github.com/cstar/erldis).
Then matt cloned to [mattsta](http://github.com/mattsta/er) and renamed it to er.
