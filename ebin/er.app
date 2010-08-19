{application, er,
 [
  {description, "Erlang Redis Library Application"},
  {vsn, "0.3.5"},
  {modules, [
             % new, good, er modules
             er,
             er_app,
             er_pool,
             er_server,
             er_sup,
             erp,
             eru,

             % Brought in from redis-erl
             er_redis
            ]},
  {registered, [er_sup]},
  {applications, [
                  kernel,
                  stdlib
                 ]},
  {mod, {er_app, []}},
  {env, []}
 ]}.
