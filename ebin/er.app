{application, er,
 [
  {description, "Erlang Redis Library Application"},
  {vsn, "0.0.4"},
  {modules, [
             % new, good, er modules
             er,
             er_app,
             er_server,
             er_sup,
             erp,

             % old, potentially dangerous, erldis modules
             erldis,
             erldis_binaries,
             erldis_client,
             erldis_dict,
             erldis_list,
             erldis_proto,
             erldis_sets,
             erldis_sync_client,

             % our friend, gen_server2
             gen_server2
            ]},
  {registered, [er_sup]},
  {applications, [
                  kernel,
                  stdlib
                 ]},
  {mod, {er_app, []}},
  {env, []}
 ]}.
