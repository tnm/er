-module(erldis_binaries).

-export([to_binary/1]).

to_binary(X) when is_binary(X)  -> X;
% Basic determination of a char list: "abc"
to_binary(X) when is_list(X) andalso is_integer(hd(X)) -> list_to_binary(X);
% Basic determination of a list of stuff: [abc, def, <<"other">>, 12]
to_binary(X) when is_list(X)    -> [to_binary(A) || A <- X];
to_binary(X) when is_atom(X)    -> list_to_binary(atom_to_list(X));
to_binary(X) when is_integer(X) -> list_to_binary(integer_to_list(X)).
