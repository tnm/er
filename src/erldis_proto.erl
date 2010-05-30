-module(erldis_proto).

-export([parse/2]).

parse(empty, <<"-", Message/binary>>) ->
    {error, Message};
parse(_, <<"$-1">>) ->
    {read, nil};
parse(empty, <<"*-1">>) ->
    {hold, nil};
parse(empty, <<"*0">>) ->
    {read, 0};
parse(_, <<"$", BulkSize/binary>>) ->
    {read, list_to_integer(binary_to_list(BulkSize))};
parse(empty, <<"*", MultiBulkSize/binary>>) ->
    {hold, list_to_integer(binary_to_list(MultiBulkSize))};
parse(read, Message)->
    convert(Message);
parse(empty, Message) ->
    convert(Message).

convert(<<":", Integer/binary>>) ->
    Integer;
convert(<<"+", Message/binary>>) ->
    Message;
convert(Message) ->
    Message.
