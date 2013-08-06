-module(simple8601).
-export([parse/1]).

parse(String) ->
  Match = re:run(String,
    "^((?:[^T]+)?)((?:T(?:.+))?)$",
    [{capture, all, binary}]
  ) ,
  {match, [_, Raw_date, Raw_time]} = Match ,
  Date = case bit_size(Raw_date) of
    0 -> {} ;
    _ -> parse_date(Raw_date)
  end ,
  Time = case bit_size(Raw_time) of
    0 -> {} ;
    _ -> parse_time(Raw_time)
  end ,
  {Date, Time} .


%% Private

parse_date(Binary) ->
  Match = re:run(Binary,
    "^([0-9]{4})-([0-9]{2})-([0-9]{2})$",
    [{capture, all, binary}]
  ) ,
  case Match of
    {match, [_, Year, Month, Day]} ->
      {binary_to_number(Year), binary_to_number(Month), binary_to_number(Day)} ;
    nomatch -> {error, unknown_time_format}
  end .


parse_time(Binary) ->
  Match = re:run(Binary,
    "^([0-9]{2}):([0-9]{2}):([0-9]+(?:\.[0-9]+)?)((?:.+)?)$",
    [{capture, all, binary}]
  ) ,
  case Match of
    {match, [_, Hours, Minutes, Seconds, Timezone]} ->
      {binary_to_number(Hours), binary_to_number(Minutes), binary_to_number(Seconds), parse_timezone(Timezone)} ;
    nomatch -> {error, unknown_time_format}
  end .


parse_timezone(Binary) ->
  Match = re:run(Binary,
    "^(Z|(?:\+|\-)[0-9]{2}:[0-9]{2})$",
    [{capture, all, binary}]
  ) ,
  case Match of
    {match, [_, Timezone]} ->
      Timezone ;
    nomatch -> {error, unknown_time_format}
  end .


binary_to_number(Binary) ->
  S = binary_to_list(Binary),
  try list_to_float(S)
  catch
    error:badarg ->
      list_to_integer(S)
  end .

