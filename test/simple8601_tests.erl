-module(simple8601_tests) .
-compile(export_all) .

-include_lib("eunit/include/eunit.hrl") .

format_test_() ->
  F = fun simple8601:format/1,
  [{
    "formats a {Date, {}}",
    ?_assertEqual(<<"2013-08-07">>, F({{2013, 8, 7}, {}}))
  }, {
    "formats a {{}, Time}",
    ?_assertEqual(<<"10:11:12Z">>, F({{}, {10, 11, 12}}))
  }, {
    "formats a {Date, Time}",
    ?_assertEqual(<<"2013-08-07T10:11:12Z">>, F({{2013, 8, 7}, {10, 11, 12}}))
  }] .

parse_test_() ->
  F = fun simple8601:parse/1,
  [{
    "parses a YYYY-MM-DD",
    ?_assertEqual({{2013, 8, 7}, {}}, F("2013-08-07"))
  }, {
    "parses a YYYY-MM-DDTHH:MM::SS",
    ?_assertEqual({{2013, 8, 7}, {10, 11, 12}}, F("2013-08-07T10:11:12"))
  }, {
    "parses a HH:MM::SS",
    ?_assertEqual({{}, {10, 11, 12}}, F("10:11:12"))
  }] .

parse_unknown_format_test_() ->
  F = fun simple8601:parse/1,
  E = {{error, unknown_format}, {}},
  [{
    "raises an unknown_format when parsing YYYYMMDD",
    ?_assertEqual(E, F("20130807"))
  }, {
    "raises an unknown_format when parsing YYYY-MM-DDT",
    ?_assertEqual(E, F("2013-08-07T"))
  }] .

