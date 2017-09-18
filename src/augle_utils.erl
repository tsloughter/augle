%%%-------------------------------------------------------------------
%%% @author Tristan Sloughter <t@crashfast.com>
%%% @copyright (C) 2017, Tristan Sloughter
%%% @doc
%%%
%%% @end
%%% Created : 4 Sep 2017 by Tristan Sloughter <t@crashfast.com>
%%%-------------------------------------------------------------------
-module(augle_utils).

-export([decode_json/1,
         atoms/0]).

decode_json(Result) ->
    jsx:decode(Result, [return_maps, {labels, attempt_atom}]).

%% HACK: no other way I know of to make all the expected atoms to be known
%% from the beginning on `attempt_atom` in jsx decode works how we want
atoms() ->
    [access_token, expires_in, token_type, id_token,
     error, error_description].
