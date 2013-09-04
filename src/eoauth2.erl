%%%-------------------------------------------------------------------
%%% @author Tristan Sloughter <tristan@heroku.com>
%%% @copyright (C) 2013, Tristan Sloughter
%%% @doc
%%%
%%% @end
%%% Created : 3 Sep 2013 by Tristan Sloughter <tristan@heroku.com>
%%%-------------------------------------------------------------------
-module(eoauth2).

-export([jwt_access_token/6]).

jwt_access_token(Host, Path, Iss, Scope, Aud, EncodedPrivateKey) ->
    eoauth2_jwt:access_token(Host, Path, Iss, Scope, Aud, EncodedPrivateKey).
