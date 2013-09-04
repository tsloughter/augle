Erlang OAuth2
=============

JWT
---

Only supports JWT authentication currently, and only tested against Google's API https://developers.google.com/accounts/docs/OAuth2ServiceAccount

To get going quickly with service authentication by creating a JWT copy `config/example.sys.config` to `config/sys.config` and insert the appropriate values for the oauth2 service you are authenticating against.

Running `make shell` with pull the deps if needed, compile the project and load an Erlang shell with the proper paths set. It assumes a `config/sys.config` file exists, which you can then use to retrieve the necessary variables to pass to `eoauth2:jwt_access_token/6` to retrieve a token.

```
$ make shell

Erlang R16B01 (erts-5.10.2) [source-bdf5300] [64-bit] [smp:8:8] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V5.10.2  (abort with ^G)
1> ssl:start().
ok
2> application:load(eoauth2).
ok
3> {ok, Iss} = application:get_env(eoauth2, iss).
{ok,<<"............@developer.gserviceaccount.com">>}
4> {ok, Aud} = application:get_env(eoauth2, aud).
{ok,<<"https://accounts.google.com/o/oauth2/token">>}
5> {ok, Scope} = application:get_env(eoauth2, scope).
{ok,<<"https://www.googleapis.com/auth/.........">>}
6> {ok, EncodedPrivateKey} = application:get_env(eoauth2, encoded_private_key).
{ok,<<"-----BEGIN RSA PRIVATE KEY-----\n.........">>}
7> {ok, Host} = application:get_env(eoauth2, host).
{ok,<<"https://........">>}
8> {ok, Path} = application:get_env(eoauth2, path).
{ok,<<"/......">>}
9> eoauth2:jwt_access_token(Host, Path, Iss, Scope, Aud, EncodedPrivateKey).
{ok, <<".........................">>}
```
