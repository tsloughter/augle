Erlang OAuth2
=============

JWT
---

Only supports JWT authentication currently, and only tested against Google's API https://developers.google.com/accounts/docs/OAuth2ServiceAccount

```
$ make shell

Erlang R16B01 (erts-5.10.2) [source-bdf5300] [64-bit] [smp:8:8] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V5.10.2  (abort with ^G)
1> ssl:start().
ok
2> application:load(eoauth2).
ok
2> {ok, Iss} = application:get_env(eoauth2, iss).
{ok,<<"............@developer.gserviceaccount.com">>}
6> {ok, Aud} = application:get_env(eoauth2, aud).
{ok,<<"https://accounts.google.com/o/oauth2/token">>}
7> {ok, Scope} = application:get_env(eoauth2, scope).
{ok,<<"https://www.googleapis.com/auth/.........">>}
8> {ok, EncodedPrivateKey} = application:get_env(eoauth2, encoded_private_key).
{ok,<<"-----BEGIN RSA PRIVATE KEY-----\n.........">>}
2> eoauth2:jwt_access_token(Iss, Scope, Aud, EncodedPrivateKey).
{ok, <<".........................">>}
```
