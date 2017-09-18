## Augle

Auth + Google = Augle

---

### Usage

This library is specifcally for service accounts for Google Cloud. It accepts a few different forms of fetching the access tokens for a service account:

* `default`: This will work through the order defined in [Google Application Default Credentials](https://developers.google.com/identity/protocols/application-default-credentials).
* `{file, Path}`: Acts the same as if you have the os variable `GOOGLE_APPLICATION_CREDENTIALS` defined to the `Path`.
* `{file, Path, Scopes}`: Acts the same as above but supports the inclusion of scopes you need credentials for.
* `{metadata, ServiceAccount}`: If all else fails under `default` it will try this method with `<<"default">>` a the `ServiceAccount`. You can instead use it directly and use different service accounts. It must be running on a Google cloud instance (including through AppEngine or Container Engine) or when developing with the [GCE Metadata Emulator](https://medium.com/google-cloud/google-compute-engine-metadata-server-emulator-fe0fb1e5a8b5).

There is a cache that is checked first, keyed on how the credentials were fetched, that sets a timer that sends a message to the cache process to refresh the credentials 10 seconds before they expire. Not expected to work well if you are storing a shit load of service account credentials in the same cache...

Simply add to your `rebar.config` deps list:

```
{deps, [augle]}.
```
