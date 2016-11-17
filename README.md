# Gun connection pool



### How To Use

To build and start playing with the library, execute following commands in the shell:

```bash
$ make app shell
```

Here is a minimal example of using the HTTP connection pool library.

```erlang
%% Creating a pool and adding it to the supervision tree.
Pool =
  #{name => default,
    size => 10,
    connection =>
      #{host => "google.com",
        port => 443,
        options => #{protocols => [http]}}},
ChildSpec = gunc_pool:child_spec(Pool),
supervisor:start_child(whereis(gunc_pool_sup), ChildSpec).

%% Getting a connection and locking it to the current process.
%% If process dies, connection will be released.
Pid = gunc_pool:lock(default),
%% Sending a request and receiving a response.
Ref = gun:get(Pid, "/"),
_ = gun:await(Pid, Ref),
%% Releasing the connection.
gunc_pool:unlock(default, Pid).
```

To learn more about Gun HTTP client library refer to its [documentation][gun-docs].



### License

The source code is provided under the terms of [the MIT license][license].

[license]:http://www.opensource.org/licenses/MIT
[gun-docs]:https://ninenines.eu/docs/en/gun/1.0/guide
