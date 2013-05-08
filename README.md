## icerl

Erlang protocol implementation for [Ice](http://www.zeroc.com/). This is an Erlang library which aims to communicate with Ice servers and clients. It's not implemented by Ice official team, and to compare with the other Ice languages implementations, it's not based on Ice core library. Instead, it implements [Ice message protocol](http://doc.zeroc.com/display/Ice/The+Ice+Protocol).

## Status

`icerl` is still under heavy development, there're many features which are still not implemented. But it's a good start, you can use this library to create a simple Ice server/client, to provide some methods invoked by Ice standard applications.

## Features

* Support to create an Ice server and provide methods to invoke
* Support to create an Ice client to invoke methods on Ice server
* Basic data typs encoding, i.e: byte/int/long/string/sequence/dictionary
* Request message encoding/decoding
* Reply message encoding/decoding

## TODO

Currently `icerl` does not support to generate erlang codes from slice file, which means it can not encode arguments and decode results for invocations. **You have to encode/decode arguments and results by yourself.** I suppose if i will still work on this project, i will solve this problem first.

## Dependence

* [`kl_tserver`](https://github.com/kevinlynx/erlang-tcpserver)

## Usage

* Download `kl_tserver` and `icerl` libraries first
* Compile `kl_tserver` and add the `ebin` directory to `erl`
* Compile `icerl` in the library root directory:

        erl -make

* Start `erl` by specifying `kl_tserver` and `icerl` libraries `ebin` directories:

        erl -pa kl_tserver_dir/ebin -pa icerl_dir/ebin

* create your `icerl` application

There are some simple examples in `test` directory which can show you the basic usage, i.e:

    -module(testadapter).
    -compile(export_all).
    -export([start/0]).

    ice_isA(Arg) ->
        {T, _} = stream:read(string, Arg),
        io:format("ice_isA: ~s~n", [T]),
        [stream:write(true)].

    say_hello(_Arg) ->
        io:format("hello world~n"),
        [].

    start() ->
        Objects = [{"hello", [{"ice_isA", fun ? MODULE:ice_isA/1}, 
                        {"sayHello", fun ?MODULE:say_hello/1}]}],
        adapter:create("Hello", 10000, Objects).


