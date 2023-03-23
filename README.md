# Erlang Demo

A demo repository to use as hands-on tutorial for Erlang beginners.

## Installation:

I recommend to use the *asdf* tool for installing Erlang and managing versions.

## 1. Erlang basics

- hello world
- .erl modules
- pattern matching
- spawn process
- messaging process

In the root folder open the Erlang shell: `erl`

In the shell compile the source file: `c(hello).`

Once compiled, call a function: `hello:hello().`

## 2. OTP basics

- rebar3
- OTP application, supervisor
- OTP gen_server as behaviour
- distributed Erlang, *net_kernel* module
- distributed OTP messaging server

In the *./demo/* folder follow the README for the OTP application.
