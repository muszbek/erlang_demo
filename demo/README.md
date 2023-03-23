demo
=====

An OTP application

Build
-----

    $ rebar3 compile

Use
-----

	$ rebar3 shell --sname one
	> message_server:reg(myname).
	> message_server:join(two@othermachine).
	> message_server:peers().
	> message_server:chat(mypeer, "ohaithar").
