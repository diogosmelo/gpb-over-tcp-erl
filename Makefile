.PHONY: compile shell clean telnet

compile:
	rebar3 compile

shell:
	rebar3 shell

clean:
	rebar3 clean

# Connect to the running server for manual testing.
# Start the server first with 'make shell', then run this in a second terminal.
telnet:
	telnet localhost 1337
