.PHONY: compile shell clean telnet test

compile:
	rebar3 compile

# Requires: envsubst (part of gettext, available via apt/brew)
shell:
	@test -f .env || (echo "No .env file found. Copy .env.example to .env and fill in your values." && exit 1)
	set -a && . ./.env && set +a && \
	envsubst < config/sys.config.src > config/sys.config && \
	rebar3 shell

clean:
	rebar3 clean

# Connect to the running server for manual testing.
# Start the server first with 'make shell', then run this in a second terminal.
telnet:
	telnet localhost 1337

test:
	@test -f .env || (echo "No .env file found. Copy .env.example to .env and fill in your values." && exit 1)
	set -a && . ./.env && set +a && rebar3 ct
