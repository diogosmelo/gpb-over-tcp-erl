.PHONY: compile shell clean telnet test docker-up docker-down

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
	@set -a; \
	. ./.env; \
	[ -f .localstack.env ] && . ./.localstack.env; \
	set +a; \
	rebar3 ct

# Start all local infrastructure and create a KMS (Key Management Service) key in LocalStack.
docker-up:
	@test -f .env || (echo "No .env file found. Copy .env.example to .env and fill in your values." && exit 1)
	docker compose up -d
	@echo "Waiting for LocalStack (Key Management Service) to be ready..."
	@set -a && . ./.env && set +a && \
	until aws --endpoint-url=http://localhost:4566 --region $$AWS_DEFAULT_REGION kms list-keys > /dev/null 2>&1; do \
		sleep 1; \
	done && \
	KEY_ARN=$$(aws --endpoint-url=http://localhost:4566 --region $$AWS_DEFAULT_REGION kms create-key --query 'KeyMetadata.Arn' --output text) && \
	echo "KMS_KEY_ARN=$$KEY_ARN" > .localstack.env && \
	echo "LocalStack ready. KMS key ARN (Amazon Resource Name): $$KEY_ARN"

docker-down:
	docker compose down
