all:
  BUILD +formatter-test
  BUILD +analisys-test
  BUILD +unit-test

ci:
  BUILD +formatter-test
  BUILD +unit-test

analisys-test:
  FROM +test-setup

  RUN mix dialyzer --format dialyxir 

formatter-test:
  FROM +test-setup

  RUN mix format --check-formatted
  RUN mix compile --warning-as-errors
  RUN mix credo --strict

unit-test:
  FROM +test-setup

  RUN mix test

setup-base:
  FROM hexpm/elixir:1.11.3-erlang-23.2.5-alpine-3.13.1

  ENV ELIXIR_ASSERT_TIMEOUT=10000

  WORKDIR /dotfiles

  COPY mix.exs .
  COPY mix.lock .

test-setup:
  FROM +setup-base

  ENV MIX_ENV=test

  COPY .formatter.exs .
  RUN mix local.rebar --force \
      && mix local.hex --force \
      && mix do deps.get, deps.compile

  COPY --dir config lib test ./
