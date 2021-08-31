defmodule Dotfiles.Proc do
  @moduledoc """
  Simple wrapper on Elixir Port!!
  """

  use GenServer

  alias :erlang, as: Erl

  @doc """
  Launch a GenServer which starts a port and proxify cast and call to
  it using a port protocol with `packet: 4`, (32bits-length+data)
  messages are transmitted throught stdin/out. Input terms are
  encoded using `binary_to_term` and received terms are decoded using
  `term_to_binary`.
  - `cmd` is the shell command to launch the port
  - when the port starts, it automatically receives as first message the `init`
    term if `init !== :no_init`
  - `opts` are options for `Port.open` (for instance `[cd: "/path/"]`)
  - `link_opts` are options for `GenServer.start_link` (for instance `[name: :servername]`)
  - messages received from the port outside of a `GenServer.call`
    context trigger a `event_fun.(event)` call if `event_fun` is not `nil` (default)
  - to allow easy supervision, if the port die with a return code == 0, then
    the GenServer die with the reason `:normal`, else with the reason `:port_terminated`
  """
  def start_link(cmd, init, opts \\ [], link_opts \\ [], event_fun \\ nil),
    do: GenServer.start_link(Dotfiles.Proc, {cmd, init, opts, event_fun}, link_opts)

  @impl true
  def init({cmd, initarg, opts}), do: init({cmd, initarg, opts, nil})

  def init({cmd, initarg, opts, event_fun}) do
    port = Port.open({:spawn, '#{cmd}'}, [:binary, :exit_status, packet: 4] ++ opts)
    if initarg !== :no_init, do: send(port, {self(), {:command, Erl.term_to_binary(initarg)}})
    {:ok, {port, event_fun}}
  end

  @impl true
  def handle_info({port, {:exit_status, 0}}, {port, _} = state), do: {:stop, :normal, state}

  def handle_info({port, {:exit_status, _}}, {port, _} = state),
    do: {:stop, :port_terminated, state}

  def handle_info({port, {:data, b}}, {port, event_fun} = state) do
    if event_fun do
      event_fun.(Erl.binary_to_term(b))
    end

    {:noreply, state}
  end

  @impl true
  def handle_cast(term, {port, _} = state) do
    send(port, {self(), {:command, Erl.term_to_binary(term)}})
    {:noreply, state}
  end

  @impl true
  def handle_call(term, _reply_to, {port, _} = state) do
    send(port, {self(), {:command, Erl.term_to_binary(term)}})

    res =
      receive do
        {^port, {:data, b}} ->
          Erl.binary_to_term(b)

        # catch exit msg and resend it
        {^port, {:exit_status, _}} = exit_msg ->
          send(self(), exit_msg)
          {:error, :port_terminated}
      end

    {:reply, res, state}
  end
end
