{stream, 0} =
  System.cmd("sudo", ["ls"],
    env: [{"SUDO_ASKPASS", "./askpass.sh"}],
    into: IO.stream(:stdio, :line)
  )

stream
|> Enum.to_list()
|> IO.inspect()
