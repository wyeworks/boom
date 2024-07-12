defmodule TestUtils do
  def above_version?(boundary) do
    [major, minor, patch] = split_version(boundary)
    [system_major, system_minor, system_patch] = split_version(System.version())

    system_major > major or
      (system_major == major and system_minor > minor) or
      (system_major == major and system_minor == minor and system_patch > patch)
  end

  defp split_version(version) when is_binary(version) do
    version |> String.split(".") |> Enum.map(&String.to_integer/1)
  end
end
