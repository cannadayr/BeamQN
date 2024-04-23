defmodule BeamQN do
  @moduledoc """
  Documentation for `BeamQN`.
  """

  @doc """
  Hello world.

  ## Examples

      iex> BeamQN.hello()
      :world

  """
  defdelegate call(n,x), to: :beamqn
  defdelegate call(n,x,opt), to: :beamqn
  defdelegate eval(n), to: :beamqn
  defdelegate eval(n,opt), to: :beamqn
  defdelegate make(n), to: :beamqn
  defdelegate make(n,opt), to: :beamqn
  defdelegate read(n), to: :beamqn
  defdelegate read(n,opt), to: :beamqn
end
