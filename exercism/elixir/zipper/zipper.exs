defmodule BinTree do
  @moduledoc """
  A node in a binary tree.

  `value` is the value of a node.
  `left` is the left subtree (nil if no subtree).
  `right` is the right subtree (nil if no subtree).
  """
  @type t :: %BinTree{  value: any,
                        left: BinTree.t | nil,
                        right: BinTree.t | nil }
  defstruct value: nil, left: nil, right: nil
end

defmodule Zipper do
  # if trail is [], the position of zipper is root
  defstruct value: nil, left: nil, right: nil, trail: []

  def from_tree(bt) do
    %Zipper{value: bt.value, left: bt.left, right: bt.right}
  end

  def to_tree(%Zipper{trail: []} = z) do
    %BinTree{value: z.value, left: z.left, right: z.right}
  end
  def to_tree(%Zipper{trail: _} = z), do: z |> up |> to_tree

  def value(z), do: z.value

  def left(%Zipper{left: nil}), do: nil
  def left(%Zipper{left: left}=z) do
    %Zipper{value: left.value, left: left.left, right: left.right,
            trail: [{:left, z.value, z.right} | z.trail]}
  end

  def right(%Zipper{right: nil}), do: nil
  def right(%Zipper{right: right}=z) do
    %Zipper{value: right.value, left: right.left, right: right.right,
            trail: [{:right, z.value, z.left} | z.trail]}
  end

  def up(%Zipper{trail: []}), do: nil
  def up(%Zipper{trail: [{:left, val, right} | trail]} = z) do
    left = %BinTree{value: z.value, left: z.left, right: z.right}
    %Zipper{value: val, left: left, right: right, trail: trail}
  end
  def up(%Zipper{trail: [{:right, val, left} | trail]} = z) do
    right = %BinTree{value: z.value, left: z.left, right: z.right}
    %Zipper{value: val, left: left, right: right, trail: trail}
  end

  def set_value(z, v), do: %{z | value: v}
  def set_left(z, l), do: %{z | left: l}
  def set_right(z, r), do: %{z | right: r}
end
