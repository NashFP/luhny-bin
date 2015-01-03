require Record

defmodule Luhn do
  # New approach:
  # - Convert string to list of records
  # - Reverse it
  # - Transform the list
  # - Recurse with one final output character and the tail of the list
  # - When we transform, we can look first for 14 digits; if < 14, no math
  # - The record should contain:
  # - - character
  # - - is_digit
  # - - digit_value
  # - - doubled_digit_sum
  # - - is_masked

  Record.defrecord :character,
    value: nil, 
    is_digit: false, 
    digit_value: 0, 
    doubled_digit_sum: 0, 
    is_masked: false

  def double_digit_value(n) do
    n2 = n * 2
    div(n2, 10) + rem(n2, 10)
  end

  def from_character(c) do
    if character(c, :is_masked), do: 'X', else: character(c, :value)
  end

  def get_slice(cs, size) do
    get_slice(cs, size, [])
  end

  def get_slice(rest, 0, result), do: {:ok, Enum.reverse(result), rest}

  def get_slice([], _size, result), do: {:error, :not_found}

  def get_slice([c|cs], size, result) do
    if character(c, :is_digit) do
      get_slice(cs, size - 1, [c|result])
    else
      if is_delimiter(character(c, :value)) do
        get_slice(cs, size, [c|result])
      else
        {:error, :not_found}
      end
    end
  end

  def handle_line(input) do
    input
    |> String.to_char_list
    |> Enum.map(&to_character/1)
    |> transform
    |> Enum.map(&from_character/1)
    |> to_string
  end

  def id(x), do: x

  def is_delimiter(c) do
    Enum.member?(' -', c)
  end

  def is_luhn(cs) do
    doubles = Stream.cycle([false, true])
    revCs = cs
            |> Enum.filter(&(character(&1, :is_digit)))
            |> Enum.reverse
    values = for {double, char} <- Enum.zip(doubles, revCs) do
      if double do
        character(char, :doubled_digit_sum)
      else
        character(char, :digit_value)
      end
    end
    rem(Enum.sum(values), 10) == 0
  end

  def main(_args) do
    read_line
  end

  def mask(chars) when is_list(chars) do
    Enum.map(chars, &mask/1)
  end

  def mask(char) when Record.is_record(char) do
    case character(char, :is_digit) do
      true -> character(char, is_masked: true)
      _    -> char
    end
  end

  def read_line do
    case String.rstrip(IO.read(:stdio, :line)) do
      "" -> :ok
      line ->
        IO.puts handle_line(line)
        read_line
    end
  end

  def to_character(c) do
    is_digit = c >= ?0 and c <= ?9
    digit_value = if is_digit, do: c - ?0, else: 0
    character(
      value: c,
      is_digit: c >= ?0 and c <= ?9,
      digit_value: digit_value,
      doubled_digit_sum: double_digit_value(digit_value)
    )
  end

  def transform(cs), do: transform(cs, [])

  def transform([], result), do: Enum.reverse(result)

  def transform(chars, result) do
    [newC|newCs] = chars
    |> transform_chunk(14)
    |> transform_chunk(15)
    |> transform_chunk(16)

    transform(newCs, [newC|result])
  end

  def transform_chunk(cs, size) do
    case get_slice(cs, size) do
      {:ok, slice, rest} ->
        f = if is_luhn(slice), do: &mask/1, else: &id/1
        f.(slice) ++ rest
      {:error, _} ->
        cs
    end
  end
end
