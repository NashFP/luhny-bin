defmodule LuhnTest do
  use ExUnit.Case

  test "handle non-digits" do
    assert Luhn.handle_line("hello") == "hello"
  end

  test "handle match of 14" do
    assert Luhn.handle_line("56613959932537") == "XXXXXXXXXXXXXX"
  end
  
  test "handle match of 15" do
    assert Luhn.handle_line("508733740140655") == "XXXXXXXXXXXXXXX"
  end

  test "handle match of 16" do
    assert Luhn.handle_line("6853371389452376") == "XXXXXXXXXXXXXXXX"
  end

  test "handle mismatch of 14" do
    assert Luhn.handle_line("49536290423965") == "49536290423965"
  end

  test "handle spaces in number" do
    assert Luhn.handle_line("4352 7211 4223 5131") == "XXXX XXXX XXXX XXXX"
  end
end
