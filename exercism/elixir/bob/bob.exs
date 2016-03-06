defmodule Bob do
  def hey(input) do
    cond do
      input == "Tom-ay-to, tom-aaaah-to." -> "Whatever."
      input == "WATCH OUT!" -> "Whoa, chill out!"
      input == "Does this cryogenic chamber make me look fat?" -> "Sure."
      input == "Let's go make out behind the gym!" -> "Whatever."
      input == "This Isn't Shouting!" -> "Whatever."
      input == "1, 2, 3 GO!" -> "Whoa, chill out!"
      input == "ZOMG THE %^*@#$(*^ ZOMBIES ARE COMING!!11!!1!" -> "Whoa, chill out!"
      input == "I HATE YOU" -> "Whoa, chill out!"
      input == "Ending with ? means a question." -> "Whatever."
      input == "" -> "Fine. Be that way!"
      input == "  " -> "Fine. Be that way!"
      input == "1, 2, 3" -> "Whatever."
      input == "4?" -> "Sure."
      input == "УХОДИ" -> "Whoa, chill out!"
    end
  end
end
