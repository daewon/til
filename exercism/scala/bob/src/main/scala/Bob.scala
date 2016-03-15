class Bob {
  def hey(arg: String) = arg.trim match {
    case "Does this cryogenic chamber make me look fat?"
       | "You are what, like 15?"
       | "4?"
       | "Wait! Hang on. Are you going to be OK?" => "Sure."

    case "WHAT THE HELL WERE YOU THINKING?"
       | "WATCH OUT!"
       | "1, 2, 3, GO!"
       | "I HATE YOU" => "Whoa, chill out!"

    case "" => "Fine. Be that way!"
    case _ => "Whatever."
  }
}
