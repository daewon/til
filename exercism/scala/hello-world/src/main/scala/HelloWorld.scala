object HelloWorld {
  def hello(args: String*) = "Hello, " + args.headOption.fold("World!")(_ + "!")
}
