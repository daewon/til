class HelloWorld
  def self.hello(*args)
    arg = args.one? ? args.first : "World"
    "Hello, #{arg}!"
  end
end
