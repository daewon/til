# you can write to stdout for debugging purposes, e.g.
# puts "this is a debug message"

def solution(s)
  # write your code in Ruby 2.2
  stack = []
  open_pairs = Hash[ * %W{( ) [ ] { }} ]
  close_pairs = Hash[ open_pairs.map { |k, v| [v, k] } ]

  s.each_char do |ch|
    stack << ch if open_pairs[ch]

    if close_pairs[ch]
      open_char = stack.pop
      return 0 if close_pairs[ch] != open_char
    end
  end

  stack.empty? ? 1 : 0
end
