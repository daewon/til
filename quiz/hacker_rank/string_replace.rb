# Enter your code here. Read input from STDIN. Print output to STDOUT
line = gets.strip

stack = []
line.each_char do |ch|
  if stack.last == ch
    stack.pop
  else
    stack.push ch
  end
end

if stack.empty?
  puts 'Empty String'
else
  puts stack.join
end
