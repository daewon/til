# you can write to stdout for debugging purposes, e.g.
# puts "this is a debug message"

def solution(s)
    # write your code in Ruby 2.2
    len = s.length
    return 0 if len == 1
    return -1 if len % 2 == 0
    
    mid = s.length / 2
    (0...mid).each do |i|
        j = len - 1 - i
        return -1 if s[i] != s[j]
    end     
    
    mid
end
