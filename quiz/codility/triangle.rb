# you can write to stdout for debugging purposes, e.g.
# puts "this is a debug message"

a = <<-eos
input = [10 2 5 1 8 20]
sorted = [1 2 5 8 10 20]

find p q r 

10 + 5 > 8
5 + 8 > 10
8 + 10 > 5 a
eos

def solution(a)    
    # write your code in Ruby 2.2
    sorted = a.sort    
    s, len = sorted, a.length
        
    for i in 0..(len-3)
        p, q, r = s[i..(i+2)]
        return 1 if p + q > r and q + r > p and r + p > q
    end
    
    0
end
