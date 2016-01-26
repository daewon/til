# you can write to stdout for debugging purposes, e.g.
# puts "this is a debug message"

def solution(a)
    map = {}
    a.each do |n|
        if n > 0 
            map[n] = true
        end
    end
    
    i = 1
    find = 0
    while i < 100002 do
        if not map[i] 
            find = i
            break
        end
        i += 1
    end
    
    find
end
