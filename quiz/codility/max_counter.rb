# you can write to stdout for debugging purposes, e.g.
# puts "this is a debug message"

# 1, [2, 1, 1, 2, 1]
def solution(n, a)
    counter = [0] * n
    
    max = 0
    start = 0
    a.each do |k|
        if k >= 1 and k <= n 
            if counter[k-1] < start 
                counter[k-1] = start + 1
            else
                counter[k-1] += 1
            end
            
            max = counter[k-1] if counter[k-1] > max
        elsif k == n + 1            
            start = max
        end
    end
    
    counter.each_with_index do |_, i| 
        counter[i] = start if counter[i] < start
    end
end
