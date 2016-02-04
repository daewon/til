# you can write to stdout for debugging purposes, e.g.
# puts "this is a debug message"

def solution(s, p, q)
    chm = { 'A' => 1, 'C' => 2, 'G' => 3, 'T' => 4 } # char to int map
    rng = {} # store { a: {max: -1, min: 9}, {..}, {..} }
    
    s.each_char.each_with_index do |ch, i| 
        rng[ch] = { min: 5,  max: -1 } unless rng[ch] 
        m = rng[ch]
        min, max = m[:min], m[:max]
        
        min = i if i < min
        max = i if i > max
        
        rng[ch] = { min: min, max: max }
    end
    # CAGCCTA => [2, 1, 3, 2, 2, 4, 1]
    # [2, 4], [5, 5], [0, 6]
    # puts rng.inspect
    
    min_map = []    
    ['A', 'C', 'G', 'T'].each do |ch|
        (rng[ch][:min]..rng[ch][:max]).each { |i| min_map[i] = ch }
    end
    
    # puts min_map.inspect
    indexes = p.zip(q).map do |i, j|
        chm[min_map[j]]
    end   
    
    indexes
end
