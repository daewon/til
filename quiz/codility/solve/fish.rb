# you can write to stdout for debugging purposes, e.g.
# puts "this is a debug message"

def solution(a, b)
  len = a.length
  fish = nil
  count = 0

  (0...len).each do |idx|
    if b[idx] == 0
      if fish.nil?
        count += 1
      else
        if fish > count
        else

          count += 1
        end
      end
    else
      fish = a[idx]
    end
  end

  count + 1
end
