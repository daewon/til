def solution(h)
  len = h.length
  stack = []
  stones = []

  h.each do |h|
    while stack.last and stack.last > h do
      stack.pop
    end

    if stack.last and stack.last == h
      # pass
    else
      stack << h
      stones << h
    end
  end

  stones.length
end
