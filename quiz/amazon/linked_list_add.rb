a = [1, [2, [3, [4, nil]]]]
b = [9, [8, [9, nil]]]

def print_list(head)
  current = head
  aa = "nil"
  loop do
    if current[1]
      aa = "#{aa} -> #{current[0]}"
      current = current[1]
    else
      break
    end
  end
  puts aa
end

def sum(a, b)
  stack_a = []
  stack_b = []

  current = a
  loop do
    if current[1]
      stack_a << current[0]
      current = current[1]
    else
      stack_a << current[0]
      break
    end
  end

  current = b
  loop do
    if current[1]
      stack_b << current[0]
      current = current[1]
    else
      stack_b << current[0]
      break
    end
  end

  node = []
  carry = 0

  puts stack_b.inspect
  puts stack_a.inspect

  loop do
    if not stack_a.empty? and not stack_b.empty?
      va = stack_a.pop
      vb = stack_b.pop

      val = va + vb + carry
      if val > 9
        carry = 1
      else
        carry = 0
      end
      node = [val % 10, node]
    elsif stack_a.empty? and not stack_b.empty?
      vb = stack_b.pop + carry
      node = [vb, node]
    elsif stack_b.empty? and not stack_a.empty?
      va = stack_a.pop + carry
      node = [va, node]
    end

    break if stack_a.empty? and stack_b.empty?
  end

  node
end

print_list(sum(a, b))
