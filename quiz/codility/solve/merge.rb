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

class Heap
  def initialize
    @heap, @idx = [], -1
  end

  def push(n)
    @heap << n
    @idx += 1

    bubble_up(@idx)
  end

  def pop()
    return nil if @idx == -1

    fst = @heap[0]
    @heap[0] = @heap[@idx]
    @heap[@idx] = nil
    @idx -= 1

    bubble_down(0)

    fst
  end

  def swap(i, j)
    tmp = @heap[i]
    @heap[i] = @heap[j]
    @heap[j] = tmp
  end

  def bubble_down(idx)
    loop do
      l_idx, r_idx = idx * 2 + 1, idx * 2 + 2
      l_value, r_value = @heap[l_idx], @heap[r_idx]
      value = @heap[idx]

      break if l_value.nil?

      if r_value.nil?
        if l_value < value
          swap(l_idx, idx)
          idx = l_idx
        else
          break
        end
      else
        if l_value < r_value
          swap(l_idx, idx)
          idx = l_idx
        else
          swap(r_idx, idx)
          idx = r_idx
        end
      end
    end
  end

  def bubble_up(idx)
    loop do
      p_idx = idx / 2

      if @heap[p_idx] > @heap[idx]
        swap(p_idx, idx)
        idx = p_idx
      else
        break
      end
    end
  end
end

def solution(a)
  # write your code in Ruby 2.2
  heap = Heap.new
  len = a.length

  i = 0
  while i < len do
    heap.push(a[i])
    i += 1
  end

  i = 0
  while i < len do
    a[i] = heap.pop
    i += 1
  end

  i = 0
  while i < len-2 do
    p, q, r = a[i], a[i+1], a[i+2]
    return 1 if p + q > r and q + r > p and r + p > q
    i += 1
  end
  0
end
