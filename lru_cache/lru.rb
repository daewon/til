class Node
  def initialize(value)
    @value = value
    @prev, @next = nil
  end
end

class LRUCache
  def initialize(size)
    @size = size
    @map, @list = {}, nil
  end

  def get(k)
  end


  def set(k, v)
  end


  def expire()

  end
end
