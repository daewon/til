class Node
  attr_accessor :prev_node, :next_node
  attr_reader :value

  def initialize(value)
    @value = value
    @next_node, @prev_node = nil
  end
end

class Lru
  attr_reader :head, :last, :table

  def initialize(max_size=3)
    @table = {}
    @max_size = max_size
    @head = nil
    @last = nil
  end

  def put(key, value)
    if @table[key]
    # update list order
    else
      if @head.nil?
        @head = Node.new([key, value])
        @table[key] = @head
      else
        new_node = Node.new([key, value])
        new_node.next_node = @head

        @head.prev_node = new_node
        @head = new_node

        @table[key] = new_node

        if @last.nil?
          @last = @head.next_node
        else
          if @table.size > @max_size
            old_key, value = @last.value

            @table.delete(old_key)
            @last = @last.prev_node
            @last.next_node = nil
          end
        end
      end
    end
  end

  def get(key)
    node = @table[key]
    if node
      kv = node.value
      old_key, value = kv

      old_prev_node = node.prev_node
      old_next_node = node.next_node

      old_prev_node.next_node = old_next_node
      old_next_node.prev_node = old_prev_node

      node.next_node = @head
      @head.prev_node = node

      value
    end
  end
end
