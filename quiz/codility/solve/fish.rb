# you can write to stdout for debugging purposes, e.g.
# puts "this is a debug message"

def solution(a, b)
  downstream = a.zip(b).map { |size, dir| [size, dir] }
  alive = 0
  upstream = []

  loop do
    if upstream.empty?
      size, dir = downstream.pop
      if dir == 1 # keep alive
        alive += 1
      else
        upstream << [size, dir]
      end
    else
      down_size, down_dir = downstream.last
      up_size, up_dir = upstream.last

      if up_dir == down_dir
        upstream << downstream.pop
      else
        if up_size > down_size
          downstream.pop
        else
          upstream.pop
        end
      end
    end

    break if downstream.empty?
  end

  downstream.size + upstream.size + alive
end
