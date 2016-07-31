def has_seven(n)
  while n > 0
    d = n % 10
    return true if d == 7

    n = n / 10
  end

  false
end

def toggle_dir (dir)
  dir == :f ? :d : :f
end

def pingpong(n)
  def pingpong_loop(idx, limit, dir, acc)
    if idx == limit
      acc
    else
      new_dir =
        if idx % 7 == 0 || has_seven(idx)
          toggle_dir(dir)
        else
          dir
        end

      new_acc = new_dir == :f ? acc + 1 : acc - 1
      pingpong_loop(idx + 1, limit, new_dir, new_acc)
    end
  end

  pingpong_loop(1, n, :f, 1)
end


puts pingpong(8) # 6
puts pingpong(22) # 0
puts pingpong(68) # 2
puts pingpong(100) # 2
