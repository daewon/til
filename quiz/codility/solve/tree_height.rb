# you can write to stdout for debugging purposes, e.g.
# puts "this is a debug message"

def solution(t)
  # write your code in Ruby 2.2
  depth = 0
  childs = []

  childs << t.l if t.l
  childs << t.r if t.r

  while not childs.empty? do
    depth += 1

    cc = []
    childs.each do |t|
      cc << t.l if t.l
      cc << t.r if t.r
    end

    childs = cc
  end

  depth
end
