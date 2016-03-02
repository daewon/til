# you can write to stdout for debugging purposes, e.g.
# puts "this is a debug message"

# [2, 1, 5, 1, 2, 2, 2]
def solution(k, m, a)
  @min = 999999999999999
  @prefix_sum = a.inject([]) { |acc, c| acc << c + (acc.last || 0) }
  @a = a
  @k = k
  @len = a.length
  @memo = {}

  def trav(idx, p_idx, prev, depth)
    @memo["#{idx}_#{p_idx}"] if @memo["#{idx}_#{p_idx}"]
    if depth == @k-1
      # sum = a[idx...@len].inject(0) { |a, c| a + c }
      sum =
        if idx > 0
          @prefix_sum[@len-1] - @prefix_sum[idx - 1]
        else
          @prefix_sum[@len-1]
        end

      @min = [@min, [prev, sum].max].min

      @memo["#{idx}_#{p_idx}"] = @min
    else
      (idx...(@len)).each do |ci|
        #sum = a[idx...ci].inject(0) { |a, c| a + c }
        sum =
          if idx > 0
            @prefix_sum[ci-1] - @prefix_sum[idx - 1]
          else
            @prefix_sum[ci-1]
          end

        trav(ci, idx, [sum, prev].max, depth + 1)
      end
    end
  end

  trav(0, 0, 0, 0)

  @min
end
