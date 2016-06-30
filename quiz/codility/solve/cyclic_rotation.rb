def solution(a, k)
  return a if a.length < 2 || a.length == k

  r = k % a.length
  a.last(r) + a.first(a.length-r)
end
