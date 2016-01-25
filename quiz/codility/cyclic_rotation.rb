def solution(a, k)
    len = a.length
    return a if k == 0 || len == 0 || k == len

    k = k % len if k > len
    a[(len-1-(k-1))..-1] + a[0..(len-1-k)]
end

