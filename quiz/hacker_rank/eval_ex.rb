# https://www.hackerrank.com/challenges/eval-ex
# The series expansion of  is given by:
#
#  
#
#  Evaluate  for given values of  by using the above expansion for the first  terms.
#
#  Input Format
#
#  The first line contains an integer , the number of test cases. 
#   lines follow. Each line contains a value of  for which you need to output the value of  using the above series expansion. These input values have exactly  decimal places each.
#
#   Output Format
#
#   Output  lines, each containing the value of , computed by your program.
#
#   Constraints
#
#    
#     
#     Var, Val in Scala and def and defn in Clojure are blocked keywords. The challenge is to accomplish this without either mutable state or direct declaration of local variables.
#
#     Sample Input
#
#     4
#     20.0000
#     5.0000
#     0.5000
#     -0.5000
#     Sample Output
#
#     2423600.1887
#     143.6895
#     1.6487
#     0.6065
#     Explanation
#
#     The output has the computed values of  corresponding to each test case. They are correct up to decimal places and on separate lines.
#
#     Scoring
#
#     All test cases carry an equal weight in the final score. For your solution to pass a given test case, all the values of  computed by you must be within  of the expected answers. This tolerance level has been kept to account for slightly different answers across different languages.
#
# ex: 
# 0th term: x^0/0!
# 1st term: x^1/1!
# 2nd term: x^2/2!
# nth term: x^n/n!

def exp(n, i)
  n ** i
end

def fact(n)
  (1..n).inject(:*) or 1
end

x = -0.5
nums = (0..9).map do |n|
  exp(x.to_f, n) / fact(n).to_f
end

puts (nums.inject(:+))
