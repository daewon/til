# http://blog.seoh.xyz/2015/08/04/wtf-4-parser/

# moclecules = homoatomic | molecules
# homoatomic = atom amount | atom

ahead = -> (pred) { -> (str) { pred.call(str) ? ['', str] : [] } }
satisfy = -> (pred) { -> (str) { pred.call(str) ? [(str[0] || ""), (str[1..-1] || "")] : [] } }

upper = satisfy.call(-> (str) { (str[0] || "") >= 'A' && (str[0] || "") <= 'Z' })
lower = satisfy.call(-> (str) { (str[0] || "") >= 'a' && (str[0] || "") <= 'z' })
digit = satisfy.call(-> (str) { (str[0] || "") >= '0' && (str[0] || "") <= '9' })

string = ahead.call(-> (str) { !str.empty? })

and_p = -> (pa, pb) do
  -> (str) do
    a = pa.call(str)
    return [] if a.empty?

    b = pb.call(a[1])
    return [] if b.empty?

    [a[0] + b[0], b[1]]
  end
end

or_p = -> (pa, pb) do
  -> (str) do
    a = pa.call(str)

    if a.empty?
      pb.call(str)
    else
      a
    end
  end
end

many_p = -> (pa) do
  -> (str) do
    or_p.call(and_p.call(pa, many_p.call(pa)), pa).call(str)
  end
end

atom_p = or_p.call(and_p.call(upper, lower), upper)
amount_p = many_p.call(digit)

homo_p = or_p.call(and_p.call(atom_p, amount_p), atom_p)
mole_p = many_p.call(homo_p)

puts mole_p.call("H20").inspect
