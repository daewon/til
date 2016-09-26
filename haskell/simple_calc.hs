-- http://www.willamette.edu/~fruehr/haskell/code/expr.html#parse
----------------------------------------
-- A Flexible Expression Parser in Haskell
-- Fritz Ruehr, Willamette University, October 2001
-- For the 3rd Annual CCSC Northwest Conference
----------------------------------------
 


 --------------------
-- Scanning

data Token = Lit Integer | Op Char
             deriving (Read,Show)

scan [] = []
scan s@(c:cs) | isSpace c = scan cs
              | isOp    c = Op c  : scan cs
              | isDigit c = Lit n : scan r
              | otherwise = error "bad symbol"
                            where [(n,r)] = readDec s
 


 --------------------
-- Parsing

parse f g ts =
  case foldl shred [] ts of
    [t] -> t
    s   -> error "too few ops"
  where shred      s  (Lit n) = f n : s
        shred (a:b:s)  (Op c) = g c b a : s
        shred      s   _ = error "too few args"
 


 --------------------
-- Operators

isOp = (`elem` "+*-")

getOp '+' = (+)
getOp '*' = (*)
getOp '-' = (-)
getOp  _  = error "bad symbol"
 


 --------------------
-- Trees

data Tree a b = Leaf a | Node b (Tree a b) (Tree a b)
                deriving (Read,Show)

fold f g (Leaf n)     = f n
fold f g (Node o l r) = g o (fold f g l) (fold f g r)


parsetree = parse Leaf Node
evaltree  = fold  id  getOp
parsenum  = parse id  getOp
 


 --------------------
-- Printing

inord   = ( paren.unwords, \o l r -> [l,o,r] )
preord  = (       unwords, \o l r -> [o,l,r] )
postord = (       unwords, \o l r -> [l,r,o] )

paren s = "(" ++ s ++ ")"

prtree :: (Show a, Show b) => 
	([String] -> String, [b] -> String -> String -> [String]) -> Tree a b -> String

prtree    = format fold 
parseexpr = format parse

format handler (wrap,ord) =
  handler show (\o l r -> wrap (ord [o] l r))
 


 --------------------
-- Pairing

pairparse = pair (parse id getOp) (parse Leaf Node)
parsepair = parse (pair id Leaf) (pair3 getOp Node)

prtpair disp (n,t) = prtree disp t ++ " = " ++ show n

pair f g x = (f x, g x)

pair3 f g o (l,l') (r,r') = (f o l r, g o l' r')
 


 --------------------
-- Calculation

trace     = fold  (\n->[Leaf n]) step . parsetree
parsecalc = parse (\n->[Leaf n]) step

step o l@(Leaf n:_) r@(Leaf m:_) = 
  Leaf (getOp o n m)
    :  map (Node o (head l)) r
    ++ map (flip (Node o) (last r)) (tail l)

prtcalc disp = eqline . map (prtree disp) . reverse 

eqline = foldr1 (\x y -> x ++ "\n\n  = " ++ y)
 


 --------------------
-- User interaction

run greet done close disp
  = do putStr greet
       str <- getLine
       if done str then do println close
                   else do println (disp str)
                           run greet done close disp

demo = run "\nEnter expressions (blank to stop):\n--> " 
           (=="") "Bye!"

println = putStrLn . ('\n':)
 


 --------------------
-- Test data and sample applications

test  = " 2 3 +  4 - 5  2 *  * "
tokes = scan test
tree  = parsetree tokes

main = demo
-- scan test
-- parsetree $$
-- evaltree  $$

-- parsenum tokes

-- prtree inord (parsetree tokes)
-- parseexpr inord tokes

-- pairparse tokes
-- parsepair tokes
-- prtpair inord $$

-- trace tokes
-- parsecalc tokes
-- prtcalc inord $$
-- putStr $$

-- demo (prtree preord . parsetree . scan)
-- demo (prtcalc inord . trace     . scan)
-- demo (show          . parsenum  . scan)


--------------------
-- End of file
--------------------



