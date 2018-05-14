data BinTree a b = Leaf b
                 | Node a (BinTree a b) (BinTree a b)
                 deriving Show

data Value = Const Int
           | Id String
           deriving Show

--2-FP.2
la :: [Char] -> [Char] -> Bool
la xs l = length xs > 0 && (head xs) `elem` l

parseFactor :: [Char] -> (BinTree Char Value, [Char])
parseFactor (x:xs) | x `elem` ['A'..'z'] = (Leaf (Id [x]), xs)
                   | x == '(' = (t, (tail ys))
                   | otherwise = (Leaf (Const (read [x] :: Int)), xs)
                  where
                    (t, ys) = parseExpr xs

parseTerm :: [Char] -> (BinTree Char Value, [Char])
parseTerm xs | la ys "*" = (Node '*' t1 t2, zs)
             | otherwise = parseFactor xs
             where 
              (t1, ys) = parseFactor xs
              (t2, zs) = parseTerm (tail ys)


parseExpr :: [Char] -> (BinTree Char Value, [Char])
parseExpr xs | la ys "+" = (Node '+' t1 t2, zs)
             | otherwise = (t1, ys)
           where 
              (t1, ys) = parseTerm xs
              (t2, zs) = parseExpr (tail ys)

data Token = Number String
           | Identifier String
           | LParen 
           | RParen 
           | Addition 
           | Multiplication
           deriving (Show, Eq)

letter = ['A'..'z']
digit  = ['0'..'9']


tokenize :: String -> [Token]
tokenize []        = []
tokenize (' ':xs)  = tokenize xs
tokenize ('\t':xs) = tokenize xs
tokenize ('(':xs)  = LParen : tokenize xs
tokenize (')':xs)  = RParen : tokenize xs
tokenize ('+':xs)  = Addition : tokenize xs
tokenize ('*':xs)  = Multiplication : tokenize xs
tokenize (x:xs)    | x `elem` letter = (Identifier id) : tokenize rem
                   | x `elem` digit  = (Number n) : tokenize rem2
                   where 
                     (id,rem) = span (`elem` letter) (x:xs)
                     (n,rem2) = span (`elem` digit) (x:xs)