

input = do {
    xs <- getLine;
    ns <- pure ( (filter (\x -> (x>='0') && (x<='9')) xs));
    return (read ns :: Int);
}


loop n = do {
    if n==0 then
        return 0;
    else do {
        x <- input;
        y <- loop (n-1);
        return (x + y);
    }
}

adder :: IO ()
adder = do {
    putStrLn "inserisci quanti numeri vuoi sommare: ";
    n <- input;
    putStrLn "inserisci i numeri da sommare: ";
    m <- loop n;
    putStrLn ("somma: "++show m);
}



-- Esercizio 2 
-- newtype Semi a = S (a,[a])
--     deriving Show
-- instance Applicative Semi where
--     pure x = S (x,[1])
--     (S(f, x)) <*> (S (a, y))
--         |mod f a == 0 = S (f a, x++y)



-- prova x = filter (\s->sum s == x) $ subsets  (filter (\n->mod x n ==0) [1..x-1])
-- p x = pure (++) <*> list <*> list
--     where list = map (:[]) (filter (\n->mod x n ==0) [1..x-1])

--inutile perche ci da solo segmenti consecutivi
-- loop1 :: ([a] -> [a]) -> [a] -> [[a]]
-- loop1 f xs@(x:txs) = xs : loop1 f (f xs)
-- loop1 f [] = []


-- segments :: [a] -> [[a]]
-- segments x = (concat . (map (loop1 tail)  . ( loop1 init) ) ) x ++ [[]] 


subsets []  = [[]]
subsets (x:xs) =  subsets xs ++ ( pure (++) <*>[[x]] <*>  subsets xs)

semiPerfetti x = filter (\s->sum s == x) $ subsets $ divisori
    where divisori = (filter (\n->mod x n ==0) [1..x-1])
-- pure (++) <*> subsets xs <*> (map (x:) (subsets xs))
-- subsets xs ++ pure (x:) <*> subsets xs

-- semiPerfetti x = filter (\s->sum s == x)
-- *Main> semiPerfetti 24                
-- [[4,8,12],[2,4,6,12],[1,3,8,12],[1,2,3,6,12],[1,2,3,4,6,8]]
-- *Main> :l hw4.hs 
-- [1 of 1] Compiling Main             ( hw4.hs, interpreted )
-- Ok, one module loaded.
-- *Main> semiPerfetti 24
-- [[4,8,12],[2,4,6,12],[1,3,8,12],[1,2,3,6,12],[1,2,3,4,6,8]]

-- subsets []  = [[]]
-- subsets (x:xs) = subsets xs ++ map (x:) (subsets xs)

-- esercizio 3// left corretto, rigth errato
data Either' a b = Left' b | Right' a
    deriving Show

data Error m = DivisionByZero | NegativeSubstraction m
    deriving Show

instance Functor (Either' a) where
    fmap _ (Right' e)  = Right' e
    fmap f ( Left' v) =  Left' (f v)

instance Applicative (Either' e) where
    pure x =  Left' x
    Right'  e  <*>  _  = Right' e
    Left' f  <*>  r  =  fmap f r
    
instance Monad (Either' e) where
    (Right' e) >>= _ = Right' e
    (Left' v) >>= f = f v


safeDiv _ 0 = Right' DivisionByZero 
safeDiv m n = Left' (m `div` n) 

safeSub m n 
    | n>m = Right' $  NegativeSubstraction $ "[Sub (Const ("++ (show m) ++ ") (Const ("++ (show n)++")] the first argument is greater than the second "
    |otherwise = Left' (m-n)

    
data Term = Const Int | Div Term Term | Sub Term Term |Sum Term Term | Mul Term Term

-- eval :: Term -> Either' [Char] Int


-- -- eval (Const a) = Left' a
-- *Main> eval ( Sum (Const 4) (Sub (Const 6 ) (Const 10)))
-- Right' (NegativeSubstraction "[Sub (Const 6) (Const 10)] il primo argomento e' minore del secondo ")

-- eval (Div t u) = case eval t of
--     Right' b -> Right' b
--     Left' a -> case eval u of
--         Right' x-> Right' x
--         Left' b -> if b==0 then Right' "errore: divisione per 0"
--         else Left' (a `div` b) 
-- eval :: Monad m => Term -> m Int
eval (Const a) = Left' a

eval (Div t u) = eval t >>= \a ->
                 eval u >>= \b ->
                 safeDiv a b

eval (Sub t u) = eval t >>= \a ->
                 eval u >>= \b ->
                 safeSub a b

eval (Sum t u) = eval t >>= \a ->
                 eval u >>= \b ->
                 return ( a+ b)

eval (Mul t u) = eval t >>= \a ->
                 eval u >>= \b ->
                 return (a * b)

-- *Main> eval (Sub (Sub (Const 2) (Const 2)) (Const 9))        
-- Right' "errore: il primo argomento e' minore del secondo "
-- *Main> eval (Sub (Sub (Const 20) (Const 2)) (Const 9))
-- Left' 9
-- *Main>

-- *Main> eval (Sum (Const 4) (Const 9))                        
-- Left' 13
-- *Main> eval (Mul (Const 4) (Const 9))
-- Left' 36

-- eval (Mul (Sub (Const 20) (Const 2)) (Const 9))

-- *Main> eval (Mul (Sub (Sub (Const 5) (Const 2)) (Const 2)) (Div (Sub (Const 3) (Const 1)) (Const 3))) 
-- Left' 0
-- *Main> eval (Mul (Sub (Sub (Const 5) (Const 2)) (Const 2)) (Div (Sub (Const 3) (Const 4)) (Const 3)))
-- Right' "errore: il primo argomento e' minore del secondo "


-- *Main> eval (Mul (Sub (Sub (Const 5) (Const 2)) (Const 2)) (Div (Const 9) (Const 3)))
-- Left' 3
-- *Main> eval (Mul (Sub (Sub (Const 5) (Const 2)) (Const 2)) (Div (Sub (Const 3) (Const 1))) (Const 3)))

-- *Main> eval (Mul (Sub (Sub (Const (-5)) (Const 2)) (Const 2)) (Div (Sub (Const 3) (Const 4)) (Const (-8))))
-- Right' NegativeNumber



-- *Main> eval (Mul (Sub (Sub (Const 5) (Const 2)) (Const 2)) (Div (Sub (Const 3) (Const 4)) (Const 0)))      
-- Right' (NegativeSubstraction "[Sub (Const (3) (Const (4)] the first argument is greater than the second ")
-- *Main> eval (Div (Const 4) (Const 0))                                                                      
-- Right' DivisionByZero
-- *Main> eval (Mul (Sub (Sub (Const 5) (Const 2)) (Const 2)) (Div (Sub (Const 8) (Const 4)) (Const 0)))      
-- Right' DivisionByZero
-- *Main> eval (Mul (Sub (Sub (Const 5) (Const 2)) (Const 2)) (Div (Sub (Const 8) (Const 4)) (Const 9)))
-- Left' 0
-- *Main> eval (Mul (Sub (Sub (Const 5) (Const 2)) (Const 2)) (Div (Sub (Const 8) (Const 4)) (Const 2)))
-- Left' 2
-- *Main> eval (Mul (Sub (Sub (Const 10) (Const 2)) (Const 2)) (Div (Sub (Const 8) (Const 4)) (Const 2)))
-- Left' 12


--tutto in inglese 
--tranne i commenti
-- *Main> semiPerfect 28 
-- [[1,2,4,7,14]]
-- *Main> eval (Mul (Sub (Sub (Const 10) (Const 2)) (Const 2)) (Div (Sub (Const 8) (Const 4)) (Const 2)))
-- Left' 12
-- *Main> eval (Div (Const 4) (Const 0))                                                                 
-- Right' DivisionByZero
-- *Main> eval (Mul (Sub (Sub (Const (-5)) (Const 2)) (Const 2)) (Div (Sub (Const 3) (Const 4)) (Const (-8))))
-- Right' NegativeNumber
-- *Main> eval (Mul (Sub (Sub (Const 5) (Const 2)) (Const 2)) (Div (Sub (Const 3) (Const 4)) (Const 0)))      
-- Right' (NegativeSubstraction "[Sub (Const (3) (Const (4)] il primo argomento e' minore del secondo ")

-- *Main> eval ( Sum (Const 4) (Sub (Const 6 ) (Const 2)))
-- Left' 8