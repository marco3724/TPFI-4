--Esercizio 1
--prendo un input (filtrando tutti i caratteri che non sono numeri)
input = do {
    xs <- getLine;
    ns <- pure $ (filter (\x -> (x>='0') && (x<='9')) xs);
    return (read ns :: Int);
}

--prendo n input
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

--Esercizio 2

subsets []  = [[]]
subsets (x:xs) =  subsets xs ++ (  (++) <$>  [[x]] <*>  subsets xs)

semiPerfect x = filter (\s->sum s == x) $ subsets $ dividers
    where dividers = (filter (\n->mod x n ==0) [1..x-1])

--Esercizio 3.1
-- left: risultato corretto
-- rigth: errore

data Either' a b = Left' b | Right' a
    deriving Show

instance Functor (Either' a) where
    fmap _ (Right' e)  = Right' e
    fmap f ( Left' v) =  Left' (f v)

instance Applicative (Either' a) where
    pure x =  Left' x
    Right'  e  <*>  _  = Right' e
    Left' f  <*>  r  =  fmap f r
    
instance Monad (Either' a) where
    Right' e >>= _ = Right' e
    Left' v >>= f = f v

--Esercizio 3.2
--strutture dati
data Error m = DivisionByZero | NegativeSubstraction m | NegativeNumber
    deriving Show

data Term = Const Int | Div Term Term | Sub Term Term |Sum Term Term | Mul Term Term


-- funzioni
safeDiv _ 0 = Right' DivisionByZero 
safeDiv m n = Left' (m `div` n) 

safeSub m n 
    | n>m = Right' $  NegativeSubstraction $ "[Sub (Const "++ show m ++ ") (Const "++ show n++")] il primo argomento e' minore del secondo "
    |otherwise = Left' (m-n)



--valutatore

-- se ammetto solo numeri maggiori (o uguali ) di 0, e la sottrazione ammette solo risultati maggiori  (o uguali ) di 0, 
-- allora questo valutatore lavorera' solo su espressioni di tipo Nat [0..]
eval (Const a)
    |a >=0 = Left' a
    |otherwise = Right' NegativeNumber

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