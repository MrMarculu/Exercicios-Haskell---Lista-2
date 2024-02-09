-- Letra a
data Caixa a = Um a | Dois a a | Tres a a a 
    deriving Show
instance Functor Caixa where
    fmap f (Um val1) = Um (f val1)
    fmap f (Dois val1 val2) = Dois (f val1) (f val2)
    fmap f (Tres val1 val2 val3) = Tres (f val1) (f val2) (f val3)

instance Applicative Caixa where
    pure = Um
    (Um f) <*> algo = fmap f algo
    
    (Dois f g) <*> (Um val1) = Dois (f val1) (g val1)
    (Dois f g) <*> (Dois val1 val2) = Dois (f val1) (g val2)
    (Dois f g) <*> (Tres val1 val2 val3) = Tres (f val1) (g val2) (f val3)

    (Tres f g w) <*> (Um val1) = Tres (f val1) (g val1) (w val1)
    (Tres f g w) <*> (Dois val1 val2)  = Tres (f val1) (g val2) (g val1)
    (Tres f g w) <*> (Tres val1 val2 val3) = Tres (f val1) (g val2) (w val3)

instance Monad Caixa where
    return = pure
    (Um val) >>= f = f val
    (Dois _ val) >>= f = f val
    (Tres _ _ val) >>= f = f val

-- letra b
mult234 :: Double -> Caixa Double
mult234 x = Um x >>= 
            \a -> Tres (2*a) (3 * a) (4 * a)-- >>
            --Tres (2*a) (3*a) (4*a)