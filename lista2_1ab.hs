data Coisa a = UmaCoisa a | DuasCoisas a a | ZeroCoisa deriving Show

-- Letra a
instance Functor Coisa where
    fmap f (UmaCoisa val1) = UmaCoisa (f val1)
    fmap f (DuasCoisas val1 val2) = DuasCoisas (f val1) (f val2)
    fmap _ ZeroCoisa = ZeroCoisa

-- Letra b
instance Applicative Coisa where
    pure = UmaCoisa
    _ <*> ZeroCoisa = ZeroCoisa
    ZeroCoisa <*> _ = ZeroCoisa
    (UmaCoisa f)  <*> val = fmap f val
    (DuasCoisas f g) <*> (UmaCoisa a) = DuasCoisas (f a) (g a)
    (DuasCoisas f g) <*> (DuasCoisas a b) = DuasCoisas (f a) (g b)
    
    