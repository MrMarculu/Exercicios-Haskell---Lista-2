data Arvore a = Nulo | Folha a |Galho a (Arvore a) (Arvore a) deriving (Show)

instance Functor Arvore where
    fmap g (Arvore x y) = Arvore (g x) (g y)


instance Applicative Arvore	where
		pure (Galho x) (Galho y) = (Galho x) (Galho y) 
        pure (Folha a) = (Folha a)
        pure Nulo = Nulo 
		fs <*> xs =	[f x | (f <- fs), (x <- xs)]



