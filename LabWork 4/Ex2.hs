
data Tree a = Leaf a | Branch (Tree a) (Tree a)

instance Applicative Tree where
  pure x = Leaf x
  (Branch f1 f2) <*> (Branch x y) = Branch (f1 <*> x) (f2 <*> y)
  -- остальное так же
  (Branch f1 f2) <*> (Leaf x) = Branch (f1 <*> Leaf x) (f2 <*> Leaf x)
  (Leaf f) <*> (Branch x y) = Branch (f <$> x) (f <$> y)
  (Leaf f) <*> (Leaf x) = Leaf (f x)


-- Идентичность

-- pure id <*> v = v
   pure id <*> v = v
   pure id <*> Leaf x = Leaf id <*> Leaf x = Leaf (id x) = Leaf x
   pure id <*> Branch x y = Leaf id <*> Branch x y 
                        = Branch (id <$> x) (id <$> y) = Branch x y
-- Композиция

-- pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
 1) pure (.) <*> Leaf u <*> Leaf v <*> Leaf w 
    = Leaf (.) <*> Leaf u <*> Leaf v <*> Leaf w
    = Leaf (u.) <*> Leaf v <*> Leaf w
    = Leaf (u.v) <*> Leaf w
    = Leaf (u.v w) 
    = Leaf(u (v w))
    
    Leaf u <*> ( Leaf v <*> Leaf w) 
    = Leaf u <*>  Leaf(v w)
    = Leaf(u (v w))

    Leaf(u (v w))
    ||
    Leaf(u (v w))


 2) pure (.) <*> Leaf u <*> Branch v1 v2 <*> Leaf w = Leaf u <*> ( Branch v1 v2 <*> Leaf w) 
    
    pure (.) <*> Leaf u <*> Branch v1 v2 <*> Leaf w 
    = Leaf (.) <*> Leaf u <*> Branch v1 v2 <*> Leaf w 
    --(Leaf f) <*> (Branch x y) = Branch (f <$> x) (f <$> y)
    = Leaf (u.) <*>  Branch v1 v2 <*> Leaf w 
    = Branch ((u.) <$> v1) ((u.) <$> v2) <*> Leaf w 
    --(Branch f1 f2) <*> (Leaf x) = Branch (f1 <*> Leaf x) (f2 <*> Leaf x)
    = Branch ((u.) <$> v1 <*> Leaf w) ((u.) <$> v2 <*> Leaf w)
    -- (f . g) <$> x = f <$> (g <$> x)
    -- f <$> x = pure f <*> x
    = Branch(pure (u.) <*> v1 <*> Leaf w) (pure(u.) <*> v2 <*> Leaf w)

    Leaf u <*> ( Branch v1 v2 <*> Leaf w) 
    --(Branch f1 f2) <*> (Leaf x) = Branch (f1 <*> Leaf x) (f2 <*> Leaf x)
    =  Leaf u <*> Branch (v1 <*> Leaf w) (v2 <*> Leaf w)
    -- (Leaf f) <*> (Branch x y) = Branch (f <$> x) (f <$> y)
    = Branch(u <$> (v1 <*> Leaf w) ) (u <$> (v2 <*> Leaf w))
    --f <$> x = pure f <*> x.
    = Branch(pure(u) <*> (v1 <*> Leaf w)) (pure(u) <*> (v2 <*> Leaf w))
    -- pure u <*> (x1<*>x) = pure(.) <*> pure u <*> x1 <*> x 
    = Branch(pure(u.) <*> v1 <*> Leaf w) (pure(u.) <*> v2 <*> Leaf w)

    Branch(pure (u.) <*> v1 <*> Leaf w) (pure(u.) <*> v2 <*> Leaf w)
    ||
    Branch(pure (u.) <*> v1 <*> Leaf w) (pure(u.) <*> v2 <*> Leaf w)

3) 
-- Гомоморфизм
    pure f <*> pure x = pure (f x)
    
    Leaf f <*> Leaf x = Leaf f x
    ||
    pure f x = Leaf f x
-- Обмен
    u <*> pure y = pure ($ y) <*> u
    1)Leaf (u) <*> pure y = pure ($ y) <*> Leaf (u)
        Leaf (u) <*> pure y
        = Leaf (u) <*> Leaf (y)
        = Leaf (u y)

        pure ($ y) <*> Leaf (u)
        = Leaf ($ y) <*> Leaf (u)
        = Leaf ($ y u) 
        = Leaf (u y)
        -- f $ a = f a = $a f   
        Leaf (u y)
        || 
        Leaf (u y)
    2)Branch(v1 v2) <*>  pure y = pure ($ y) <*> Branch(v1 v2)
    Branch(v1 v2) <*>  pure y
    = Branch(v1 v2) <*>  Leaf y
    --(Branch f1 f2) <*> (Leaf x) = Branch (f1 <*> Leaf x) (f2 <*> Leaf x)
    = Branch(v1 <*>  Leaf y)(v2 <*>  Leaf y)

    pure ($ y) <*> Branch(v1 v2)
    = Leaf ($ y) <*> Branch(v1 v2)
    --(Leaf f) <*> (Branch x y) = Branch (f <$> x) (f <$> y)
    = Branch ($ y <$> v1) ($ y <$> v2)
    = Branch (pure $y <*> v1) (pure $y <*> v2)
    = Branch(v1 <*>  Leaf y)(v2 <*>  Leaf y)

    = Branch(v1 <*>  Leaf y)(v2 <*>  Leaf y)
    ||
    = Branch(v1 <*>  Leaf y)(v2 <*>  Leaf y)
