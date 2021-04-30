
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
 1) pure (.) <*> Leaf u <*> Leaf v <*> Leaf w =  Leaf u <*> ( Leaf v <*> Leaf w) 
    pure (.) <*> Leaf u <*> Leaf v <*> Leaf w 
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




3) pure (.) <*> Leaf u <*> Leaf v <*> Branch(w1 w2) =  Leaf u <*> ( Leaf v <*> Branch(w1 w2)) 

    pure (.) <*> Leaf u <*> Leaf v <*> Branch(w1 w2)
    = Leaf (.) <*> Leaf u <*> Leaf v <*> Branch(w1 w2)
    = Leaf (u.) <*> Leaf v <*> Branch(w1 w2)
    = Leaf (u.v) <*> Branch(w1 w2)
    --(Leaf f) <*> (Branch x y) = Branch (f <$> x) (f <$> y)
    = Branch (u.v <$> w1) (u.v <$> w2)
    = Branch (u <$> (v <$> w1)) (u <$> (v <$> w2))

     Leaf u <*> ( Leaf v <*> Branch(w1 w2)) 
     --(Leaf f) <*> (Branch x y) = Branch (f <$> x) (f <$> y)
     = Leaf u <*> Branch(v <$> w1) (v <$> w2)
     = Branch (u <$> (v <$> w1)) (u <$> (v <$> w2))

    Branch (u <$> (v <$> w1)) (u <$> (v <$> w2))
    ||
    Branch (u <$> (v <$> w1)) (u <$> (v <$> w2))

4)  pure (.) <*> Leaf u <*>  Branch(v1 v2) <*> Branch(w1 w2) =  Leaf u <*> ( Branch(v1 v2) <*> Branch(w1 w2)) 

    pure (.) <*> Leaf u <*>  Branch(v1 v2) <*> Branch(w1 w2)
    = Leaf(u.) <*>  Branch(v1 v2) <*> Branch(w1 w2)
    -- (Leaf f) <*> (Branch x y) = Branch (f <$> x) (f <$> y)
    = Branch((u. <$> v1) (u. <$> v2)) Branch(w1 w2)
    -- (Branch f1 f2) <*> (Branch x y) = Branch (f1 <*> x) (f2 <*> y)
    = Branch((u. <$> v1) <*> w1) ((u. <$> v2) <*> w2)
    --f <$> x = pure f <*> x.
    = Branch(pure(u.)<*> v1 <*> w1) (pure(u.)<*> v2 <*> w2)

    Leaf u <*> ( Branch(v1 v2) <*> Branch(w1 w2)) 
    -- (Branch f1 f2) <*> (Branch x y) = Branch (f1 <*> x) (f2 <*> y)
    = Leaf u <*> Branch(v1 <*> w1) (v2 <*> w2)
    --(Leaf f) <*> (Branch x y) = Branch (f <$> x) (f <$> y)
    = Branch(u <$> (v1 <*> w1)) (u <$> (v2 <*> w2))
    --f <$> x = pure f <*> x.
    = Branch(pure(u) <*> (v1 <*> w1)) (pure(u) <*> (v1 <*> w1))
    = Branch(pure(u.)<*> v1 <*> w1) (pure(u.)<*> v2 <*> w2)
    
    Branch(pure(u.)<*> v1 <*> w1) (pure(u.)<*> v2 <*> w2)
    ||
    Branch(pure(u.)<*> v1 <*> w1) (pure(u.)<*> v2 <*> w2)

5) pure (.) <*> Branch (u1 u2) <*>  Branch(v1 v2) <*> Branch(w1 w2) =  Branch (u1 u2) <*> ( Branch(v1 v2) <*> Branch(w1 w2)) 

    pure (.) <*> Branch (u1 u2) <*>  Branch(v1 v2) <*> Branch(w1 w2) 
    = Leaf(.) <*> Branch (u1 u2) <*>  Branch(v1 v2) <*> Branch(w1 w2) 
    = Branch (((.) <$> u1) ((.) <$> u2)) <*>  Branch(v1 v2) <*> Branch(w1 w2) 
    = Branch(((.) <$> u1) <*> v1 ((.) <$> u2) <*> v2) <*> Branch(w1 w2) 
    = Branch((.)<$> u1 <*> v1 <*> w1)((.)<$> u2 <*> v2 <*> w2)
    = Branch(pure(.)<*>u1 <*> v1 <*> w1) (pure(.)<*>u2 <*> v2 <*> w2)
    = Branch (u1 <*> (v1 <*> w1)) (u2 <*> (v2 <*> w2)))

    Branch (u1 u2) <*> ( Branch(v1 v2) <*> Branch(w1 w2)) 
    = Branch (u1 u2) <*> Branch((v1<*>w1)(v2<*>w2))
    = Branch (u1 <*> (v1 <*> w1)) (u2 <*> (v2 <*> w2)))

    Branch (u1 <*> (v1 <*> w1)) (u2 <*> (v2 <*> w2)))
    ||
    Branch (u1 <*> (v1 <*> w1)) (u2 <*> (v2 <*> w2)))

6) pure (.) <*> Branch (u1 u2) <*>  Branch(v1 v2) <*> Leaf(w) =  Branch (u1 u2) <*> ( Branch(v1 v2) <*> Leaf(w)) 

    pure (.) <*> Branch (u1 u2) <*>  Branch(v1 v2) <*> Leaf(w) 
    = Leaf(.) <*> Branch (u1 u2) <*>  Branch(v1 v2) <*> Leaf(w) 
    = Branch (((.) <$> u1) ((.) <$> u2)) <*>  Branch(v1 v2) <*> Leaf(w) ) 
    = Branch(((.) <$> u1 <*> v1) ((.) <$> u2 <*> v2)) <*> Leaf(w) 
    = Branch((.) <$> u1 <*> v1 <*> Leaf(w)) ((.) <$> u2 <*> v2 <*> Leaf(w))
    = Branch (u1 <*> (v1 <*> Leaf(w))) (u2 <*> (v2 <*> Leaf(w)))

     Branch (u1 u2) <*> (Branch(v1 v2) <*> Leaf(w)) 
     = Branch (u1 u2) <*>  Branch ((v1 <*> Leaf w) (v2 <*> Leaf w))
     = Branch (u1 <*> (v1 <*> Leaf(w))) (u2 <*> (v2 <*> Leaf(w)))

    = Branch (u1 <*> (v1 <*> Leaf(w))) (u2 <*> (v2 <*> Leaf(w)))
    ||
    = Branch (u1 <*> (v1 <*> Leaf(w))) (u2 <*> (v2 <*> Leaf(w)))

7)  pure (.) <*> Branch (u1 u2) <*> Leaf(v) <*> Leaf(w) =  Branch (u1 u2) <*> (Leaf(v) <*> Leaf(w)) 

    pure (.) <*> Branch (u1 u2) <*> Leaf(v) <*> Leaf(w)
    = Branch (((.) <$> u1) ((.) <$> u2)) <*> Leaf(v) <*> Leaf(w)
    = Branch (((.) <$> u1 <*> Leaf(v)) ((.) <$> u2 <*> Leaf(v))) <*> Leaf(w)
    = Branch ((.) <$> u1 <*> Leaf(v) <*> Leaf(w)) ((.) <$> u2 <*> Leaf(v) <*> Leaf(w))
    = Branch (u1 <*> (Leaf(v) <*> Leaf(w))) (u2 <*> (Leaf(v) <*> Leaf(w)))
    = Branch (u1 <*> Leaf(v w)) (u2 <*> Leaf(v w))

    Branch (u1 u2) <*> (Leaf(v) <*> Leaf(w)) 
    = Branch (u1 u2) <*> Leaf(v w)
    = Branch (u1 <*> Leaf(v w)) (u2 <*> Leaf(v w))

    Branch (u1 <*> Leaf(v w)) (u2 <*> Leaf(v w))
    ||
    Branch (u1 <*> Leaf(v w)) (u2 <*> Leaf(v w))

8) pure (.) <*> Branch (u1 u2) <*> Leaf(v) <*> Branch (w1 w2) =  Branch (u1 u2) <*> (Leaf(v) <*> Branch (w1 w2)) 

    pure (.) <*> Branch (u1 u2) <*> Leaf(v) <*> Branch (w1 w2)
    = Branch (((.) <$> u1) ((.) <$> u2) <*> Leaf(v) <*> Branch (w1 w2)
    = Branch (((.) <$> u1 <*> Leaf(v)) ((.) <$> u2 <*> Leaf(v))) <*> Branch (w1 w2)
    = Branch ((.) <$> u1 <*> Leaf(v) <*> w1) ((.) <$> u2 <*> Leaf(v) <*> w2)
    = Branch (u1 <*> (Leaf(v) <*> w1)) (u2 <*> (Leaf(v) <*> w2))

    Branch (u1 u2) <*> (Leaf(v) <*> Branch (w1 w2)) 
    = Branch (u1 u2) <*> Branch ((v <$> w1) (v <$> w2))
    = Branch (u1<*> (v <$> w1)) (u2<*> (v <$> w2))
    --f <$> x = pure f <*> x.
    --pure x = Leaf x
    = Branch(u1 <*> (Leaf(v) <*> w1)) (u2 <*> (Leaf(v) <*> w2))
    
    Branch (u1 <*> (Leaf(v) <*> w1)) (u2 <*> (Leaf(v) <*> w2))
    ||
    Branch (u1 <*> (Leaf(v) <*> w1)) (u2 <*> (Leaf(v) <*> w2))

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
