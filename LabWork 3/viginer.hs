vigenere :: [Char] -> [Int] -> String -> [Char]
vigenere x k ys = [func2 x l |l<- (zipWith func (take (length ys) (cycle (chk x k))) (vig1 x ys))]
    where
    vig1 x [] = [] 
    --vig1 для извлечения индексов символов строки из алфавита построения пары: (индекс символа в алфавите, символ)
    vig1 x (y:ys) =((elemIndex y x),y) : (vig1 x ys)
    func x (mx,a) =case mx of 
    -- func для сложения индексов символов сообщения из алфавита с ключом, 
    --(-1) если символа нет в алфавите
        Just b -> (Just(b + x),a)
        Nothing -> (Nothing,a)
    func2 x (y,z)= case y of
        Nothing -> z
        Just y -> (!!) x (mod (y+(length x)) (length x))
    chk x [] = [] -- функция для преобразования отрицательных элементов ключа в положительные эквивалентные значения
    chk x (y:ys) | y<0 = (mod (y+(length x)) (length x)) : (chk x ys)
    chk x (y:ys) = y: (chk x ys)

vigenereDec :: [Char] -> [ Int] -> String -> [Char]
vigenered x k ys = [func2 x l |l<- (zipWith func (take (length ys) (cycle (chk x k))) (vig1 x ys))]
    where
        vig1 x [] = [] --vig1 для извлечения индексов символов строки из алфавита построения пары: (индекс символа в алфавите, символ)
        vig1 x (y:ys) =((elemIndex y x),y) : (vig1 x ys)
        func x (mx,a) =case mx of -- func для вычитания ключа из индексов символов сообщения из алфавита, (-1) если символа нет в алфавите
            Just b -> (Just(b - x),a)
            Nothing -> (Nothing,a)
        --func2 x (y,z) |y==(-1) = z -- строим результирующую строку, извлекая из алфавита символы по индексу
        --func2 x (y,z)=(!!) x (mod (y+(length x)) (length x))
        func2 x (y,z)= case y of
            Nothing -> z
            Just y -> (!!) x (mod (y+(length x)) (length x))
        --func2 x (y,z)=(!!) x (mod (y+(length x)) (length x))
        chk x [] = [] -- функция для преобразования отрицательных элементов ключа в положительные эквивалентные значения
        chk x (y:ys) | y<0 = (mod (y+(length x)) (length x)) : (chk x ys)
        chk x (y:ys) = y: (chk x ys)