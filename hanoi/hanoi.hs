type Rod = [Int]

push :: Rod -> Int -> Rod
push [] d = [d]
push (x:xs) d
    | d < x = d:x:xs
    | otherwise = error "invalid push"

data RodId = A | B | C deriving (Eq, Ord, Show)

type Tower = (Rod, Rod, Rod)

a (f,_,_) = f
b (_,s,_) = s
c (_,_,t) = t

move :: Rod -> Rod -> (Rod, Rod)
move a b = (tail a, push b (head a))

move' :: Tower -> RodId -> RodId -> Tower
move' t A B = (fst res, snd res, c t) where res = move (a t) (b t)
move' t A C = (fst res, b t, snd res) where res = move (a t) (c t)
move' t B A = (snd res, fst res, c t) where res = move (b t) (a t)
move' t B C = (a t, fst res, snd res) where res = move (b t) (c t)
move' t C A = (snd res, b t, fst res) where res = move (c t) (a t)
move' t C B = (a t, snd res, fst res) where res = move (c t) (b t)


solve :: Tower -> Tower
solve t =
