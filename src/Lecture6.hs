-- example
tuple :: (t -> a, t -> b) -> t -> (a, b)
tuple (f,g) = \c -> (f c, g c)

toFst :: b -> (b, ())
toFst = tuple (id, const ())

toSnd :: b -> ((), b)
toSnd = tuple (const (), id)

commuteL :: (a1, (a2, a3)) -> ((a1, a2), a3)
commuteL = tuple (tuple (fst ,fst.snd ), snd.snd )

commuteR :: ((a1, a2), a3) -> (a1, (a2, a3))
commuteR = tuple (fst.fst, tuple (snd.fst, snd))

