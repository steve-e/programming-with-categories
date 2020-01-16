-- example
tuple :: (t -> a, t -> b) -> t -> (a, b)
tuple (f,g) = \c -> (f c, g c)

toFst :: b -> (b, ())
toFst = tuple (id, const ())

toSnd :: b -> ((), b)
toSnd = tuple (const (), id)

