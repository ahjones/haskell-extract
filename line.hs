extract :: [String] -> [[String]]
extract (_:_:_:[]) = []
extract (a:b:c:[]:rs) = extract (a:b:c:rs)
extract ([]:[]:c:r:rs) = [c, r] : extract([]:[]:c:rs)
extract ([]:b:_:r:rs) = [b, r] : extract([]:b:[]:rs)
extract (a:b:_:r:rs) = [b ++ a, r] : extract(a:b:[]:rs)
