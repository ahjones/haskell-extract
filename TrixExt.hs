module TrixExt (extract) where

extract :: [String] -> [[String]]
extract ([]:[]:c:rs) = trix c rs
extract ([]:b:_:rs) = trix b rs
extract (a:b:_:rs) = trix (b ++ a) rs

trix :: String -> [String] -> [[String]]
trix names [] = []
trix names ([]:rs) = trix names rs
trix names (r:rs) = [names, r] : trix names rs