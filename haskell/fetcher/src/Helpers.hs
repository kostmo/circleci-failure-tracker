module Helpers where


transformLeft :: (a -> c) -> Either a b -> Either c b
transformLeft transformer original = case original of
  Left x -> Left $ transformer x
  Right x -> Right x


