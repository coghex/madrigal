module Win.Data where
import Prelude()
import UPrelude

data BitmapWindow = BitmapWindow { bmScreenWidth  ∷ Int
                                 , bmScreenHeight ∷ Int
                                 , bmBitmapWidth  ∷ Int
                                 , bmBitmapHeight ∷ Int
                                 } deriving (Show, Eq)
