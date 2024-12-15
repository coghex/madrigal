module Win where
import Prelude()
import UPrelude
import Prog ( Prog(..) )
import Win.Data ( BitmapWindow(..) )

generateBitmapWindow ∷ Int → Int → Prog ε σ BitmapWindow
generateBitmapWindow sw sh = do
  -- keep 160 pixels on the screen width
  let pixWidth     = sw `div` 160
      bitmapHeight = sh `div` pixWidth

  let bmw = BitmapWindow
              { bmScreenWidth  = sw
              , bmScreenHeight = sh
              , bmBitmapWidth  = 160
              , bmBitmapHeight = bitmapHeight
              }
  return bmw
