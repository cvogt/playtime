import GHC.Err (error)
import My.IO
import My.Prelude
import SpaceMiner.Types

main :: IO ()
main = do
  let area = Area 1 12
  assert ("corners: " <> (show $ corners area)) $
    corners area == (1, Pos 1 12, 12, Pos 12 1)

  assert "1" $
    1 `isWithin` area
  assert "12" $
    12 `isWithin` area
  assert "1 12" $
    Pos 1 12 `isWithin` area
  assert "12 1" $
    Pos 12 1 `isWithin` area
  assert "13"
    $ not
    $ 13 `isWithin` area
  assert "0"
    $ not
    $ 0 `isWithin` area
  assert "1 13"
    $ not
    $ Pos 1 13 `isWithin` area
  assert "1 0"
    $ not
    $ Pos 1 0 `isWithin` area
  assert "0 12"
    $ not
    $ Pos 0 12 `isWithin` area
  assert "13 12"
    $ not
    $ Pos 13 12 `isWithin` area
  assert "0 6"
    $ not
    $ Pos 0 6 `isWithin` area
  assert "6 0"
    $ not
    $ Pos 6 0 `isWithin` area

  assert "144 96 within"
    $ not
    $ (Pos 156.0 107.01540000000021) `isWithin` (Area (Pos 144 96) 12)

  assert "144 96"
    $ not
    $ (Area (Pos 156.0 107.01540000000021) 12) `collidesWith` (Area (Pos 144 96) 12)

  assert "144 108"
    $ not
    $ (Area (Pos 156.0 107.01540000000021) 12) `collidesWith` (Area (Pos 144 108) 12)

assert :: [Char] -> Bool -> IO ()
assert msg predicate = unless predicate $ error msg
