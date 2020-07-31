import GHC.Err (error)
import My.IO
import My.Prelude
import Playtime.Types

main :: IO ()
main = do
  -- COLLISION DETECTION TESTS
  let area = (12, 1)
  assert ("corners: " <> (show $ corners area)) $
    corners area == Corners 1 (1, 13) 13 (13, 1)

  assert "1" $
    1 `isWithin` area
  assert "13" $
    13 `isWithin` area
  assert "1 13" $
    (1, 13) `isWithin` area
  assert "13 1" $
    (13, 1) `isWithin` area
  assert "14"
    $ not
    $ 14 `isWithin` area
  assert "0"
    $ not
    $ 0 `isWithin` area
  assert "1 14"
    $ not
    $ (1, 14) `isWithin` area
  assert "1 0"
    $ not
    $ (1, 0) `isWithin` area
  assert "0 13"
    $ not
    $ (0, 13) `isWithin` area
  assert "13 12"
    $ not
    $ (14, 13) `isWithin` area
  assert "0 6"
    $ not
    $ (0, 6) `isWithin` area
  assert "6 0"
    $ not
    $ (6, 0) `isWithin` area

  assert "144 96 within"
    $ not
    $ (156.1, 107.01540000000021) `isWithin` (12, (144, 96))

  assert "144 96"
    $ not
    $ (12, (156.1, 107.01540000000021)) `collidesWith` (12, (144, 96))

  assert "144 108"
    $ not
    $ (12, (156.1, 107.01540000000021)) `collidesWith` (12, (144, 108))

  -- VECTOR ARITHMETICS TESTS
  assert "Absolute |+ Relative" $ xAbsolute 1 |+ xRelative 1 == xAbsolute 2
  assert "Relative |+ Relative" $ xRelative 1 |+ xRelative 1 == xRelative 2
  assert "Pos |+ Relative X" $ (1 :: Pos) |+ xRelative 1 == ((2, 1) :: Pos)
  assert "Pos |+ Relative Y" $ (1 :: Pos) |+ yRelative 1 == ((1, 2) :: Pos)
  assert "Dim |+ Dim" $ ((1, 2) :: Dim) |+ ((1, 2) :: Dim) == ((2, 4) :: Dim)
  assert "Dim |+ Dim" $ ((1, 2) :: Pos) |+ ((1, 2) :: Dim) == ((2, 4) :: Pos)
  assert "Absolute X |+ Dim" $ xAbsolute 1 |+ ((1, 2) :: Dim) == xAbsolute 2
  assert "Absolute Y |+ Dim" $ yAbsolute 1 |+ ((1, 2) :: Dim) == yAbsolute 3
  assert "Relative X |+ Dim" $ xRelative 1 |+ ((1, 2) :: Dim) == xRelative 2
  assert "Relative Y |+ Dim" $ yRelative 1 |+ ((1, 2) :: Dim) == yRelative 3

  assert "Absolute |-| Absolute" $ xAbsolute 2 |-| xAbsolute 1 == xRelative 1
  assert "Pos |-| Pos" $ ((3, 5) :: Pos) |-| ((1, 2) :: Pos) == ((2, 3) :: Dim)

  assert "Absolute |- Relative" $ xAbsolute 2 |- xRelative 1 == xAbsolute 1
  assert "Pos |- Dim" $ ((3, 5) :: Pos) |- ((1, 2) :: Dim) == ((2, 3) :: Pos)
  -- instance SubtractionPairWiseLeft (Absolute a) (Relative a) where (Absolute l) |- (Relative r) = Absolute $ l - r
  -- instance SubtractionPairWiseLeft (Relative a) (Relative a) where (Relative l) |- (Relative r) = Relative $ l - r
  -- instance SubtractionPairWiseLeft (Absolute X) Dim where l |- dim = l |- fst dim
  -- instance SubtractionPairWiseLeft (Absolute Y) Dim where l |- dim = l |- snd dim
  -- instance SubtractionPairWiseLeft (Relative X) Dim where l |- dim = l |- fst dim
  -- instance SubtractionPairWiseLeft (Relative Y) Dim where l |- dim = l |- snd dim
  -- instance SubtractionPairWiseLeft Pos (Relative X) where pos |- r = (fst pos |- r, snd pos)
  -- instance SubtractionPairWiseLeft Pos (Relative Y) where pos |- r = (fst pos, snd pos |- r)
  -- instance SubtractionPairWiseLeft Pos Dim where (|-) = pairWise (|-) (|-)

  -- instance MultiplicationPairWiseRight (Factor a) (Relative a) where (Factor l) *| (Relative r) = Relative $ l * r
  -- instance MultiplicationPairWise (Relative a) (Factor a) where (Relative r) |*| (Factor l) = Relative $ l * r
  -- instance MultiplicationPairWise (Factor a) (Factor a) where (Factor l) |*| (Factor r) = Factor $ l * r
  -- instance MultiplicationPairWise (Factor X) Scale where l |*| scale = l |*| fst scale
  -- instance MultiplicationPairWise (Factor Y) Scale where l |*| scale = l |*| snd scale
  -- instance MultiplicationPairWise (Relative X) Scale where l |*| scale = l |*| fst scale
  -- instance MultiplicationPairWise (Relative Y) Scale where l |*| scale = l |*| snd scale
  -- instance MultiplicationPairWiseRight Scale (Relative X) where scale *| r = fst scale *| r
  -- instance MultiplicationPairWiseRight Scale (Relative Y) where scale *| r = snd scale *| r
  -- instance MultiplicationPairWiseRight (Factor X) Dim where scale *| r = (scale *| fst r, snd r)
  -- instance MultiplicationPairWiseRight (Factor Y) Dim where scale *| r = (fst r, scale *| snd r)

  -- instance MultiplicationPairWise Scale Scale where (|*|) = pairWise (|*|) (|*|)
  -- instance MultiplicationPairWise Dim Scale where (|*|) = pairWise (|*|) (|*|)
  -- instance MultiplicationPairWiseRight Scale Dim where (*|) = pairWise (*|) (*|)

  -- instance DivisionPairWise (Relative a) (Factor a) where (Relative l) |/| (Relative r) = Factor $ l / r
  -- instance DivisionPairWise Dim Scale where (|/|) = pairWise (|/|) (|/|)
  -- instance DivisionPairWiseLeft (Relative a) (Factor a) where (Relative l) |/ (Factor r) = Relative $ l / r
  -- instance DivisionPairWiseLeft Dim Scale where (|/) = pairWise (|/) (|/)

  assert "Relative X |% Dim" $ xRelative 8 |% ((5, 3) :: Dim) == xRelative 3
  assert "Relative Y |% Dim" $ yRelative 8 |% ((5, 3) :: Dim) == yRelative 2
  assert "Dim |% Relative X" $ ((8, 19) :: Dim) |% xRelative 5 == ((3, 19) :: Dim)
  assert "Dim |% Relative Y" $ ((8, 19) :: Dim) |% yRelative 5 == ((8, 4) :: Dim)
  assert "Pos |% Relative X" $ ((8, 19) :: Pos) |% xRelative 5 == ((3, 19) :: Pos)
  assert "Pos |% Relative Y" $ ((8, 19) :: Pos) |% yRelative 5 == ((8, 4) :: Pos)
  assert "Pos |% Dim" $ ((8, 19) :: Pos) |% ((5, 10) :: Dim) == ((3, 9) :: Pos)
  assert "Dim |% Dim" $ ((8, 19) :: Dim) |% ((5, 10) :: Dim) == ((3, 9) :: Dim)
  assert "Absolute |% Relative" $ xAbsolute 9 |% xRelative 5 == xAbsolute 4
  assert "Relative |% Relative" $ xRelative 9 |% xRelative 5 == xRelative 4

  assert "Pos |%%| Dim" $ ((8, 19) :: Pos) |%%| ((5, 10) :: Dim) == ((3, 9) :: Pos)
  assert "Dim |%%| Dim" $ ((8, 19) :: Dim) |%%| ((5, 10) :: Dim) == ((3, 9) :: Dim)
  assert "Absolute |%%| Relative" $ xAbsolute 9 |%%| xRelative 5 == xAbsolute 4
  assert "Relative |%%| Relative" $ xRelative 9 |%%| xRelative 5 == xRelative 4

assert :: [Char] -> Bool -> IO ()
assert msg predicate =
  if predicate
    then putStrLn ("SUCCESS: " <> msg)
    else error ("FAILED: " <> msg)
