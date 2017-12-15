import ClassyPrelude

doCaptcha :: String -> IO Int
doCaptcha input = do
  is <- traverse (\ x -> maybe (fail $ "not a digit: " <> [x]) pure $ readMay [x]) input
  maybe (fail "not enough input") (pure . flip captcha is) $ headMay is

captcha :: Int -> [Int] -> Int
captcha i = \ case
  [] -> 0
  x:[] -> if x == i then x else 0
  x:y:xs -> (if x == y then x else 0) + captcha i (y:xs)
