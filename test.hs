import SqlLikeInterp

test f = do
  putStrLn $ "*** Testing " ++ f
  putStrLn "=== Parsed String"
  showParsedExp f
  putStrLn ""
  runFile f
  putStrLn ""

main :: IO ()
main = do
  test "simple.imp"
  test "complex.imp"

