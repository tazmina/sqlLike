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
  --test "simple.imp"
  --test "simple.imp"
  test "add.imp"
  --test "fact.imp"
  --test "times.imp"
  --test "test.imp"
  --test "error.imp"

