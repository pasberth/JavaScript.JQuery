module Main

import JavaScript.JQuery

main : IO ()
main = runJQueryIO $ ready $ do
  jQueryContent "body" >>= setText "hello world"
  return ()