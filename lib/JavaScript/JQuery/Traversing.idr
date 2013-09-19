module JavaScript.JQuery.Traversing

import JavaScript.JQuery.Types

public
addContent : Content c => c -> JQuery -> JQueryIO JQuery
addContent c q = do
  c <- getContentPtr c
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.add(%1)" [FPtr, FPtr] FPtr) p c

public
addSelectorContext : Selector s => s -> Element -> JQuery -> JQueryIO JQuery
addSelectorContext s e q = do
  s <- getSelectorPtr s
  e <- getContentPtr e
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.add(%1, %1)" [FPtr, FPtr, FPtr] FPtr) p s e

public
addBack : JQuery -> JQueryIO JQuery
addBack q = do
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.addBack()" [FPtr] FPtr) p

public
addBackSelector : Selector s => s -> JQuery -> JQueryIO JQuery
addBackSelector s q = do
  s <- getSelectorPtr s
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.addBack(%1)" [FPtr, FPtr] FPtr) p s

public
getChildren : JQuery -> JQueryIO JQuery
getChildren q = do
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.children()" [FPtr] FPtr) p

public
getChildrenSelector : Selector s => s -> JQuery -> JQueryIO JQuery
getChildrenSelector s q = do
  s <- getSelectorPtr s
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.children(%1)" [FPtr, FPtr] FPtr) p s

public
getClosest : Selector s => s -> JQuery -> JQueryIO JQuery
getClosest s q = do
  s <- getSelectorPtr s
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.closest(%1)" [FPtr, FPtr] FPtr) p s

public
getClosestJQuery : JQuery -> JQuery -> JQueryIO JQuery
getClosestJQuery s q = do
  s <- getContentPtr s
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.closest(%1)" [FPtr, FPtr] FPtr) p s

public
getClosestElement : Element -> JQuery -> JQueryIO JQuery
getClosestElement (MkElement s) q = do
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.closest(%1)" [FPtr, FPtr] FPtr) p s
