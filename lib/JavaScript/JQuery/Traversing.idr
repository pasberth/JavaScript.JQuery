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
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.add(%1, %2)" [FPtr, FPtr, FPtr] FPtr) p s e

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
getClosestContext : Selector s => s -> Element -> JQuery -> JQueryIO JQuery
getClosestContext s e q = do
  s <- getSelectorPtr s
  e <- getContentPtr e
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.closest(%1, %2)" [FPtr, FPtr, FPtr] FPtr) p s e

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

public
getContents : JQuery -> JQueryIO JQuery
getContents q = do
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.contents()" [FPtr] FPtr) p

each : (Int -> Element -> JQueryIO $ the Type ()) -> JQuery -> JQueryIO JQuery
each f q = do
  let f' = \p => \i => runJQueryIO $ f i $ MkElement p
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.each(function () { return %1.apply(this, [this].concat([].slice.call(arguments, 0))) })" [FPtr, FFunction FPtr (FFunction FInt (FAny (IO ())))] FPtr) p f'

public
end : JQuery -> JQueryIO JQuery
end q = do
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.end()" [FPtr] FPtr) p

public
eq : Int -> JQuery -> JQueryIO JQuery
eq i q = do
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.eq(%1)" [FPtr, FInt] FPtr) p i
