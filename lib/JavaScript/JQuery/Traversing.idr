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

eachWith : (Int -> Element -> JQueryIO $ the Type ()) -> JQuery -> JQueryIO JQuery
eachWith f q = do
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

public
filterSelector : Selector s => s -> JQuery -> JQueryIO JQuery
filterSelector s q = do
  s <- getSelectorPtr s
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.filter(%1)" [FPtr, FPtr] FPtr) p s

public
filterElement : Element -> JQuery -> JQueryIO JQuery
filterElement (MkElement s) q = do
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.filter(%1)" [FPtr, FPtr] FPtr) p s

public
filterJQuery : JQuery -> JQuery -> JQueryIO JQuery
filterJQuery s q = do
  s <- getContentPtr s
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.filter(%1)" [FPtr, FPtr] FPtr) p s

filterWith : (Int -> Element -> JQueryIO Bool) -> JQuery -> JQueryIO JQuery
filterWith f q = do
  let f' = \p => \i => runJQueryIO $ f i $ MkElement p
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.filter(function () { return %1.apply(this, [this].concat([].slice.call(arguments, 0))) })" [FPtr, FFunction FPtr (FFunction FInt (FAny (IO Bool)))] FPtr) p f'

public
findSelector : Selector s => s -> JQuery -> JQueryIO JQuery
findSelector s q = do
  s <- getSelectorPtr s
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.find(%1)" [FPtr, FPtr] FPtr) p s

public
findElement : Element -> JQuery -> JQueryIO JQuery
findElement (MkElement s) q = do
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.find(%1)" [FPtr, FPtr] FPtr) p s

public
findJQuery : JQuery -> JQuery -> JQueryIO JQuery
findJQuery s q = do
  s <- getContentPtr s
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.find(%1)" [FPtr, FPtr] FPtr) p s

public
getFirst : JQuery -> JQueryIO JQuery
getFirst q = do
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.first()" [FPtr] FPtr) p

public
hasSelector : Selector s => s -> JQuery -> JQueryIO JQuery
hasSelector s q = do
  s <- getSelectorPtr s
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.has(%1)" [FPtr, FPtr] FPtr) p s

public
hasElement : Element -> JQuery -> JQueryIO JQuery
hasElement (MkElement s) q = do
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.has(%1)" [FPtr, FPtr] FPtr) p s

public
isSelector : Selector s => s -> JQuery -> JQueryIO JQuery
isSelector s q = do
  s <- getSelectorPtr s
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.is(%1)" [FPtr, FPtr] FPtr) p s

public
isElement : Element -> JQuery -> JQueryIO JQuery
isElement (MkElement s) q = do
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.is(%1)" [FPtr, FPtr] FPtr) p s

public
isJQuery : JQuery -> JQuery -> JQueryIO JQuery
isJQuery s q = do
  s <- getContentPtr s
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.is(%1)" [FPtr, FPtr] FPtr) p s

isWith : (Int -> Element -> JQueryIO Bool) -> JQuery -> JQueryIO JQuery
isWith f q = do
  let f' = \p => \i => runJQueryIO $ f i $ MkElement p
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.is(function () { return %1.apply(this, [this].concat([].slice.call(arguments, 0))) })" [FPtr, FFunction FPtr (FFunction FInt (FAny (IO Bool)))] FPtr) p f'

public
getLast : JQuery -> JQueryIO JQuery
getLast q = do
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.last()" [FPtr] FPtr) p

-- TODO:
-- .map()

public
getNext : JQuery -> JQueryIO JQuery
getNext q = do
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.next()" [FPtr] FPtr) p

public
getNextSelector : Selector s => s -> JQuery -> JQueryIO JQuery
getNextSelector s q = do
  s <- getSelectorPtr s
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.next(%1)" [FPtr, FPtr] FPtr) p s
