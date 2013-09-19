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
getClosest : Content c => c -> JQuery -> JQueryIO JQuery
getClosest c q = do
  c <- getContentPtr c
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.closest(%1)" [FPtr, FPtr] FPtr) p c

public
getClosestContext : Content c => c -> Element -> JQuery -> JQueryIO JQuery
getClosestContext c e q = do
  c <- getContentPtr c
  e <- getContentPtr e
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.closest(%1, %2)" [FPtr, FPtr, FPtr] FPtr) p c e

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
filterContent : Content c => c -> JQuery -> JQueryIO JQuery
filterContent c q = do
  c <- getContentPtr c
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.filter(%1)" [FPtr, FPtr] FPtr) p c

filterWith : (Int -> Element -> JQueryIO Bool) -> JQuery -> JQueryIO JQuery
filterWith f q = do
  let f' = \p => \i => runJQueryIO $ f i $ MkElement p
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.filter(function () { return %1.apply(this, [this].concat([].slice.call(arguments, 0))) })" [FPtr, FFunction FPtr (FFunction FInt (FAny (IO Bool)))] FPtr) p f'

public
findContent : Content c => c -> JQuery -> JQueryIO JQuery
findContent c q = do
  c <- getContentPtr c
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.find(%1)" [FPtr, FPtr] FPtr) p c

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
is : Content c => c -> JQuery -> JQueryIO JQuery
is c q = do
  c <- getContentPtr c
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.is(%1)" [FPtr, FPtr] FPtr) p c

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

public
getNextAll : JQuery -> JQueryIO JQuery
getNextAll q = do
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.nextAll()" [FPtr] FPtr) p

public
getNextAllSelector : Selector s => s -> JQuery -> JQueryIO JQuery
getNextAllSelector s q = do
  s <- getSelectorPtr s
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.nextAll(%1)" [FPtr, FPtr] FPtr) p s

public
getNextUntil : JQuery -> JQueryIO JQuery
getNextUntil q = do
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.nextUntil()" [FPtr] FPtr) p

public
getNextUntilSelector : Selector s => s -> JQuery -> JQueryIO JQuery
getNextUntilSelector s q = do
  s <- getSelectorPtr s
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.nextUntil(%1)" [FPtr, FPtr] FPtr) p s

public
getNextUntilSelectorFilter : (Selector s, Selector t) => s -> t -> JQuery -> JQueryIO JQuery
getNextUntilSelectorFilter s t q = do
  s <- getSelectorPtr s
  t <- getSelectorPtr t
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.nextUntil(%1, %2)" [FPtr, FPtr, FPtr] FPtr) p s t

public
getNextUntilElement : Element -> JQuery -> JQueryIO JQuery
getNextUntilElement (MkElement s) q = do
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.nextUntil(%1)" [FPtr, FPtr] FPtr) p s

public
getNextUntilElementFilter : Selector t => Element -> t -> JQuery -> JQueryIO JQuery
getNextUntilElementFilter (MkElement s) t q = do
  t <- getSelectorPtr t
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.nextUntil(%1, %2)" [FPtr, FPtr, FPtr] FPtr) p s t

public
notContent : Content c => c -> JQuery -> JQueryIO JQuery
notContent c q = do
  c <- getContentPtr c
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.not(%1)" [FPtr, FPtr] FPtr) p c

notWith : (Int -> Element -> JQueryIO Bool) -> JQuery -> JQueryIO JQuery
notWith f q = do
  let f' = \p => \i => runJQueryIO $ f i $ MkElement p
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.not(function () { return %1.apply(this, [this].concat([].slice.call(arguments, 0))) })" [FPtr, FFunction FPtr (FFunction FInt (FAny (IO Bool)))] FPtr) p f'
