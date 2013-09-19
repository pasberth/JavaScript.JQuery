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
is : Content c => c -> JQuery -> JQueryIO Bool
is c q = do
  c <- getContentPtr c
  p <- getContentPtr q
  s <- liftIO $ mkForeign (FFun "%0.is(%1)" [FPtr, FPtr] FString) p c
  return $ s /= "false"

isWith : (Int -> Element -> JQueryIO Bool) -> JQuery -> JQueryIO Bool
isWith f q = do
  let f' = \p => \i => runJQueryIO $ f i $ MkElement p
  p <- getContentPtr q
  s <- liftIO $ mkForeign (FFun "%0.is(function () { return %1.apply(this, [this].concat([].slice.call(arguments, 0))) })" [FPtr, FFunction FPtr (FFunction FInt (FAny (IO Bool)))] FString) p f'
  return $ s /= "false"

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

public
getOffsetParent : JQuery -> JQueryIO JQuery
getOffsetParent q = do
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.offsetParent()" [FPtr] FPtr) p

public
getParent : JQuery -> JQueryIO JQuery
getParent q = do
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.parent()" [FPtr] FPtr) p

public
getParentSelector : Selector s => s -> JQuery -> JQueryIO JQuery
getParentSelector s q = do
  s <- getSelectorPtr s
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.parent(%1)" [FPtr, FPtr] FPtr) p s

public
getParents : JQuery -> JQueryIO JQuery
getParents q = do
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.parents()" [FPtr] FPtr) p

public
getParentsSelector : Selector s => s -> JQuery -> JQueryIO JQuery
getParentsSelector s q = do
  s <- getSelectorPtr s
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.parents(%1)" [FPtr, FPtr] FPtr) p s

public
getParentsUntil : JQuery -> JQueryIO JQuery
getParentsUntil q = do
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.parentsUntil()" [FPtr] FPtr) p

public
getParentsUntilSelector : Selector s => s -> JQuery -> JQueryIO JQuery
getParentsUntilSelector s q = do
  s <- getSelectorPtr s
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.parentsUntil(%1)" [FPtr, FPtr] FPtr) p s

public
getParentsUntilSelectorFilter : (Selector s, Selector t) => s -> t -> JQuery -> JQueryIO JQuery
getParentsUntilSelectorFilter s t q = do
  s <- getSelectorPtr s
  t <- getSelectorPtr t
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.parentsUntil(%1, %2)" [FPtr, FPtr, FPtr] FPtr) p s t

public
getParentsUntilElement : Element -> JQuery -> JQueryIO JQuery
getParentsUntilElement (MkElement s) q = do
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.parentsUntil(%1)" [FPtr, FPtr] FPtr) p s

public
getParentsUntilElementFilter : Selector t => Element -> t -> JQuery -> JQueryIO JQuery
getParentsUntilElementFilter (MkElement s) t q = do
  t <- getSelectorPtr t
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.parentsUntil(%1, %2)" [FPtr, FPtr, FPtr] FPtr) p s t

public
getPrev : JQuery -> JQueryIO JQuery
getPrev q = do
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.prev()" [FPtr] FPtr) p

public
getPrevSelector : Selector s => s -> JQuery -> JQueryIO JQuery
getPrevSelector s q = do
  s <- getSelectorPtr s
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.prev(%1)" [FPtr, FPtr] FPtr) p s

public
getPrevAll : JQuery -> JQueryIO JQuery
getPrevAll q = do
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.prevAll()" [FPtr] FPtr) p

public
getPrevAllSelector : Selector s => s -> JQuery -> JQueryIO JQuery
getPrevAllSelector s q = do
  s <- getSelectorPtr s
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.prevAll(%1)" [FPtr, FPtr] FPtr) p s

public
getPrevUntil : JQuery -> JQueryIO JQuery
getPrevUntil q = do
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.prevUntil()" [FPtr] FPtr) p

public
getPrevUntilSelector : Selector s => s -> JQuery -> JQueryIO JQuery
getPrevUntilSelector s q = do
  s <- getSelectorPtr s
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.prevUntil(%1)" [FPtr, FPtr] FPtr) p s

public
getPrevUntilSelectorFilter : (Selector s, Selector t) => s -> t -> JQuery -> JQueryIO JQuery
getPrevUntilSelectorFilter s t q = do
  s <- getSelectorPtr s
  t <- getSelectorPtr t
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.prevUntil(%1, %2)" [FPtr, FPtr, FPtr] FPtr) p s t

public
getPrevUntilElement : Element -> JQuery -> JQueryIO JQuery
getPrevUntilElement (MkElement s) q = do
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.prevUntil(%1)" [FPtr, FPtr] FPtr) p s

public
getPrevUntilElementFilter : Selector t => Element -> t -> JQuery -> JQueryIO JQuery
getPrevUntilElementFilter (MkElement s) t q = do
  t <- getSelectorPtr t
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.prevUntil(%1, %2)" [FPtr, FPtr, FPtr] FPtr) p s t

public
getSiblings : JQuery -> JQueryIO JQuery
getSiblings q = do
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.siblings()" [FPtr] FPtr) p

public
getSiblingsSelector : Selector s => s -> JQuery -> JQueryIO JQuery
getSiblingsSelector s q = do
  s <- getSelectorPtr s
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.siblings(%1)" [FPtr, FPtr] FPtr) p s

public
slice : Int -> Int -> JQuery -> JQueryIO JQuery
slice i j q = do
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.slice(%1, %2)" [FPtr, FInt, FInt] FPtr) p i j

public
sliceToEnd : Int -> JQuery -> JQueryIO JQuery
sliceToEnd i q = do
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.slice(%1, %2)" [FPtr, FInt] FPtr) p i
