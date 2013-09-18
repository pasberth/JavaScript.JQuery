module JavaScript.JQuery.Manipulation

import JavaScript.JQuery.Types

public
addClassAll : Foldable t => t String -> JQuery -> JQueryIO JQuery
addClassAll ss q = do
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.addClass(%1)" [FPtr, FString] FPtr) p (unwords $ toList ss)

public
addClass : String -> JQuery -> JQueryIO JQuery
addClass s = addClassAll $ the (List String) [s]

public
addClassAllWith : Foldable t => (Int -> String -> JQueryIO $ t String) -> JQuery -> JQueryIO JQuery
addClassAllWith f q = do
  let f' = \a => \b => runJQueryIO $ f a b >>= return . unwords . toList
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.addClass(%1)" [FPtr, FFunction FInt (FFunction FString (FAny (IO String)))] FPtr) p f'

public
addClassWith : (Int -> String -> JQueryIO String) -> JQuery -> JQueryIO JQuery
addClassWith f = addClassAllWith (the (Int -> String -> JQueryIO $ List String) (\a => \b => (f a b) >>= (\s => return [s])))

public
after : Content c => c -> JQuery -> JQueryIO JQuery
after c q = do
  c <- getContentPtr c
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.after(%1)" [FPtr, FPtr] FPtr) p c

public
afterWith : Content c => (Int -> JQueryIO c) -> JQuery -> JQueryIO JQuery
afterWith f q = do
  let f' = \x => runJQueryIO $ (f x) >>= getContentPtr
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.after(%1)" [FPtr, FFunction FInt (FAny (IO Ptr))] FPtr) p f'

public
append : Content c => c -> JQuery -> JQueryIO JQuery
append c q = do
  c <- getContentPtr c
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.append(%1)" [FPtr, FPtr] FPtr) p c

public
appendWith : Content c => (Int -> String -> JQueryIO c) -> JQuery -> JQueryIO JQuery
appendWith f q = do
  let f' = \a => \b => runJQueryIO $ f a b >>= getContentPtr
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.append(%1)" [FPtr, FFunction FInt (FFunction FString (FAny (IO Ptr)))] FPtr) p f'

public
appendTo : Content c => c -> JQuery -> JQueryIO JQuery
appendTo c q = do
  c <- getContentPtr c
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.appendTo(%1)" [FPtr, FPtr] FPtr) p c

public
getAttr : String -> JQuery -> JQueryIO String
getAttr k q = do
  p <- getContentPtr q
  liftIO $ mkForeign (FFun "%0.attr(%1)" [FPtr, FString] FString) p k

public
setAttr : String -> String -> JQuery -> JQueryIO JQuery
setAttr k v q = do
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.attr(%1, %2)" [FPtr, FString, FString] FPtr) p k v

public
setAttrWith : String -> (Int -> String -> JQueryIO String) -> JQuery -> JQueryIO JQuery
setAttrWith k f q = do
  let f' = \a => \b => runJQueryIO $ f a b
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.attr(%1, %2)" [FPtr, FString, FFunction FInt (FFunction FString (FAny (IO String)))] FPtr) p k f'

public
before : Content c => c -> JQuery -> JQueryIO JQuery
before c q = do
  c <- getContentPtr c
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.before(%1)" [FPtr, FPtr] FPtr) p c

public
beforeWith : Content c => (Int -> JQueryIO c) -> JQuery -> JQueryIO JQuery
beforeWith f q = do
  let f' = \x => runJQueryIO $ (f x) >>= getContentPtr
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.before(%1)" [FPtr, FFunction FInt (FAny (IO Ptr))] FPtr) p f'

public
clone : JQuery -> JQueryIO JQuery
clone q = do
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.clone()" [FPtr] FPtr) p

public
cloneWithDataAndEvents  : JQuery -> JQueryIO JQuery
cloneWithDataAndEvents q = do
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.clone(true)" [FPtr] FPtr) p

public
cloneDeepWithDataAndEvents  : JQuery -> JQueryIO JQuery
cloneDeepWithDataAndEvents q = do
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.clone(true, true)" [FPtr] FPtr) p

public
getCSS : String -> JQuery -> JQueryIO String
getCSS k q = do
  p <- getContentPtr q
  liftIO $ mkForeign (FFun "%0.css(%1)" [FPtr, FString] FString) p k

public
setCSS : String -> String -> JQuery -> JQueryIO JQuery
setCSS k v q = do
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.css(%1, %2)" [FPtr, FString, FString] FPtr) p k v

public
setCSSWith : String -> (Int -> String -> JQueryIO String) -> JQuery -> JQueryIO JQuery
setCSSWith k f q = do
  let f' = \x => runJQueryIO . f x
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.css(%1, %2)" [FPtr, FString, FFunction FInt (FFunction FString (FAny (IO String)))] FPtr) p k f'

public
detach : JQuery -> JQueryIO JQuery
detach q = do
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.detach()" [FPtr] FPtr) p

public
detachSelector : Selector s => s -> JQuery -> JQueryIO JQuery
detachSelector s q = do
  s <- getSelectorPtr s
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.detach(%1)" [FPtr, FPtr] FPtr) p s

public
empty : JQuery -> JQueryIO JQuery
empty q = do
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.empty()" [FPtr] FPtr) p

public
hasClass : String -> JQuery -> JQueryIO Bool
hasClass k q = do
  p <- getContentPtr q
  s <- liftIO $ mkForeign (FFun "%0.hasClass(%1)" [FPtr, FString] FString) p k
  if s == "false" then return True else return False

public
getHTML : JQuery -> JQueryIO String
getHTML q = do
  p <- getContentPtr q
  liftIO $ mkForeign (FFun "%0.html()" [FPtr] FString) p

public
setHTML : String -> JQuery -> JQueryIO JQuery
setHTML s q = do
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.html(%1)" [FPtr, FString] FPtr) p s

public
setHTMLWith : (Int -> String -> JQueryIO String) -> JQuery -> JQueryIO JQuery
setHTMLWith f q = do
  let f' = \x => runJQueryIO . f x
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.html(%1, %2)" [FPtr, FFunction FInt (FFunction FString (FAny (IO String)))] FPtr) p f'

public
getHeight : JQuery -> JQueryIO Int
getHeight q = do
  p <- getContentPtr q
  liftIO $ mkForeign (FFun "%0.height()" [FPtr] FInt) p

public
setHeight : Int -> JQuery -> JQueryIO JQuery
setHeight i q = do
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.height(%1)" [FPtr, FInt] FPtr) p i

public
setHeightWith : (Int -> Int -> JQueryIO Int) -> JQuery -> JQueryIO JQuery
setHeightWith f q = do
  let f' = \x => runJQueryIO . f x
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.height(%1, %2)" [FPtr, FFunction FInt (FFunction FInt (FAny (IO Int)))] FPtr) p f'

public
getWidth : JQuery -> JQueryIO Int
getWidth q = do
  p <- getContentPtr q
  liftIO $ mkForeign (FFun "%0.width()" [FPtr] FInt) p

public
setWidth : Int -> JQuery -> JQueryIO JQuery
setWidth i q = do
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.width(%1)" [FPtr, FInt] FPtr) p i

public
setWidthWith : (Int -> Int -> JQueryIO Int) -> JQuery -> JQueryIO JQuery
setWidthWith f q = do
  let f' = \x => runJQueryIO . f x
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.width(%1, %2)" [FPtr, FFunction FInt (FFunction FInt (FAny (IO Int)))] FPtr) p f'

public
innerHeight : JQuery -> JQueryIO Int
innerHeight q = do
  p <- getContentPtr q
  liftIO $ mkForeign (FFun "%0.innerHeight()" [FPtr] FInt) p

public
innerWidth : JQuery -> JQueryIO Int
innerWidth q = do
  p <- getContentPtr q
  liftIO $ mkForeign (FFun "%0.innerWidth()" [FPtr] FInt) p

public
outerHeight : JQuery -> JQueryIO Int
outerHeight q = do
  p <- getContentPtr q
  liftIO $ mkForeign (FFun "%0.outerHeight()" [FPtr] FInt) p

public
outerWidth : JQuery -> JQueryIO Int
outerWidth q = do
  p <- getContentPtr q
  liftIO $ mkForeign (FFun "%0.outerWidth()" [FPtr] FInt) p

public
insertAfter : Content c => c -> JQuery -> JQueryIO JQuery
insertAfter c q = do
  c <- getContentPtr c
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.insertAfter(%1)" [FPtr, FPtr] FPtr) p c

public
insertBefore : Content c => c -> JQuery -> JQueryIO JQuery
insertBefore c q = do
  c <- getContentPtr c
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.insertBefore(%1)" [FPtr, FPtr] FPtr) p c

public
getOffsetLeft : JQuery -> JQueryIO Int
getOffsetLeft q = do
  p <- getContentPtr q
  liftIO $ mkForeign (FFun "%0.offset().left" [FPtr] FInt) p

public
getOffsetTop : JQuery -> JQueryIO Int
getOffsetTop q = do
  p <- getContentPtr q
  liftIO $ mkForeign (FFun "%0.offset().top" [FPtr] FInt) p

public
getOffset : JQuery -> JQueryIO $ the Type (Int, Int)
getOffset q = do
  x <- getOffsetLeft q
  y <- getOffsetTop q
  return (x, y)

public
setOffsetLeft : Int -> JQuery -> JQueryIO JQuery
setOffsetLeft x q = do
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.offset({ left: %1 })" [FPtr, FInt] FPtr) p x

public
setOffsetTop : Int -> JQuery -> JQueryIO JQuery
setOffsetTop x q = do
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.offset({ top: %1 })" [FPtr, FInt] FPtr) p x

public
setOffset : Int -> Int -> JQuery -> JQueryIO JQuery
setOffset x y q = do
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.offset({ left: %1, top: %2 })" [FPtr, FInt, FInt] FPtr) p x y

-- TODO:
-- setOffsetWith

public
getPositionLeft : JQuery -> JQueryIO Int
getPositionLeft q = do
  p <- getContentPtr q
  liftIO $ mkForeign (FFun "%0.position().left" [FPtr] FInt) p

public
getPositionTop : JQuery -> JQueryIO Int
getPositionTop q = do
  p <- getContentPtr q
  liftIO $ mkForeign (FFun "%0.position().top" [FPtr] FInt) p

public
getPosition : JQuery -> JQueryIO $ the Type (Int, Int)
getPosition q = do
  x <- getPositionLeft q
  y <- getPositionTop q
  return (x, y)

public
prepend : Content c => c -> JQuery -> JQueryIO JQuery
prepend c q = do
  c <- getContentPtr c
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.prepend(%1)" [FPtr, FPtr] FPtr) p c

public
prependWith : Content c => (Int -> String -> JQueryIO c) -> JQuery -> JQueryIO JQuery
prependWith f q = do
  let f' = \a => \b => runJQueryIO $ f a b >>= getContentPtr
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.prepend(%1)" [FPtr, FFunction FInt (FFunction FString (FAny (IO Ptr)))] FPtr) p f'

public
prependTo : Content c => c -> JQuery -> JQueryIO JQuery
prependTo c q = do
  c <- getContentPtr c
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.prependTo(%1)" [FPtr, FPtr] FPtr) p c

-- TODO:
-- .prop()

public
remove : JQuery -> JQueryIO JQuery
remove q = do
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.remove()" [FPtr] FPtr) p

public
removeSelector : Selector s => s -> JQuery -> JQueryIO JQuery
removeSelector s q = do
  s <- getSelectorPtr s
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.remove(%1)" [FPtr, FPtr] FPtr) p s
