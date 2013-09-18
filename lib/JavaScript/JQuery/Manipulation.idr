module JavaScript.JQuery.Manipulation

import JavaScript.JQuery.Types

public
addClassList : Foldable t => t String -> JQuery -> JQueryIO JQuery
addClassList ss q = do
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.addClass(%1)" [FPtr, FString] FPtr) p (unwords $ toList ss)

public
addClass : String -> JQuery -> JQueryIO JQuery
addClass s = addClassList $ the (List String) [s]

public
addClassListWith : Foldable t => (Int -> String -> Element -> JQueryIO (t String)) -> JQuery -> JQueryIO JQuery
addClassListWith f q = do
    p <- getContentPtr q
    liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.addClass(function () { return %1.apply(this, [this].concat([].slice.call(arguments, 0))) })"
                                                [FPtr, FFunction FPtr (FFunction FInt (FFunction FString (FAny (IO String))))] FPtr)
                                          p f'
  where f' : Ptr -> Int -> String -> IO String
        f' p i s = runJQueryIO $ f i s (MkElement p) >>= return . unwords . toList

public
addClassWith : (Int -> String -> Element -> JQueryIO String) -> JQuery -> JQueryIO JQuery
addClassWith f = addClassListWith f' where
  f' : Int -> String -> Element -> JQueryIO (List String)
  f' a b e = (f a b e) >>= (\s => return [s])

public
after : Content c => c -> JQuery -> JQueryIO JQuery
after c q = do
  c <- getContentPtr c
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.after(%1)" [FPtr, FPtr] FPtr) p c

public
afterWith : Content c => (Int -> Element -> JQueryIO c) -> JQuery -> JQueryIO JQuery
afterWith f q = do
  let f' = \p => \i => runJQueryIO $ (f i (MkElement p)) >>= getContentPtr
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.after(function () { return %1.apply(this, [this].concat([].slice.call(arguments, 0))) })" [FPtr, FFunction FPtr (FFunction FInt (FAny (IO Ptr)))] FPtr) p f'

public
append : Content c => c -> JQuery -> JQueryIO JQuery
append c q = do
  c <- getContentPtr c
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.append(%1)" [FPtr, FPtr] FPtr) p c

public
appendWith : Content c => (Int -> String -> Element -> JQueryIO c) -> JQuery -> JQueryIO JQuery
appendWith f q = do
    p <- getContentPtr q
    liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.append(function () { return %1.apply(this, [this].concat([].slice.call(arguments, 0))) })" [FPtr, FFunction FPtr (FFunction FInt (FFunction FString (FAny (IO Ptr))))] FPtr) p f'
  where
    f' : Ptr -> Int -> String -> IO Ptr
    f' p a b = runJQueryIO $ f a b (MkElement p) >>= getContentPtr

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
setAttrWith : String -> (Int -> String -> Element -> JQueryIO String) -> JQuery -> JQueryIO JQuery
setAttrWith k f q = do
    p <- getContentPtr q
    liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.attr(%1, (function () { return %2.apply(this, [this].concat([].slice.call(arguments, 0))) }))" [FPtr, FString, FFunction FPtr (FFunction FInt (FFunction FString (FAny (IO String))))] FPtr) p k f'
  where
    f' : Ptr -> Int -> String -> IO String
    f' p a b = runJQueryIO $ f a b (MkElement p)

public
before : Content c => c -> JQuery -> JQueryIO JQuery
before c q = do
  c <- getContentPtr c
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.before(%1)" [FPtr, FPtr] FPtr) p c

public
beforeWith : Content c => (Int -> Element -> JQueryIO c) -> JQuery -> JQueryIO JQuery
beforeWith f q = do
    p <- getContentPtr q
    liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.before(function () { return %1.apply(this, [this].concat([].slice.call(arguments, 0))) })" [FPtr, FFunction FPtr (FFunction FInt (FAny (IO Ptr)))] FPtr) p f'
  where
    f' : Ptr -> Int -> IO Ptr
    f' p x = runJQueryIO $ f x (MkElement p) >>= getContentPtr

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
setCSSWith : String -> (Int -> String -> Element -> JQueryIO String) -> JQuery -> JQueryIO JQuery
setCSSWith k f q = do
    p <- getContentPtr q
    liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.css(%1, (function () { return %2.apply(this, [this].concat([].slice.call(arguments, 0))) }))" [FPtr, FString, FFunction FPtr (FFunction FInt (FFunction FString (FAny (IO String))))] FPtr) p k f'
  where
    f' : Ptr -> Int -> String -> IO String
    f' p x y = runJQueryIO $ f x y (MkElement p)

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
setHTMLWith : (Int -> String -> Element -> JQueryIO String) -> JQuery -> JQueryIO JQuery
setHTMLWith f q = do
    p <- getContentPtr q
    liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.html(function () { return %1.apply(this, [this].concat([].slice.call(arguments, 0))) })" [FPtr, FFunction FPtr (FFunction FInt (FFunction FString (FAny (IO String))))] FPtr) p f'
  where
    f' : Ptr -> Int -> String -> IO String
    f' p x y = runJQueryIO $ f x y (MkElement p)

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
setHeightWith : (Int -> Int -> Element -> JQueryIO Int) -> JQuery -> JQueryIO JQuery
setHeightWith f q = do
    p <- getContentPtr q
    liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.height(function () { return %1.apply(this, [this].concat([].slice.call(arguments, 0))) })" [FPtr, FFunction FPtr (FFunction FInt (FFunction FInt (FAny (IO Int))))] FPtr) p f'
  where
    f' : Ptr -> Int -> Int -> IO Int
    f' p x y = runJQueryIO $ f x y (MkElement p)

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
setWidthWith : (Int -> Int -> Element -> JQueryIO Int) -> JQuery -> JQueryIO JQuery
setWidthWith f q = do
    p <- getContentPtr q
    liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.width(function () { return %1.apply(this, [this].concat([].slice.call(arguments, 0))) })" [FPtr, FFunction FPtr (FFunction FInt (FFunction FInt (FAny (IO Int))))] FPtr) p f'
  where
    f' : Ptr -> Int -> Int -> IO Int
    f' p x y = runJQueryIO $ f x y (MkElement p)

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

public
removeAttrList : Foldable t => t String -> JQuery -> JQueryIO JQuery
removeAttrList ss q = do
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.removeAttr(%1)" [FPtr, FString] FPtr) p (unwords $ toList ss)

public
removeAttr : String -> JQuery -> JQueryIO JQuery
removeAttr s = removeAttrList $ the (List String) [s]

public
removeClassList : Foldable t => t String -> JQuery -> JQueryIO JQuery
removeClassList ss q = do
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.removeClass(%1)" [FPtr, FString] FPtr) p (unwords $ toList ss)

public
removeClass : String -> JQuery -> JQueryIO JQuery
removeClass s = removeClassList $ the (List String) [s]

public
removeClassAll : JQuery -> JQueryIO JQuery
removeClassAll q = do
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.removeClass()" [FPtr] FPtr) p

public
removeClassListWith : Foldable t => (Int -> String -> Element -> JQueryIO $ t String) -> JQuery -> JQueryIO JQuery
removeClassListWith f q = do
    p <- getContentPtr q
    liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.removeClass(function () { return %1.apply(this, [this].concat([].slice.call(arguments, 0))) })" [FPtr, FFunction FPtr (FFunction FInt (FFunction FString (FAny (IO String))))] FPtr) p f'
  where
    f' : (Ptr -> Int -> String -> IO String)
    f' p a b = runJQueryIO $ f a b (MkElement p) >>= return . unwords . toList

public
removeClassWith : (Int -> String -> Element -> JQueryIO String) -> JQuery -> JQueryIO JQuery
removeClassWith f = removeClassListWith (the (Int -> String -> Element -> JQueryIO $ List String) (\a => \b => \e => (f a b e) >>= (\s => return [s])))

-- TODO:
-- removeProp

public
replaceAll : Content c => c -> JQuery -> JQueryIO JQuery
replaceAll c q = do
  c <- getContentPtr c
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.replaceAll(%1)" [FPtr, FPtr] FPtr) p c

public
replaceWith : Content c => c -> JQuery -> JQueryIO JQuery
replaceWith c q = do
  c <- getContentPtr c
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.replaceWith(%1)" [FPtr, FPtr] FPtr) p c

public
getScrollLeft : JQuery -> JQueryIO Int
getScrollLeft q = do
  p <- getContentPtr q
  liftIO $ mkForeign (FFun "%0.scrollLeft()" [FPtr] FInt) p

public
setScrollLeft : Int -> JQuery -> JQueryIO JQuery
setScrollLeft i q = do
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.scrollLeft(%1)" [FPtr, FInt] FPtr) p i

public
getScrollTop : JQuery -> JQueryIO Int
getScrollTop q = do
  p <- getContentPtr q
  liftIO $ mkForeign (FFun "%0.scrollTop()" [FPtr] FInt) p

public
setScrollTop : Int -> JQuery -> JQueryIO JQuery
setScrollTop i q = do
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.scrollTop(%1)" [FPtr, FInt] FPtr) p i

public
getText : JQuery -> JQueryIO String
getText q = do
  p <- getContentPtr q
  liftIO $ mkForeign (FFun "%0.text()" [FPtr] FString) p

public
setText : String -> JQuery -> JQueryIO JQuery
setText v q = do
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.text(%1)" [FPtr, FString] FPtr) p v

public
setTextWith : (Int -> String -> Element -> JQueryIO String) -> JQuery -> JQueryIO JQuery
setTextWith f q = do
    p <- getContentPtr q
    liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.text(function () { return %1.apply(this, [this].concat([].slice.call(arguments, 0))) })" [FPtr, FFunction FPtr (FFunction FInt (FFunction FString (FAny (IO String))))] FPtr) p f'
  where
    f' : Ptr -> Int -> String -> IO String
    f' p x y = runJQueryIO $ f x y (MkElement p)

public
toggleClassList : Foldable t => t String -> JQuery -> JQueryIO JQuery
toggleClassList ss q = do
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.toggleClass(%1)" [FPtr, FString] FPtr) p (unwords $ toList ss)

public
toggleClass : String -> JQuery -> JQueryIO JQuery
toggleClass s = toggleClassList $ the (List String) [s]

public
toggleClassListWith : Foldable t => (Int -> String -> Element -> JQueryIO $ t String) -> JQuery -> JQueryIO JQuery
toggleClassListWith f q = do
    p <- getContentPtr q
    liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.toggleClass(function () { return %1.apply(this, [this].concat([].slice.call(arguments, 0))) })" [FPtr, FFunction FPtr (FFunction FInt (FFunction FString (FAny (IO String))))] FPtr) p f'
  where
    f' : (Ptr -> Int -> String -> IO String)
    f' p a b = runJQueryIO $ f a b (MkElement p) >>= return . unwords . toList

public
toggleClassWith : (Int -> String -> Element -> JQueryIO String) -> JQuery -> JQueryIO JQuery
toggleClassWith f = toggleClassListWith (the (Int -> String -> Element -> JQueryIO $ List String) (\a => \b => \e => (f a b e) >>= (\s => return [s])))

public
unwrap : JQuery -> JQueryIO JQuery
unwrap q = do
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.unwrap()" [FPtr] FPtr) p