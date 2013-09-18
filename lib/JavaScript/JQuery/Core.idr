module JavaScript.JQuery.Core

import JavaScript.JQuery.Types

public
jQueryContent : Content c => c -> JQueryIO JQuery
jQueryContent c = do
  c <- getContentPtr c
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "jQuery(%0)"  [FPtr] FPtr) c

public
jQuerySelectorContext : Selector s => s -> Element -> JQueryIO JQuery
jQuerySelectorContext s e = do
  s <- getSelectorPtr s
  e <- getContentPtr e
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "jQuery(%0, %1)" [FPtr, FPtr] FPtr) s e

public
holdReady : Bool -> JQuery -> JQueryIO JQuery
holdReady b q = do
  let s = if b then "%0.holdReady(true)" else "%0.holdReady(false)"
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun s  [FPtr] FPtr) p

public
noConflict : JQuery -> JQueryIO JQuery
noConflict q = do
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.noConflict()" [FPtr] FPtr) p


public
noConflictRemoveAll : Bool -> JQuery -> JQueryIO JQuery
noConflictRemoveAll b q = do
  let s = if b then "%0.noConflict(true)" else "%0.noConflict(false)"
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun s  [FPtr] FPtr) p