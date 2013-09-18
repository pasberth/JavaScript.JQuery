module JavaScript.JQuery.Events

import JavaScript.JQuery.Types

abstract
eventFunctionToFEventFunction : (Event -> Element -> JQueryIO $ the Type ()) -> Ptr -> Ptr -> IO $ the Type ()
eventFunctionToFEventFunction f eventPtr elemPtr = runJQueryIO $ f (MkEvent eventPtr) (MkElement elemPtr)

abstract
fEventFunction : FTy
fEventFunction = FFunction FPtr (FFunction FPtr (FAny (IO ())))

public
customBind : CustomEventType t => t -> (Event -> Element -> JQueryIO $ the Type ()) -> JQuery -> JQueryIO JQuery
customBind t f q = do
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.bind(%1, (function () { return %2.apply(this, [this].concat([].slice.call(arguments, 0))) }))" [FPtr, FString, fEventFunction] FPtr) p (customEventTypeToString t) (eventFunctionToFEventFunction f)

public
bind : EventType -> (Event -> Element -> JQueryIO $ the Type ()) -> JQuery -> JQueryIO JQuery
bind = customBind

public
customDelegate : (Selector s, CustomEventType t) => s -> t -> (Event -> Element -> JQueryIO $ the Type ()) -> JQuery -> JQueryIO JQuery
customDelegate s t f q = do
  s <- getSelectorPtr s
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.delegate(%1, %2, (function () { return %3.apply(this, [this].concat([].slice.call(arguments, 0))) }))" [FPtr, FPtr, FString, fEventFunction] FPtr) p s (customEventTypeToString t) (eventFunctionToFEventFunction f)

public
delegate : Selector s => s -> EventType -> (Event -> Element -> JQueryIO $ the Type ()) -> JQuery -> JQueryIO JQuery
delegate = customDelegate

-- TODO:
-- .live()

public
offEventList : Foldable t => t String -> JQuery -> JQueryIO JQuery
offEventList ss q = do
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.off(%1)" [FPtr, FString] FPtr) p (unwords $ toList ss)

public
offSelector : (Selector s, Foldable t) => t String -> s -> JQuery -> JQueryIO JQuery
offSelector ss s q = do
  s <- getSelectorPtr s
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.off(%1, %2)" [FPtr, FString, FPtr] FPtr) p (unwords $ toList ss) s

public
onEventList : Foldable t => t String -> (Event -> Element -> JQueryIO $ the Type ()) -> JQuery -> JQueryIO JQuery
onEventList ss f q = do
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.on(%1, %2)" [FPtr, FString, fEventFunction] FPtr) p (unwords $ toList ss) (eventFunctionToFEventFunction f)

public
ready : JQueryIO $ the Type () -> JQueryIO $ the Type ()
ready q = liftIO $ mkForeign (FFun "jQuery(%0)" [FFunction FUnit (FAny (IO ()))] FUnit) (runJQueryIO . const q)
