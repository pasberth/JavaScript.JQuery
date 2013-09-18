module JavaScript.JQuery.Events

import JavaScript.JQuery.Types

abstract
eventFunctionToFEventFunction : (Event -> Element -> JQueryIO $ the Type ()) -> Ptr -> Ptr -> IO $ the Type ()
eventFunctionToFEventFunction f eventPtr elemPtr = runJQueryIO $ f (MkEvent eventPtr) (MkElement elemPtr)

public
customBind : CustomEventType t => t -> (Event -> Element -> JQueryIO $ the Type ()) -> JQuery -> JQueryIO JQuery
customBind t f q = do
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.bind(%1, (function () { return %2.apply(this, [this].concat([].slice.call(arguments, 0))) }))" [FPtr, FString, FFunction FPtr (FFunction FPtr (FAny (IO ())))] FPtr) p (customEventTypeToString t) (eventFunctionToFEventFunction f)

public
bind : EventType -> (Event -> Element -> JQueryIO $ the Type ()) -> JQuery -> JQueryIO JQuery
bind = customBind

public
customDelegate : (Selector s, CustomEventType t) => s -> t -> (Event -> Element -> JQueryIO $ the Type ()) -> JQuery -> JQueryIO JQuery
customDelegate s t f q = do
  s <- getSelectorPtr s
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.delegate(%1, %2, (function () { return %3.apply(this, [this].concat([].slice.call(arguments, 0))) }))" [FPtr, FPtr, FString, FFunction FPtr (FFunction FPtr (FAny (IO ())))] FPtr) p s (customEventTypeToString t) (eventFunctionToFEventFunction f)

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
customOnEventList : CustomEventType t => List t -> (Event -> Element -> JQueryIO $ the Type ()) -> JQuery -> JQueryIO JQuery
customOnEventList ss f q = do
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.on(%1, %2)" [FPtr, FString, FFunction FPtr (FFunction FPtr (FAny (IO ())))] FPtr) p (unwords $ map customEventTypeToString ss) (eventFunctionToFEventFunction f)

public
onEventList : List EventType -> (Event -> Element -> JQueryIO $ the Type ()) -> JQuery -> JQueryIO JQuery
onEventList = customOnEventList

public
customOnSelector : (CustomEventType t, Selector s) => List t -> s -> (Event -> Element -> JQueryIO $ the Type ()) -> JQuery -> JQueryIO JQuery
customOnSelector ss s f q = do
  s <- getSelectorPtr s
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.on(%1, %2, %3)" [FPtr, FString, FPtr, FFunction FPtr (FFunction FPtr (FAny (IO ())))] FPtr) p (unwords $ toList $ map customEventTypeToString ss) s (eventFunctionToFEventFunction f)

public
onSelector : Selector s => List EventType -> s -> (Event -> Element -> JQueryIO $ the Type ()) -> JQuery -> JQueryIO JQuery
onSelector = customOnSelector

public
customOneEventList : CustomEventType t => List t -> (Event -> Element -> JQueryIO $ the Type ()) -> JQuery -> JQueryIO JQuery
customOneEventList ss f q = do
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.one(%1, %2)" [FPtr, FString, FFunction FPtr (FFunction FPtr (FAny (IO ())))] FPtr) p (unwords $ toList $ map customEventTypeToString ss) (eventFunctionToFEventFunction f)

public
oneEventList : List EventType -> (Event -> Element -> JQueryIO $ the Type ()) -> JQuery -> JQueryIO JQuery
oneEventList = customOneEventList

public
customOneSelector : (CustomEventType t, Selector s) => List t -> s -> (Event -> Element -> JQueryIO $ the Type ()) -> JQuery -> JQueryIO JQuery
customOneSelector ss s f q = do
  s <- getSelectorPtr s
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.one(%1, %2, %3)" [FPtr, FString, FPtr, FFunction FPtr (FFunction FPtr (FAny (IO ())))] FPtr) p (unwords $ toList $ map customEventTypeToString ss) s (eventFunctionToFEventFunction f)

public
oneSelector : Selector s => List EventType -> s -> (Event -> Element -> JQueryIO $ the Type ()) -> JQuery -> JQueryIO JQuery
oneSelector = customOneSelector

public
customTrigger : CustomEventType t => t -> JQuery -> JQueryIO JQuery
customTrigger t q = do
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.trigger(%1)" [FPtr, FString] FPtr) p (customEventTypeToString t)

public
trigger : EventType -> JQuery -> JQueryIO JQuery
trigger = customTrigger

public
customTriggerHandler : CustomEventType t => t -> JQuery -> JQueryIO JQuery
customTriggerHandler t q = do
  p <- getContentPtr q
  liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.triggerHandler(%1)" [FPtr, FString] FPtr) p (customEventTypeToString t)

public
triggerHandler : EventType -> JQuery -> JQueryIO JQuery
triggerHandler = customTriggerHandler

-- TODO:
-- .unbind()


-- TODO:
-- .undelegate()

public
ready : JQueryIO $ the Type () -> JQueryIO $ the Type ()
ready q = liftIO $ mkForeign (FFun "jQuery(%0)" [FFunction FUnit (FAny (IO ()))] FUnit) (runJQueryIO . const q)
