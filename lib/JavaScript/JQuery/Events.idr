module JavaScript.JQuery.Events

import JavaScript.JQuery.Types

public
customBind : CustomEventType t => t -> (Event -> Element -> JQueryIO $ the Type ()) -> JQuery -> JQueryIO JQuery
customBind t f q = do
    p <- getContentPtr q
    liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.bind(%1, (function () { return %2.apply(this, [this].concat([].slice.call(arguments, 0))) }))" [FPtr, FString, FFunction FPtr (FFunction FPtr (FAny (IO ())))] FPtr) p s f'
  where
    s : String
    s = customEventTypeToString t

    f' : Ptr -> Ptr -> IO $ the Type ()
    f' evp elp = runJQueryIO $ f (MkEvent evp) (MkElement elp)

public
bind : EventType -> (Event -> Element -> JQueryIO $ the Type ()) -> JQuery -> JQueryIO JQuery
bind = customBind

public
customDelegate : (Selector s, CustomEventType t) => s -> t -> (Event -> Element -> JQueryIO $ the Type ()) -> JQuery -> JQueryIO JQuery
customDelegate s t f q = do
    s <- getSelectorPtr s
    p <- getContentPtr q
    liftIOPtrToJQueryIOJQuery $ mkForeign (FFun "%0.delegate(%1, %2, (function () { return %3.apply(this, [this].concat([].slice.call(arguments, 0))) }))" [FPtr, FPtr, FString, FFunction FPtr (FFunction FPtr (FAny (IO ())))] FPtr) p s (customEventTypeToString t) f'
  where
    f' : Ptr -> Ptr -> IO $ the Type ()
    f' evp elp = runJQueryIO $ f (MkEvent evp) (MkElement elp)

public
delegate : Selector s => s -> EventType -> (Event -> Element -> JQueryIO $ the Type ()) -> JQuery -> JQueryIO JQuery
delegate = customDelegate

public
ready : JQueryIO $ the Type () -> JQueryIO $ the Type ()
ready q = liftIO $ mkForeign (FFun "jQuery(%0)" [FFunction FUnit (FAny (IO ()))] FUnit) (runJQueryIO . const q)
