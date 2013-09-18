module JavaScript.JQuery.Types

abstract
data JQueryIO : {a : Type} ->  a -> Type where
  MkJQueryIO : {a : Type} -> IO a -> JQueryIO a

instance Functor JQueryIO where
  map f (MkJQueryIO io) = MkJQueryIO $ map f io

instance Applicative (the (Type -> Type) JQueryIO) where
  pure = MkJQueryIO . return
  (MkJQueryIO m) <$> (MkJQueryIO k) = MkJQueryIO (m <$> k)

instance Monad (the (Type -> Type) JQueryIO) where
  (MkJQueryIO m) >>= k = MkJQueryIO $ do
    x <- m
    let (MkJQueryIO k') = k x
    k'

public
liftIO : IO a -> JQueryIO a
liftIO = MkJQueryIO

public
runJQueryIO : JQueryIO a -> IO a
runJQueryIO q = case q of
  MkJQueryIO io => io

public
data JQuery : Type where
  MkJQuery : Ptr -> JQuery

public
data Element : Type where
  MkElement : Ptr -> Element

public
data Event : Type where
  MkEvent : Ptr -> Event

public
data EventType
  = OnResize
  | OnScroll
  | OnReady
  | OnBlur
  | OnChange
  | OnClick
  | OnDblclick
  | OnFocus
  | OnFocusin
  | OnFocusout
  | OnSelect
  | OnSubmit
  | OnKeydown
  | OnKeypress
  | OnKeyup
  | OnHover
  | OnMousedown
  | OnMouseenter
  | OnMouseleave
  | OnMousemove
  | OnMouseout
  | OnMouseover
  | OnMouseup
  | OnToggle

public
class Selector (s : Type) where
  getSelectorPtr : s -> JQueryIO Ptr

public
class Content (c : Type) where
  getContentPtr : c -> JQueryIO Ptr

public
class CustomEventType (t : Type) where
  customEventTypeToString : t -> String

instance Selector String where
  getSelectorPtr s = do
    p <- liftIO $ mkForeign (FFun "%0" [FString] FPtr) s
    return p

instance Content JQuery where
  getContentPtr q = case q of
    MkJQuery p => return p

instance Content Element where
  getContentPtr e = case e of
    MkElement p => return p

instance Content String where
  getContentPtr s = do
    p <- liftIO $ mkForeign (FFun "%0" [FString] FPtr) s
    return p

instance CustomEventType String where
  customEventTypeToString = id

instance CustomEventType EventType where
  customEventTypeToString OnResize = "resize"
  customEventTypeToString OnScroll = "scroll"
  customEventTypeToString OnReady = "ready"
  customEventTypeToString OnBlur = "blur"
  customEventTypeToString OnChange = "change"
  customEventTypeToString OnClick = "click"
  customEventTypeToString OnDblclick = "dblclick"
  customEventTypeToString OnFocus = "focus"
  customEventTypeToString OnFocusin = "focusin"
  customEventTypeToString OnFocusout = "focusout"
  customEventTypeToString OnSelect = "select"
  customEventTypeToString OnSubmit = "submit"
  customEventTypeToString OnKeydown = "keydown"
  customEventTypeToString OnKeypress = "keypress"
  customEventTypeToString OnKeyup = "keyup"
  customEventTypeToString OnHover = "hover"
  customEventTypeToString OnMousedown = "mousedown"
  customEventTypeToString OnMouseenter = "mouseenter"
  customEventTypeToString OnMouseleave = "mouseleave"
  customEventTypeToString OnMousemove = "mousemove"
  customEventTypeToString OnMouseout = "mouseout"
  customEventTypeToString OnMouseover = "mouseover"
  customEventTypeToString OnMouseup = "mouseup"
  customEventTypeToString OnToggle = "toggle"

public
liftIOPtrToJQueryIOJQuery : IO Ptr -> JQueryIO JQuery
liftIOPtrToJQueryIOJQuery m = do
  p <- liftIO m
  return $ MkJQuery p
