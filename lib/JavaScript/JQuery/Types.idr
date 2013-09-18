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
class Selector (s : Type) where
  getSelectorPtr : s -> JQueryIO Ptr

public
class Content (c : Type) where
  getContentPtr : c -> JQueryIO Ptr

public
class EventType (t : Type) where
  eventTypeToString : t -> String

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

instance EventType String where
  eventTypeToString = id

public
liftIOPtrToJQueryIOJQuery : IO Ptr -> JQueryIO JQuery
liftIOPtrToJQueryIOJQuery m = do
  p <- liftIO m
  return $ MkJQuery p
