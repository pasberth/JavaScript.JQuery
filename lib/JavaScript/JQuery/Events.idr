module JavaScript.JQuery.Events

import JavaScript.JQuery.Types

public
ready : JQueryIO (the Type ()) -> JQueryIO (the Type ())
ready q = liftIO $ mkForeign (FFun "jQuery(%0)" [FFunction FUnit (FAny (IO ()))] FUnit) (runJQueryIO . const q)
