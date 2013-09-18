module JavaScript.JQuery.Events

import JavaScript.JQuery.Types

-- TODO:
-- JQueryIO () -> JQueryIO ()
public
ready : IO () -> IO ()
ready io = mkForeign (FFun "jQuery(%0)" [FFunction FUnit (FAny (IO ()))] FUnit) (const io)
