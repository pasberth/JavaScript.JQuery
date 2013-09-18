module JavaScript.JQuery.Events

import JavaScript.JQuery.Types

public
ready : IO () -> IO ()
ready io = mkForeign (FFun "jQuery(%0)" [FFunction FUnit (FAny (IO ()))] FUnit) (const io)
