-- This file is generated, there is no use in modifying it directly
-- Please just make sure the .ivi files are in order
module Scripts.ScriptsList where
import Script
import Scripts.TestScript2.TestScript
import Scripts.TestScript.TestScript

scripts :: [(String, IVIScript)] 
scripts = [
              ("test1", Script "test1" Scripts.TestScript.TestScript.execute)
            , ("test2", Script "test2" Scripts.TestScript2.TestScript.execute)
          ]
