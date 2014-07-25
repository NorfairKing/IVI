module Scripts.ScriptsList where
import Script
import Scripts.TestScript.TestScript
scripts :: [(String, IVIScript)] 
scripts = [
            ("name", Script "name" execute)
          ]
