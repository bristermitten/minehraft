import qualified Data.ByteString.Char8 as BS
import Data.Serialize
import DataTypes
import Packets
import Prelude hiding (max)
import qualified Data.Aeson as A

main :: IO ()
main = do
  putStrLn "control"
  let response =
        Response
          PingResponse
            { version = newVersion "1.16.5" 754,
              players =
                Players
                  { max = 100,
                    online = 0,
                    sample = []
                  },
              description = chat "Hello Haskell!"
            }
  let packetData = runPut $ putVarInt 168
  BS.putStrLn packetData
  return ()
