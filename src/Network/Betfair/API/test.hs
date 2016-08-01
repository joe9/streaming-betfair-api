{-# LANGUAGE OverloadedStrings #-}

import System.IO.Streams (InputStream, OutputStream, stdout)
import qualified System.IO.Streams as Streams
import Network.Http.Client
import Blaze.ByteString.Builder (Builder)
import qualified Blaze.ByteString.Builder as Builder (flush, fromByteString,
                                                      toByteString)
import qualified Blaze.ByteString.Builder.HTTP as Builder (chunkedTransferEncoding, chunkedTransferTerminator)


main :: IO ()
main = do
    c <- openConnection "stream-api.betfair.com" 443
--     c <- establishConnection url

--     receiveResponse c debugHandler
    x <- receiveResponse c concatHandler
--     x <- receiveResponse c concatHandler'
    print x

--     receiveResponse c (\p i -> do
--         Streams.connect i stdout)

-- --     q <- buildRequest $ do
-- --         http GET "/time"
-- --         setAccept "text/plain"

-- -- --     sendRequest c q emptyBody
-- --     sendRequest c q (\o ->
-- --         Streams.write (Just (Builder.fromByteString "Hello World\n")) o)

--     receiveResponse c (\p i -> do
--         Streams.connect i stdout)

    closeConnection c

url :: URL
url = "https://stream-api-integration.betfair.com:443/"
-- url = "https://stream-api.betfair.com/api/request"
