{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Serialize

import Network.HTTP.AltSvc

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

arbitraryByteString :: Gen ByteString
arbitraryByteString = B.pack <$> arbitrary

arbitraryToken :: Gen ByteString
arbitraryToken = B.pack <$> listOf1 arbitraryTokenByte
  where
    arbitraryTokenByte = oneof
        [ elements [ 0x21, 0x23, 0x24, 0x25, 0x26, 0x27, 0x2a, 0x2b
                   , 0x2d, 0x2e, 0x5e, 0x5f, 0x60, 0x7c, 0x7e ]
        , choose (0x30, 0x39)  -- 0-9
        , choose (0x41, 0x5a)  -- A-Z
        , choose (0x61, 0x7a)  -- a-z
        ]

arbitraryParam :: Gen (ByteString, ByteString)
arbitraryParam = (,) <$> arbitraryToken <*> arbitraryByteString

arbitraryProtocolId :: Gen ByteString
arbitraryProtocolId = B.pack <$> listOf1 arbitrary

instance Arbitrary AltValue where
    arbitrary = AltValue
        <$> arbitraryProtocolId
        <*> arbitraryByteString
        <*> choose (0,65535)
        <*> listOf arbitraryParam

instance Arbitrary AltSvc where
    arbitrary = fmap AltSvc arbitrary

vectors :: [(AltSvc, ByteString)]
vectors =
    [ ( AltSvc []
      , "clear")
    , ( AltSvc [AltValue "h2" "" 8000 []]
      , "h2=\":8000\"")
    , ( AltSvc [AltValue "h2" "new.example.org" 80 []]
      , "h2=\"new.example.org:80\"")
    , ( AltSvc [AltValue "w=x:y#z" "" 80 []]
      , "w%3Dx%3Ay#z=\":80\"")
    , ( AltSvc [AltValue "x%y" "" 80 []]
      , "x%25y=\":80\"")
    , ( AltSvc [AltValue "h2" "alt.example.com" 8000 [], AltValue "h2" "" 443 []]
      , "h2=\"alt.example.com:8000\", h2=\":443\"")
    , ( AltSvc [AltValue "h2" "" 443 [("ma", "3600")]]
      , "h2=\":443\"; ma=3600")
    , ( AltSvc [AltValue "h2" "" 443 [("ma", "2592000"), ("persist", "1")]]
      , "h2=\":443\"; ma=2592000; persist=1")
    ]

runGetFull :: Get a -> ByteString -> Either String a
runGetFull parse bs = handle (runGetPartial parse bs)
  where
    handle result = case result of
        Fail i _        -> Left i
        Partial f       -> handle (f B.empty)  -- confirm complete
        Done a rb
            | B.null rb -> Right a  -- fully parsed
            | otherwise -> Left ("Remaining bytes: " ++ show rb)

main :: IO ()
main = defaultMain $ testGroup "AltSvc"
    [ localOption (QuickCheckMaxSize 10) $ testProperty "property" $ \v ->
        runGetFull get (runPut $ put v) === Right (v :: AltSvc)
    , testGroup "vectors" $
        let toCase i (v, bs) = testCase (show i) $ bs @=? runPut (put v)
         in zipWith toCase [(1::Int)..] vectors
    ]
