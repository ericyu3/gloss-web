{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE DeriveDataTypeable   #-}

module Instances where

import Control.Monad
import Crypto.Hash.MD5
import Data.Aeson
import Data.Ix
import Data.Typeable
import Graphics.GD hiding (Point)
import Graphics.Gloss
import System.IO.Unsafe
import System.Directory

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Base64 as B64


deriving instance Typeable Picture


bmpToPng :: Int -> Int -> ByteString -> IO ByteString
bmpToPng w h bs = withImage (newImage (w,h)) $ \img -> do
    forM (range ((0,0), (w-1,h-1))) $ \(x,y) -> do
        let i         = w * y + x
        let [r,g,b,a] = map fromIntegral $ B.unpack $ B.take 4 $ B.drop i bs
        let c         = rgba r g b (a `div` 2)
        setPixel (x,y) c img
    savePngByteString img


cachedBmpToPng :: Int -> Int -> ByteString -> IO ByteString
cachedBmpToPng w h bs = do
    let digest = BC.unpack $ B64.encode $ hash bs
    let fn     = "tmp/" ++ digest ++ ".png"
    e <- doesFileExist fn
    case e of
        True  -> B.readFile fn
        False -> do res <- bmpToPng w h bs
                    B.writeFile fn res
                    return res


toBitmap :: Int -> Int -> ByteString -> Text
toBitmap w h bs = unsafePerformIO $ do
    png <- cachedBmpToPng w h bs
    return ("data:image/png;base64," `T.append` T.decodeUtf8 (B64.encode png))


instance ToJSON Picture where
    toJSON Blank             = Null
    toJSON (Polygon path)    = object [ "t" .= ("p" :: Text), "p" .= path ]
    toJSON (Line path)       = object [ "t" .= ("l" :: Text), "p" .= path ]
    toJSON (Circle r)        = object [ "t" .= ("c" :: Text), "r" .= r ]
    toJSON (ThickCircle r w) = object [ "t" .= ("h" :: Text), "w" .= w, "r" .= r ]
    toJSON (Text str)        = object [ "t" .= ("t" :: Text), "c" .= T.pack str ]
    toJSON (Bitmap w h bmp)  = object [ "t" .= ("b" :: Text), "c" .= toBitmap w h bmp ]
    toJSON (Color c p)       = let (r,g,b,a) = rgbaOfColor c
                               in  object [ "t" .= ("z" :: Text)
                                          , "r" .= (round (255 * r) :: Int)
                                          , "g" .= (round (255 * g) :: Int)
                                          , "b" .= (round (255 * b) :: Int)
                                          , "a" .= (round (255 * a) :: Int)
                                          , "p" .= p ]
    toJSON (Translate x y p) = object [ "t" .= ("x" :: Text)
                                      , "x" .= x
                                      , "y" .= y
                                      , "p" .= p ]
    toJSON (Rotate r p)      = object [ "t" .= ("r" :: Text)
                                      , "r" .= r
                                      , "p" .= p ]
    toJSON (Scale x y p)     = object [ "t" .= ("s" :: Text)
                                      , "x" .= x
                                      , "y" .= y
                                      , "p" .= p ]
    toJSON (Pictures ps)     = toJSON ps


instance ToJSON Point where
    toJSON (x,y) = object [ "x" .= x, "y" .= y ]

