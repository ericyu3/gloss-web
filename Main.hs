{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

module Main where

import Control.Applicative
import Control.Monad
import Crypto.Hash.MD5
import Data.Aeson
import Data.IORef
import Data.Ix
import Data.Typeable
import Graphics.Gloss
import Graphics.GD hiding (Point)
import Language.Haskell.Interpreter
import Snap.Http.Server
import Snap.Types
import Snap.Util.FileServe
import System.FilePath
import System.IO.Unsafe
import System.Directory
import System.Random
import Text.Templating.Heist
import Text.Templating.Heist.TemplateDirectory
import Text.XmlHtml

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Base64 as B64

import Data.Map (Map)
import qualified Data.Map as M

import Data.Vector (Vector)
import qualified Data.Vector as V

deriving instance Typeable Picture

main = do
    Right heist <- loadTemplates "web" (emptyTemplateState "web")
    quickHttpServe $
        route [ ("displayInBrowser", displayInBrowser heist) ]
        <|> serveDirectory "web"


displayInBrowser heist = do
    src <- maybe mzero return =<< getParam "source"
    res <- liftIO $ do
        fn <- chooseFile ".hs"
        B.writeFile fn src
        runInterpreter $ do
            loadModules [ fn ]
            themod <- head <$> getLoadedModules
            setTopLevelModules [ themod ]
            setImports ["Graphics.Gloss", "Graphics.Gloss.Data.Picture"]
            interpret "picture" (undefined :: Picture)
    case res of
        Left errs  -> displayErrors heist errs
        Right pic -> displaySuccess heist pic


displaySuccess heist pic = do
    Just (b, t) <- renderTemplate
        (bindSplice "displayScript" (scrSplice pic) heist)
        "display"
    modifyResponse (setContentType t)
    writeBuilder b
  where
    scrSplice pic = return [ Element "script" [("type", "text/javascript")] [
        TextNode "picture = ",
        TextNode $ T.decodeUtf8 $ B.concat $ LB.toChunks $ encode pic,
        TextNode ";"
        ]]


displayErrors heist errs = do
    let errStrs = case errs of
            WontCompile es -> map errMsg es
            GhcException e -> [ e ]
            NotAllowed   e -> [ e ]
            UnknownError e -> [ e ]
    Just (b, t) <- renderTemplate
        (bindSplice "showErrors" (errSplice errStrs) heist)
        "displayErrors"
    modifyResponse (setContentType t)
    writeBuilder b
  where
    errSplice errs = return [Element "ul" []
        (map (\s -> Element "li" [] [TextNode (T.pack s)]) errs)]


chooseFile :: String -> IO String
chooseFile sfx = do
    let chars = ['0'..'9'] ++ ['a'..'z']
        len   = length chars
    base <- replicateM 16 $ fmap (chars !!) $ randomRIO (0, len - 1)
    return ("tmp" </> (base ++ sfx))


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

