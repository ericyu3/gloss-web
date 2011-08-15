{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MagicHash         #-}

module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Data.Aeson
import Data.IORef
import Data.Typeable
import GHC.Exts (unsafeCoerce#)
import Graphics.Gloss
import Snap.Http.Server
import Snap.Types
import Snap.Util.FileServe
import System.FilePath
import System.Random
import Text.Templating.Heist
import Text.Templating.Heist.TemplateDirectory
import Text.XmlHtml

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Data.Map (Map)
import qualified Data.Map as M

import Data.Vector (Vector)
import qualified Data.Vector as V

import qualified GHC        as GHC
import qualified MonadUtils as GHC
import qualified GHC.Paths  as GHC
import qualified Bag        as GHC
import qualified Outputable as GHC
import qualified ErrUtils   as GHC
import qualified HscTypes   as GHC
import qualified DynFlags   as GHC

import Instances

main = do
    Right heist <- loadTemplates "web" (emptyTemplateState "web")
    quickHttpServe $
        route [ ("displayInBrowser", displayInBrowser heist) ]
        <|> serveDirectory "web"


displayInBrowser heist = do
    src <- maybe mzero return =<< getParam "source"
    res <- liftIO $ getPicture src
    case res of
        Left eref -> do errs <- liftIO $ readIORef eref
                        displayErrors heist errs
        Right pic -> displaySuccess heist pic


getPicture src = do
    fn <- chooseFileName ".hs"
    B.writeFile fn src
    codeErrors <- newIORef []
    GHC.defaultErrorHandler (addErrorTo codeErrors)
        $ GHC.runGhc (Just GHC.libdir)
        $ GHC.handleSourceError (handle codeErrors) $ do
            dflags <- GHC.getSessionDynFlags
            GHC.setSessionDynFlags $ dflags {
                GHC.ghcMode = GHC.CompManager,
                GHC.ghcLink = GHC.LinkInMemory,
                GHC.hscTarget = GHC.HscInterpreted,
                GHC.safeHaskell = GHC.Sf_Safe,
                GHC.packageFlags = [GHC.TrustPackage "gloss"],
                GHC.log_action = addErrorTo codeErrors
                }
            target <- GHC.guessTarget fn Nothing
            GHC.setTargets [target]
            r <- fmap GHC.succeeded (GHC.load GHC.LoadAllTargets)
            case r of
                True -> do
                    mods <- GHC.getModuleGraph
                    GHC.setContext [ GHC.ms_mod (head mods) ]
                                   [ GHC.simpleImportDecl
                                       (GHC.mkModuleName "Graphics.Gloss") ]
                    v <- GHC.compileExpr "picture :: Picture"
                    return (Right (unsafeCoerce# v :: Picture))
                False -> return (Left codeErrors)
  where
    handle ref se = do
        let errs    = GHC.bagToList (GHC.srcErrorMessages se)
            nice e  = GHC.showSDoc (GHC.errMsgShortDoc e)
            cleaned = map nice errs
        GHC.liftIO $ modifyIORef ref (++ cleaned)
        return (Left ref)
    addErrorTo ref _ span style msg =
        let niceError = GHC.showSDoc
                $ GHC.withPprStyle style $ GHC.mkLocMessage span msg
        in  modifyIORef ref (++ [ niceError ])


displaySuccess :: TemplateState Snap -> Picture -> Snap ()
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
    Just (b, t) <- renderTemplate
        (bindSplice "showErrors" (errSplice errs) heist)
        "displayErrors"
    modifyResponse (setContentType t)
    writeBuilder b
  where
    errSplice errs = return [Element "ul" []
        (map (\s -> Element "li" [] [TextNode (T.pack s)]) errs)]


chooseFileName :: String -> IO String
chooseFileName sfx = do
    let chars = ['0'..'9'] ++ ['a'..'z']
        len   = length chars
    base <- replicateM 16 $ fmap (chars !!) $ randomRIO (0, len - 1)
    return ("tmp" </> (base ++ sfx))

