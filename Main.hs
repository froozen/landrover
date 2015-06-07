{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getArgs)
import System.Directory (doesFileExist, doesDirectoryExist)
import Data.List (isSuffixOf)
import Control.Monad (liftM2, void)
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Text.Pandoc (def, readMarkdown, writeHtmlString)
import qualified Data.Text as T

-- | The monad of landrover
newtype LandroverM a = LandroverM {
    unwrapL :: MaybeT IO a
    }
  deriving (Functor, Applicative, MonadIO)

instance Monad LandroverM where
    a >>= f = LandroverM (unwrapL a >>= unwrapL . f)
    fail err = do
        liftIO . putStrLn $ err
        LandroverM . fail $ err

-- | Run a LandroverM
runLandroverM :: LandroverM a -> IO (Maybe a)
runLandroverM = runMaybeT . unwrapL

-- | Retrieve and validate the paths supplied via arguments
getPaths :: LandroverM (FilePath, FilePath)
getPaths = do
    args <- liftIO getArgs
    case args of
      [markdown, presentation] -> liftM2 (,)
              (validateMarkdownPath markdown)
              (validatePresentationPath presentation)
      _ -> fail "Usage: landrover [markdown] [presentation]"

-- | Validate the path to the markdown file by assuring it exists
validateMarkdownPath :: FilePath -> LandroverM FilePath
validateMarkdownPath path = do
    exists <- liftIO . doesFileExist $ path
    if exists
       then return path
       else fail $ "Can't find markdown at " ++ path

-- | Validate the path to the presentation folder, checking wether it and
--   the template file exist
validatePresentationPath :: FilePath -> LandroverM FilePath
validatePresentationPath path = do
    -- Make sure the path ends with a '/'
    let fixedPath = if "/" `isSuffixOf` path
        then path
        else path ++ "/"

    presentationExists <- liftIO . doesDirectoryExist $ fixedPath
    if presentationExists
       then do
           templateExists <- liftIO . doesFileExist $
             fixedPath ++ "template.html"
           if templateExists
              then return fixedPath
              else fail $ "Can't find template at " ++
                fixedPath ++ "template.html"
       else fail $ "Can't find presentation in " ++ path

-- | Try converting the markdown to html
convertMarkdown :: FilePath -> LandroverM String
convertMarkdown path = do
    markdown <- liftIO . readFile $ path
    case readMarkdown def markdown of
      Right pandoc -> return . writeHtmlString def $ pandoc
      Left err -> fail $ "Error reading markdown: " ++ show err

-- | Convert the html code into reveal.js slides
slidifyHtml :: String -> T.Text
slidifyHtml html = foldr1 T.append sections
    where sections = map surround . T.splitOn "<hr />" $ packed
          packed = T.pack html
          surround text = "<section>" `T.append` text `T.append` "</section>"

-- | Insert the slides into the template and save it as index.html
insertIntoTemplate :: FilePath -> T.Text -> LandroverM ()
insertIntoTemplate presentationPath insert = do
    content <- liftIO . readFile $ presentationPath ++ "template.html"
    let template = T.pack content
        output = T.replace token insert template
    liftIO . writeFile (presentationPath ++ "index.html") . T.unpack $ output

    where token = "<!-- SLIDES -->"

main = void .  runLandroverM $ do
        (markdown, presentation) <- getPaths
        html <- convertMarkdown markdown
        let slides = slidifyHtml html
        insertIntoTemplate presentation slides
