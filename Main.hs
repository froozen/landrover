module Main where

import System.Environment (getArgs)
import System.Directory (doesFileExist, doesDirectoryExist)
import Data.List (isSuffixOf)
import Control.Monad (liftM2)
import Text.Pandoc (def, readMarkdown, writeHtmlString)
import qualified Data.Text as T

-- | Retrieve and validate the paths supplied via arguments
getPaths :: IO (Maybe (FilePath, FilePath))
getPaths = do
    args <- getArgs
    case args of
      markdown:presentation:_ -> do
          markdownPath <- validateMarkdownPath markdown
          presentationPath <- validatePresentationPath presentation
          return . liftM2 (,) markdownPath $ presentationPath
      _ -> do
          putStrLn "Usage: landrover [markdown] [presentation]"
          return Nothing

-- | Validate the path to the markdown file by assuring it exists
validateMarkdownPath :: FilePath -> IO (Maybe FilePath)
validateMarkdownPath path = do
    exists <- doesFileExist path
    if exists
       then return . Just $ path
       else do
           putStrLn $ "Can't find markdown at " ++ path
           return Nothing

-- | Validate the path to the presentation folder, checking wether it and
--   the template file exist
validatePresentationPath :: FilePath -> IO (Maybe FilePath)
validatePresentationPath path = do
    -- Make sure the path ends with a '/'
    let fixedPath = if "/" `isSuffixOf` path
        then path
        else path ++ "/"

    presentationExists <- doesDirectoryExist fixedPath
    if presentationExists
       then do
           templateExists <- doesFileExist $ fixedPath ++ "template.html"
           if templateExists
              then return . Just $ fixedPath
              else do
                  putStrLn $
                    "Can't find template at " ++ fixedPath ++ "template.html"
                  return Nothing
       else do
           putStrLn $ "Can't find presentation in " ++ path
           return Nothing

-- | Try converting the markdown to html
convertMarkdown :: FilePath -> IO (Maybe String)
convertMarkdown path = do
    markdown <- readFile path
    case readMarkdown def markdown of
      Right pandoc -> return . Just . writeHtmlString def $ pandoc
      Left err -> do
          putStrLn $ "Error reading markdown: " ++ show err
          return Nothing

-- | Convert the html code into reveal.js slides
slidifyHtml :: String -> T.Text
slidifyHtml html = foldr1 T.append sections
    where sections = map sorround . T.splitOn hr $ packed
          hr = T.pack "<hr />"
          packed = T.pack html
          sorround text = T.pack "<section>" `T.append` text `T.append`
                          T.pack "</section>"

-- | Insert the slides into the template and save it as index.html
insertIntoTemplate :: FilePath -> T.Text -> IO ()
insertIntoTemplate presentationPath insert = do
    content <- readFile $ presentationPath ++ "template.html"
    let template = T.pack content
        output = T.replace token insert template
    writeFile (presentationPath ++ "index.html") . T.unpack $ output

    where token = T.pack "<!-- SLIDES -->"

main = do
    paths <- getPaths
    case paths of
      Just (markdown, presentation) -> do
          html <- convertMarkdown markdown
          case html of
            Just converted ->
              let slides = slidifyHtml converted
              in insertIntoTemplate presentation slides
            _ -> return ()
      _ -> return ()
