--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import           System.FilePath (takeDirectory,takeBaseName,(</>))
import           Data.List (isSuffixOf,intersperse)
import           Text.Regex (mkRegex,splitRegex)
import           Data.Char (toLower,isAlphaNum)
import           Control.Monad (mfilter)
import qualified Data.Map.Lazy as M

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.rst", "contact.markdown"]) $ do
        route stripExtensionRoute
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= tidyUrls

    match "posts/*" $ do
        route tidyRoute
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= tidyUrls

    match "projects/*" $ do
        compile $ pandocCompiler

    create ["archive.html"] $ do
        route stripExtensionRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= tidyUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            projects <- loadAll "projects/*"

            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    listField "projects" projectCtx (return projects) `mappend`
                    constField "title" "Home"                `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= tidyUrls

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

projectCtx :: Context String
projectCtx =
    imgSrcContext `mappend`
    defaultContext

imgSrcContext :: Context a
imgSrcContext = field "imgSrc" $ \item -> do
    metadata <- getMetadata (itemIdentifier item)
    return $ maybe "/images/projects/no-image.png" titleToImgSrc (mfilter (not . null) $ M.lookup "title" metadata)

titleToImgSrc :: String -> String
titleToImgSrc = (\src -> "/images/projects/" ++ src ++ ".png") . concat . intersperse "-" . map (filter isAlphaNum) . words . map toLower

--------------------------------------------------------------------------------

stripExtensionRoute :: Routes
stripExtensionRoute = customRoute createIndexRoute
  where
    createIndexRoute ident = takeDirectory p ++ "/" ++ takeBaseName p ++ "/index.html"
                            where p = toFilePath ident

cleanIndex :: String -> String
cleanIndex url
    | idx `isSuffixOf` url = take (length url - length idx) url
    | otherwise            = url
  where idx = "index.html"

cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withInternalUrls cleanIndex)

cleanIndexHtmls :: Item String -> Compiler (Item String)
cleanIndexHtmls = return . fmap (replaceAll "/index.html" $ const "/")

stripHtmlExtensions :: Item String -> Compiler (Item String)
stripHtmlExtensions = return . fmap (withInternalUrls $ replaceAll ".html" (const "/"))

stripDateRoute :: Routes
stripDateRoute = customRoute $ stripDate . toFilePath

stripDate :: String -> String
stripDate = concat . splitRegex (mkRegex "([0-9]{4})\\-([0-9]{2})\\-([0-9]{2})\\-")

stripDateUrls :: Item String -> Compiler (Item String)
stripDateUrls = return . fmap (withInternalUrls stripDate)

tidyRoute :: Routes
tidyRoute =
    stripExtensionRoute `composeRoutes`
    stripDateRoute

tidyUrls :: Item String -> Compiler (Item String)
tidyUrls item = return item
    >>= relativizeUrls
    >>= cleanIndexUrls
    >>= stripHtmlExtensions
    >>= stripDateUrls

withInternalUrls :: (String -> String) -> String -> String
withInternalUrls fn = withUrls (\str -> if isExternal str then str else fn str)

--------------------------------------------------------------------------------

