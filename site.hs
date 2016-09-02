--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import           System.FilePath (takeDirectory,takeBaseName,(</>))
import           Data.List (isSuffixOf,intersperse)
import           Text.Regex (mkRegex,splitRegex)
import           Data.Char (toLower,isAlphaNum)
import           Control.Monad (mfilter)

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

    tags <- buildTags ("posts/*" .||. "projects/*") (fromCapture "tags/*/index.html")

    match "posts/*" $ do
        route tidyRoute
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    (postCtx tags)
            >>= loadAndApplyTemplate "templates/default.html" (postCtx tags)
            >>= tidyUrls

    match "projects/*" $ do
        compile pandocCompiler

    create ["archive.html"] $ do
        route stripExtensionRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" (postCtx tags) (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= tidyUrls

    tagsRules tags $ \tag pattern -> do
        route idRoute
        compile $ do
            let takeOnly p = fmap (filter $ matches p . itemIdentifier)

            posts <- recentFirst =<< takeOnly "posts/*" (loadAll pattern)
            projects <- takeOnly "projects/*" (loadAll pattern)

            let tagCtx =
                    constField "title" ("Tag: " ++ tag) `mappend`
                    constField "tag" tag `mappend`
                    listField "posts" (postCtx tags) (return posts) `mappend`
                    listField "projects" (projectCtx tags) (return projects) `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/tag.html" (tagCtx)
                >>= loadAndApplyTemplate "templates/default.html" (tagCtx)
                >>= tidyUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            projects <- loadAll "projects/*"

            let indexCtx =
                    tagCloudField "tagCloud" 75 300 tags `mappend`
                    listField "posts" (postCtx tags) (return posts) `mappend`
                    listField "projects" (projectCtx tags) (return projects) `mappend`
                    constField "title" "Home"                `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= tidyUrls

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------

postCtx :: Tags -> Context String
postCtx tags =
    tagsField "tags" tags `mappend`
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

projectCtx :: Tags -> Context String
projectCtx tags =
    tagsField "tags" tags `mappend`
    imgSrcContext `mappend`
    defaultContext

imgSrcContext :: Context a
imgSrcContext = field "imgSrc" $ \item -> do
    metadata <- getMetadata (itemIdentifier item)
    return $ maybe "/images/projects/no-image.png" titleToImgSrc (mfilter (not . null) $ lookupString "title" metadata)

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
