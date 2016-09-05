--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import qualified GHC.IO.Encoding as E

import           Data.Monoid (mappend)
import           Hakyll
import           System.FilePath (takeDirectory,takeBaseName,(</>))
import           Data.List (isSuffixOf,intersperse)
import           Text.Regex (mkRegex,splitRegex)
import           Data.Char (toLower,isAlphaNum)
import           Control.Monad (mfilter)

--------------------------------------------------------------------------------
main :: IO ()
main = do
    E.setLocaleEncoding E.utf8
    hakyll $ do
        tags <- buildTags ("posts/*" .||. "projects/*") (fromCapture "tags/*/index.html")

        match "images/**" $ do
            route   idRoute
            compile copyFileCompiler

        match "css/*" $ do
            route   idRoute
            compile compressCssCompiler

        match "posts/*" $ do
            route tidyRoute
            compile $ pandocCompiler
                >>= saveSnapshot "content"
                >>= loadAndApplyTemplate "templates/post.html"    (postCtx tags)
                >>= saveSnapshot "contentAfterPostTemplate"
                >>= loadAndApplyTemplate "templates/default.html" (postCtx tags)
                >>= tidyUrls

        match "about.md" (compile $ pandocCompiler >>= tidyUrls)

        match "projects/*" $ do
            compile pandocCompiler

        create ["archive.html"] $ do
            route stripExtensionRoute
            compile $ do
                posts <- recentFirst =<< loadAll "posts/*"
                let archiveCtx =
                        listField "posts" (postCtx tags) (return posts) `mappend`
                        constField "title" "Archives"            `mappend`
                        constField "heading" "Archives"            `mappend`
                        baseContext tags

                makeItem ""
                    >>= loadAndApplyTemplate "templates/post-list.html" archiveCtx
                    >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                    >>= tidyUrls

        create ["rss.xml"] $ do
            route idRoute
            compile $ do
                let feedCtx =
                        field "url" (fmap (maybe "" $ (:) '/' . cleanIndex) . getRoute . itemIdentifier) `mappend`
                        postCtx tags `mappend`
                        bodyField "description"
                posts <- fmap (map cleanupRssItem . take 10) . recentFirst =<< loadAllSnapshots "posts/*" "contentAfterPostTemplate"
                renderRss myFeedConfiguration feedCtx posts

        create ["portfolio.html"] $ do
            route stripExtensionRoute
            compile $ do
                projects <- recentFirst =<< loadAll "projects/*"
                let projCtx =
                        listField "projects" (projectCtx tags) (return projects) `mappend`
                        constField "title" "Portfolio" `mappend`
                        constField "heading" "Portfolio" `mappend`
                        baseContext tags

                makeItem ""
                    >>= loadAndApplyTemplate "templates/portfolio.html" projCtx
                    >>= loadAndApplyTemplate "templates/default.html" projCtx
                    >>= tidyUrls

        tagsRules tags $ \tag pattern -> do
            route idRoute
            compile $ do
                let takeOnly p = fmap (filter $ matches p . itemIdentifier)

                posts <- recentFirst =<< takeOnly "posts/*" (loadAll pattern)
                projects <- recentFirst =<< takeOnly "projects/*" (loadAll pattern)

                let tagCtx =
                        constField "title" ("Tag: " ++ tag) `mappend`
                        constField "tag" tag `mappend`
                        listField "posts" (postCtx tags) (return posts) `mappend`
                        listField "projects" (projectCtx tags) (return projects) `mappend`
                        baseContext tags

                makeItem ""
                    >>= loadAndApplyTemplate "templates/tag.html" (tagCtx)
                    >>= loadAndApplyTemplate "templates/default.html" (tagCtx)
                    >>= tidyUrls


        match "index.html" $ do
            route idRoute
            compile $ do
                posts <- recentFirst =<< loadAll "posts/*"
                projects <- recentFirst =<< loadAll "projects/*"
                about <- loadBody "about.md" :: Compiler String

                let indexCtx =
                        listField "posts" (postCtx tags) (return posts) `mappend`
                        listField "projects" (projectCtx tags) (return projects) `mappend`
                        constField "title" "Home" `mappend`
                        constField "banner" "/images/banner.png"                `mappend`
                        constField "about" about `mappend`
                        baseContext tags

                getResourceBody
                    >>= applyAsTemplate indexCtx
                    >>= loadAndApplyTemplate "templates/default.html" indexCtx
                    >>= tidyUrls

        match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------

postCtx :: Tags -> Context String
postCtx tags =
    teaserField "teaser" "content" `mappend`
    tagsField "tags" tags `mappend`
    dateField "date" "%B %e, %Y" `mappend`
    baseContext tags `mappend`
    constField "author" "Rob"


projectCtx :: Tags -> Context String
projectCtx tags =
    tagsField "tags" tags `mappend`
    imgSrcContext `mappend`
    baseContext tags

imgSrcContext :: Context a
imgSrcContext = field "imgSrc" $ \item -> do
    metadata <- getMetadata (itemIdentifier item)
    return $ maybe "/images/projects/no-image.png" titleToImgSrc (mfilter (not . null) $ lookupString "title" metadata)

titleToImgSrc :: String -> String
titleToImgSrc = (\src -> "/images/projects/" ++ src ++ ".png") . concat . intersperse "-" . map (filter isAlphaNum) . words . map toLower

baseContext :: Tags -> Context String
baseContext tags =
    tagCloudField "tagCloud" 75 300 tags `mappend`
    defaultContext

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

myFeedConfiguration :: FeedConfiguration
myFeedConfiguration = FeedConfiguration
    { feedTitle       = "Blog - Rob Whitaker"
    , feedDescription = "A blog about cool things tech and writing!"
    , feedAuthorName  = "Rob Whitaker"
    , feedAuthorEmail = "robjameswhitaker@gmail.com"
    , feedRoot        = "https://robwhitaker.github.io"
    }

cleanupRssItem :: Item String -> Item String
cleanupRssItem =
    fmap (withInternalUrls $ replaceAll ".html" (const "/")) .
    fmap (withInternalUrls cleanIndex)


