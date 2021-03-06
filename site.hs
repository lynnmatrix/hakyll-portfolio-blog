--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import qualified GHC.IO.Encoding as E

import           Data.Monoid (mappend,mconcat)
import           Hakyll
import           System.FilePath (takeDirectory,takeBaseName,(</>))
import           Data.List (isSuffixOf,intersperse)
import           Text.Regex (mkRegex,splitRegex)
import           Data.Char (toLower,isAlphaNum)
import           Control.Monad (mfilter)
import           Control.Applicative (empty)
import           Resume

import           Text.Blaze.Html                 (toHtml, toValue, (!))
import           Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as A

--------------------------------------------------------------------------------
main :: IO ()
main = do
    E.setLocaleEncoding E.utf8
    resume <- getResume
    makePDF
    hakyll $ do
        tags <- buildTags ("posts/*" .||. "projects/*") (fromCapture "tags/*/index.html")

        match "images/**" $ do
            route   idRoute
            compile copyFileCompiler

        match "css/*" $ do
            route   idRoute
            compile $ getResourceString
                >>= withItemBody (unixFilter "postcss" ["--use autoprefixer"])
                >>= withItemBody (return . compressCss)

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
                    >>= loadAndApplyTemplate "templates/post-teaser-list.html" archiveCtx
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
                posts <- fmap (take 3) . recentFirst =<< loadAll "posts/*"
                projects <- recentFirst =<< loadAll "projects/*"
                about <- loadBody "about.md" :: Compiler String

                let indexCtx = mconcat
                        [ listField "posts" (postCtx tags) (return posts)
                        , listField "projects" (projectCtx tags) (return projects)
                        , constField "title" "Home"
                        , constField "heading" "Robert J. Whitaker"
                        , constField "about" about
                        , resumeCtx resume
                        , baseContext tags
                        ]

                getResourceBody
                    >>= applyAsTemplate indexCtx
                    >>= loadAndApplyTemplate "templates/default.html" indexCtx
                    >>= tidyUrls

        match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------

postCtx :: Tags -> Context String
postCtx tags =
    field "url" (fmap (maybe "" $ (:) '/' . cleanIndex) . getRoute . itemIdentifier) `mappend`
    authorUrlIsExternal `mappend`
    teaserField "teaser" "content" `mappend`
    tagsFieldNonEmpty "tags" tags `mappend`
    dateField "date" "%B %e, %Y" `mappend`
    baseContext tags `mappend`
    constField "twitterVia" "RobertJWhitaker" `mappend`
    constField "author" "Rob" `mappend`
    constField "authorUrl" "/"

projectCtx :: Tags -> Context String
projectCtx tags =
    tagsFieldNonEmpty "tags" tags `mappend`
    fieldFromMetadata "imgSrc" "title" (maybe "/images/projects/no-image.png" projectTitleToImgSrc) `mappend`
    baseContext tags

projectTitleToImgSrc :: String -> String
projectTitleToImgSrc = (\src -> "/images/projects/" ++ src ++ ".png") . concat . intersperse "-" . map (filter isAlphaNum) . words . map toLower

baseContext :: Tags -> Context String
baseContext tags =
    tagCloudField "tagCloud" 75 300 tags `mappend`
    defaultContext

fieldFromMetadata :: String -> String -> (Maybe String -> String) -> Context a
fieldFromMetadata label metaField fn =
    field label $ \item -> do
        metadata <- getMetadata (itemIdentifier item)
        return $ fn (mfilter (not . null) $ lookupString metaField metadata)

authorUrlIsExternal :: Context a
authorUrlIsExternal =
    field "authorUrlIsExternal" $ \item -> do
        metadata <- getMetadata (itemIdentifier item)
        case lookupString "authorUrl" metadata of
            Just authorUrl ->
                if isExternal authorUrl then
                    return ""
                else
                    empty
            Nothing ->
                empty

getTagsNonEmpty :: Identifier -> Compiler [String]
getTagsNonEmpty identifier = do
    tags <- getTags identifier
    if null tags then
        empty
    else
        return tags

tagsFieldNonEmpty :: String -> Tags -> Context a
tagsFieldNonEmpty =
    tagsFieldWith getTagsNonEmpty simpleRenderLink (mconcat . intersperse ", ")
  where
    simpleRenderLink :: String -> (Maybe FilePath) -> Maybe H.Html
    simpleRenderLink _   Nothing         = Nothing
    simpleRenderLink tag (Just filePath) =
      Just $ H.a ! A.href (toValue $ toUrl filePath) $ toHtml tag


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


