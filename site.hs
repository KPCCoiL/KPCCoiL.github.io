--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Data.List
import           Data.Maybe
import qualified Data.Map.Strict as Map
import           Control.Monad
import           Hakyll
import           Text.Pandoc.Options

myPandocCompiler :: Compiler (Item String)
myPandocCompiler = pandocCompilerWith readerOpt writerOpt
  where rexts = [ Ext_emoji
                , Ext_tex_math_double_backslash
                , Ext_latex_macros
                , Ext_east_asian_line_breaks
                ]
        readerOpt = defaultHakyllReaderOptions
                      { readerExtensions = foldr enableExtension pandocExtensions rexts }
        writerOpt = defaultHakyllWriterOptions
                      { writerHTMLMathMethod = KaTeX "" }
--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    tags <- buildTags "posts/*" (fromCapture "tags/*.html")
    let pageCtx = postCtx `mappend` tagCloudField "tagCloud" 80 300 tags

    match (fromList ["about.rst", "links.mkd"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" pageCtx
            >>= relativizeUrls

    tagsRules tags $ \tag pattern -> do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll pattern
        let tagCtx =
              listField "posts" postCtx (return posts) `mappend`
              constField "title" ("Tag : " ++ tag) `mappend`
              pageCtx

        makeItem ""
            >>= loadAndApplyTemplate "templates/post-list.html" tagCtx
            >>= loadAndApplyTemplate "templates/default.html" tagCtx
            >>= relativizeUrls


    match "posts/*" $ do
        route $ setExtension "html"

        compile $ do
          let ctx = tagListField `mappend` pageCtx
          myPandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    ctx
            >>= saveSnapshot "postContent"
            >>= loadAndApplyTemplate "templates/default.html" ctx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    pageCtx

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    create ["tags.html"] $ do
      route idRoute
      compile $ do
        let ctx = taggedItemsField tags `mappend`
                  constField "title" "Tags" `mappend`
                  pageCtx

        makeItem ""
            >>= loadAndApplyTemplate "templates/tags.html" ctx
            >>= loadAndApplyTemplate "templates/default.html" ctx
            >>= relativizeUrls

    create ["atom.xml"] $ do
      route idRoute
      compile $ do
        let feedConf = FeedConfiguration
                        { feedTitle = "CoiL's Memorand[u/o]m"
                        , feedDescription = "CoiL's blog"
                        , feedAuthorName = "CoiL"
                        , feedAuthorEmail = "copernicium@me.com"
                        , feedRoot = "https://KPCCoiL.github.io"
                        }
        posts <- recentFirst =<< loadAllSnapshots "posts/*" "postContent"
        renderAtom feedConf pageCtx posts

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Home"                `mappend`
                    pageCtx

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

wrapItem :: a -> Item a
wrapItem x = Item 
  { itemIdentifier = fromFilePath ""
  , itemBody = x
  }

tagListField :: Context String
tagListField = listField "tagList" tagField $ map wrapItem <$> (getTags =<< getUnderlying)
  where tagField = bodyField "tag"

taggedItemsField :: Tags -> Context String
taggedItemsField = listField "taggedItems" itemsField . return . map wrapItem . tagsMap
  where itemsField = field "tagName" (return . fst . itemBody) `mappend`
                     listFieldWith "posts" postCtx getPosts
        getPosts (Item _ (tag, ids)) = loadAll $ fromList ids
