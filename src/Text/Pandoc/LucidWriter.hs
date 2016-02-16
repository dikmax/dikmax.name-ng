{-# LANGUAGE OverloadedStrings, TemplateHaskell, ScopedTypeVariables #-}

module Text.Pandoc.LucidWriter (writeLucid, writeLucidString) where

import           Control.Monad.State
import           Control.Lens
import           Data.Data
import           Data.Default
import qualified Data.Text           as T
import           Data.Text.Lazy      hiding (map)
import           Lucid
import           Lucid.Base
import           Prelude             hiding (drop, head, init, length, null,
                                      tail, take, takeWhile, unwords)
import qualified Prelude             as P
import           Text.Pandoc


data LucidWriterOptions = LucidWriterOptions
    { _idPrefix     :: Text
    , _siteDomain   :: Text
    , _debugOutput  :: Bool
    , _renderForRSS :: Bool
    }

instance Default LucidWriterOptions where
    def = LucidWriterOptions
        { _idPrefix     = ""
        , _siteDomain   = ""
        , _debugOutput  = False
        , _renderForRSS = False
        }

makeLenses ''LucidWriterOptions

data WriterStateData = WriterStateData
    { _notesList     :: [Html ()]
    , _writerOptions :: LucidWriterOptions
    , _countBlocks   :: Int
    }

instance Default WriterStateData where
    def = WriterStateData
        { _notesList     = []
        , _writerOptions = def
        , _countBlocks   = 0
        }

makeLenses ''WriterStateData

type WriterState = State WriterStateData

writeLucidString :: LucidWriterOptions -> Pandoc -> String
writeLucidString options pandoc = unpack $ renderText $ writeLucid options pandoc

writeLucid :: LucidWriterOptions -> Pandoc -> Html ()
writeLucid options pandoc
    | options ^. debugOutput =
        pre_ [] (toHtml $ writeNative def pandoc)
    | otherwise = evalState (writeLucid' pandoc)
                    (def & writerOptions %~ idPrefix %~ (\p -> if p == "" then "new" else p))

writeLucid' :: Pandoc -> WriterState (Html ())
writeLucid' (Pandoc _ blocks) = do
    mainBlocks <- concatBlocks blocks
    footer <- getFooter
    return $ do
        mainBlocks
        footer

concatBlocks :: [Block] -> WriterState (Html ())
concatBlocks blocks = do
    result <- mapM writeBlock blocks
    return $ mconcat result

writeBlock :: Block -> WriterState (Html ())
writeBlock (Plain inline) =
    concatInlines inline


writeBlock (Para inline) = withCountBlocksIncrement $ \c -> do
    inlines <- concatInlines inline
    return $ case inline of
        [Image{}] -> inlines
        _ -> p_ [class_ "main__centered post__block post__block_para", id_ $ toStrict $ pack $ "p-" ++ show c] inlines

writeBlock (CodeBlock (identifier, classes, others) code) =
    return $ pre_ (class_ "main__centered post__block post__block_code" : mapAttrs) $ code_ mapAttrs $ toHtml code
  where
    mapAttrs = writeAttr (identifier, "sourceCode" : classes, others)

writeBlock (RawBlock "html" str) =
    return $ toHtmlRaw str
writeBlock (RawBlock _ _) = return mempty

writeBlock (BlockQuote blocks) = do
    items <- concatBlocks blocks
    return $ blockquote_ [class_ "main__centered post__block post__block_blockquote"] items

writeBlock (OrderedList (startNum, numStyle, _) listItems) = withCountBlocksIncrement $ \c -> do
    items <- mapM processListItems listItems
    return $ ol_ (class_ "main__centered post__block post__block_ordered-list" : attributes c) $ mconcat items
    where
        char :: T.Text
        char = case numStyle of
            Decimal    -> "1"
            LowerAlpha -> "a"
            UpperAlpha -> "A"
            LowerRoman -> "i"
            UpperRoman -> "I"
            _          -> "1"
        attributes :: Int -> [Attribute]
        attributes s = [type_ char, id_ $ toStrict $ pack $ "p-" ++ show (s + 1)] ++
            [ start_ $ toStrict $ pack $ show startNum | startNum /= 1 ]

writeBlock (BulletList listItems) = withCountBlocksIncrement $ \c -> do
    items <- mapM processListItems listItems
    return $ ul_
        [ class_ "main__centered post__block post__block_unordered-list"
        , id_ $ toStrict $ pack $ "p-" ++ show c
        ] $ mconcat items

writeBlock (Header 1 attr inline) = withCountBlocksIncrement $ \c -> do
    inlines <- concatInlines inline
    return $ h1_
        ( class_ "main__centered post__block post__block_header-1"
        : id_ (toStrict $ pack $ "p-" ++ show c)
        : writeAttr attr) inlines
writeBlock (Header 2 attr inline) = withCountBlocksIncrement $ \c -> do
    inlines <- concatInlines inline
    return $ h2_
        ( class_ "main__centered post__block post__block_header-2"
        : id_ (toStrict $ pack $ "p-" ++ show c)
        : writeAttr attr) inlines
writeBlock (Header 3 attr inline) = withCountBlocksIncrement $ \c -> do
    inlines <- concatInlines inline
    return $ h3_
        ( class_ "main__centered post__block post__block_header-3"
        : id_ (toStrict $ pack $ "p-" ++ show c)
        : writeAttr attr) inlines
writeBlock (Header 4 attr inline) = withCountBlocksIncrement $ \c -> do
    inlines <- concatInlines inline
    return $ h4_
        ( class_ "main__centered post__block post__block_header-4"
        : id_ (toStrict $ pack $ "p-" ++ show c)
        : writeAttr attr) inlines
writeBlock (Header 5 attr inline) = withCountBlocksIncrement $ \c -> do
    inlines <- concatInlines inline
    return $ h5_
        ( class_ "main__centered post__block post__block_header-5"
        : id_ (toStrict $ pack $ "p-" ++ show c)
        : writeAttr attr) inlines
writeBlock (Header _ attr inline) = withCountBlocksIncrement $ \c -> do
    inlines <- concatInlines inline
    return $ h6_
        ( class_ "main__centered post__block post__block_header-6"
        : id_ (toStrict $ pack $ "p-" ++ show c)
        : writeAttr attr) inlines

writeBlock HorizontalRule = return $ hr_ [class_ "main__centered post__block post__block_rule"]

writeBlock (Div attr blocks) = do
    items <- concatBlocks blocks
    return $ div_ (class_ "main__centered post__block post__block_div" : writeAttr attr) items

writeBlock Null = return mempty

writeBlock b = return $ toHtml (show (toConstr b) ++ " not implemented")

processListItems :: [Block] -> WriterState (Html ())
processListItems blocks = do
    items <- concatBlocks blocks
    return $ li_ items

writeAttr :: Attr -> [Attribute]
writeAttr (identifier, classes, others) =
    [id_ $ toStrict $ pack identifier | identifier /= ""] ++
    [class_ classesString | classesString /= ""] ++
    map (\(k, v) -> makeAttribute (toStrict $ pack k) (toStrict $ pack v)) others
    where
        classesString = toStrict $ unwords $ map pack classes

concatInlines :: [Inline] -> WriterState (Html ())
concatInlines inlines = do
    result <- mapM writeInline inlines
    return $ mconcat result

writeInline :: Inline -> WriterState (Html ())
writeInline (Str string) = return $ toHtml string

writeInline (Emph inline) = do
    inlines <- concatInlines inline
    return $ em_ inlines

writeInline (Strong inline) = do
    inlines <- concatInlines inline
    return $ strong_ inlines

writeInline (Strikeout inline) = do
    inlines <- concatInlines inline
    return $ term "s" inlines

writeInline (Superscript inline) = do
    inlines <- concatInlines inline
    return $ sup_ inlines

writeInline (Subscript inline) = do
    inlines <- concatInlines inline
    return $ sub_ inlines

writeInline (SmallCaps inline) = do
    inlines <- concatInlines inline
    return $ span_ [style_ "font-variant: small-caps;"] inlines

writeInline (Quoted SingleQuote inline) = do
    inlines <- concatInlines inline
    return $ do {"'"; inlines; "'"}

writeInline (Quoted DoubleQuote inline) = do
    inlines <- concatInlines inline
    return $ do {"«"; inlines; "»"}

writeInline (Code attr code) = return $ code_ (writeAttr attr) (toHtml code)

writeInline Space = return " "

writeInline LineBreak = return $ br_ []

writeInline SoftBreak = return " "

writeInline (Math InlineMath str) = return $ span_ [class_ "math"] $
    toHtml $ "\\(" `append` pack str `append` "\\)"

writeInline (Math DisplayMath str) = return $ span_ [class_ "math"] $
    toHtml $ "\\[" `append` pack str `append` "\\]"

writeInline (RawInline "html" str) = return $ toHtmlRaw str
writeInline (RawInline _ _) = return mempty

writeInline (Link attr inline target) = do
    inlines <- concatInlines inline
    options <- use writerOptions
    return $ a_
        (writeAttr attr ++
            [ href_ $ toStrict $ linkToAbsolute (options ^. renderForRSS)
                (pack $ fst target) (options ^. siteDomain)
            , title_ $ toStrict $ pack $ snd target
            ]
        ) inlines

writeInline (Image attr inline target) = do
    inlines <- concatInlines inline
    options <- use writerOptions

    return $ if "http://www.youtube.com/watch?v=" `isPrefixOf` pack (fst target) ||
            "https://www.youtube.com/watch?v=" `isPrefixOf` pack (fst target)
        -- Youtube video
        then div_ (class_ "main__full-width post__block post__figure post__figure_youtube" : writeAttr attr) $
            div_ [class_ "embed-responsive embed-responsive-16by9"] $ do
                iframe_
                    [ src_ $ toStrict $ "https://www.youtube.com/embed/" `append`
                        videoId (pack $ fst target) `append` "?wmode=transparent"
                    , makeAttribute "allowfullscreen" "allowfullscreen"
                    , class_ "img-polaroid embed-responsive-item"
                    ] mempty
                unless (P.null inline) $
                    p_ [class_ "figure-description"] inlines
        else figure_
            ([ id_ $ toStrict (extractId $ pack $ fst target)
             , class_ "main__full-width post__block post__figure"] ++ writeAttr attr) $
            div_ [class_ "post__figure-outer"] $
                div_ [class_ "post__figure-inner"] $ do
                    img_ [ class_ "post__figure-img"
                        , src_ $ toStrict $ linkToAbsolute (options ^. renderForRSS) (pack $ fst target)
                            (options ^. siteDomain)
                        , alt_ $ toStrict $ pack $ fixImageTitle $ snd target
                        ]
                    unless (P.null inline) $
                        p_ [class_ "post__figure-description"] inlines
    where
        videoId url = takeWhile (/= '&') $ replace "http://www.youtube.com/watch?v=" "" $
            replace "https://www.youtube.com/watch?v=" "" url
        -- http://dikmax.name/images/travel/2014-06-eurotrip/rome-santa-maria-maggiore-1.jpg -> rome-santa-maria-maggiore-1
        extractId = init . fst . breakOnEnd "." . snd . breakOnEnd "/"

        fixImageTitle :: String -> String
        fixImageTitle title
            | P.take 4 title == "fig:" = P.drop 4 title
            | otherwise = title


writeInline (Note block) = do   -- TODO there should be a link to footer
    blocks <- concatBlocks block
    n <- use notesList
    options <- use writerOptions
    notesList %= (++ [blocks])
    let noteId = P.length n + 1
    return $ sup_
        [ id_ $ toStrict $ "note-" `append` (options ^. idPrefix) `append` pack (show noteId)
        , class_ "note-link"
        ] $ toHtml $ show noteId

writeInline (Span attr inline) = do
    inlines <- concatInlines inline
    return $ span_ (writeAttr attr) inlines

writeInline i = return $ toHtml (show (toConstr i) ++ " not implemented")

getFooter :: WriterState (Html ())
getFooter = do
    n <- use notesList
    options <- use writerOptions
    return $
        if not $ P.null n
            then div_ [class_ "main__centered post__footnotes"] $ do
                hr_ []
                ol_ $ transformNotes n 1 ("note-" `append` (options ^. idPrefix))
            else mempty
            {- | not $ null $ notesList writerState -}
    where
        transformNotes :: [Html ()] -> Int -> Text -> Html ()
        transformNotes (n:ns) i prefix = do
            li_ [data_ "for" (toStrict $ prefix `append` pack (show i))] n
            transformNotes ns (i+1) prefix
        transformNotes [] _ _ = mempty

linkToAbsolute :: Bool -> Text -> Text -> Text
linkToAbsolute False link _ = link
linkToAbsolute True link "" = link
linkToAbsolute True link domain
  | (length link >= 2) && (head link == '/') && head (tail link) /= '/' = domain `append` link
  | otherwise = link

withCountBlocksIncrement :: (Int -> WriterState a) -> WriterState a
withCountBlocksIncrement process = do
    countBlocks += 1
    cb <- use countBlocks
    process cb
