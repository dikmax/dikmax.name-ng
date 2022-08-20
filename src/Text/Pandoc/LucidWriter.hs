{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, MultiWayIf #-}

module Text.Pandoc.LucidWriter (
    LucidWriterOptions(..),
    RenderType(..),
    idPrefix,
    siteDomain,
    commonData,
    debugOutput,
    renderType,
    showFigureNumbers,
    responsiveFigures,

    writeLucid,
    writeLucidText
) where

import           BasicPrelude
import           Control.Monad.State
import           Control.Lens        hiding (lazy)
import           Data.Data
import           Data.Default
import qualified Data.Bifunctor      as BF
import qualified Data.Text           as T
import           Lucid
import           Lucid.Base
import           Lucid.AMP
import           Lucid.Html5Extra
import           Text.Pandoc
import           Types


data RenderType =
        RenderNormal | RenderRSS | RenderAMP deriving (Eq)

data LucidWriterOptions = LucidWriterOptions
    { _idPrefix          :: Text
    , _siteDomain        :: Text
    , _commonData        :: CommonData
    , _debugOutput       :: Bool
    , _renderType        :: RenderType
    , _showFigureNumbers :: Bool
    , _responsiveFigures :: Bool
    }

instance Default LucidWriterOptions where
    def = LucidWriterOptions
        { _idPrefix          = ""
        , _siteDomain        = ""
        , _commonData        = def
        , _debugOutput       = False
        , _renderType        = RenderNormal
        , _showFigureNumbers = True
        , _responsiveFigures = True
        }

makeLenses ''LucidWriterOptions

data WriterStateData = WriterStateData
    { _notesList     :: [Html ()]
    , _writerOptions :: LucidWriterOptions
    , _countBlocks   :: Int
    , _level         :: Int
    }

instance Default WriterStateData where
    def = WriterStateData
        { _notesList     = []
        , _writerOptions = def
        , _countBlocks   = 0
        , _level         = 0
        }

makeLenses ''WriterStateData

type WriterState = State WriterStateData

blankImage :: Text
blankImage = "data:image/gif;base64,\
    \R0lGODlhAQABAAAAACH5BAEKAAEALAAAAAABAAEAAAICTAEAOw=="


writeLucidText :: LucidWriterOptions -> Pandoc -> LText
writeLucidText options pandoc = renderText $ writeLucid options pandoc

writeLucid :: LucidWriterOptions -> Pandoc -> Html ()
writeLucid options pandoc
    | options ^. debugOutput =
        -- let (Right native) = runPure $ writeNative def pandoc
        pre_ [] (toHtml $
            either (error "writeNative error") id $
            runPure $ writeNative def pandoc)
    | otherwise = evalState (writeLucid' pandoc)
                    (def & writerOptions .~ (options & idPrefix %~ (\p -> if p == "" then "new" else p)))

writeLucid' :: Pandoc -> WriterState (Html ())
writeLucid' (Pandoc _ blocks) = do
    mainBlocks <- concatBlocks blocks
    footer <- getFooter
    return $ do
        mainBlocks
        footer

concatBlocks :: [Block] -> WriterState (Html ())
concatBlocks blocks = do
    level += 1
    result <- mapM writeBlock blocks
    level -= 1
    return $ mconcat result


writeBlock :: Block -> WriterState (Html ())
writeBlock (Plain inline) =
    concatInlines inline

writeBlock (Para inline) = withCountBlocksIncrement $ \c -> do
    inlines <- concatInlines inline
    cl <- getMainBlockClass
    return $ case inline of
        [Image{}] -> inlines
        _ -> p_ [class_ $ cl ++ "post__block_para", id_ $ "p-" ++ tshow c] inlines

writeBlock (CodeBlock (identifier, classes, others) code) = do
    cl <- getMainBlockClass
    let mapAttrs =
            writeAttr (identifier,
                cl : "post__block_code sourceCode" : classes, others) ""
    let mapAttrs2 =
            writeAttr (identifier,
                "sourceCode" : classes, others) ""
    return $ pre_ mapAttrs $ code_ mapAttrs2 $ toHtml code


writeBlock (RawBlock "html" str) =
    return $ toHtmlRaw str
writeBlock (RawBlock _ _) = return mempty

writeBlock (BlockQuote blocks) = do
    cl <- getMainBlockClass
    items <- concatBlocks blocks
    return $ blockquote_ [class_ $ cl ++ "post__block_blockquote"] items

writeBlock (OrderedList (startNum, numStyle, _) listItems) = withCountBlocksIncrement $ \c -> do
    cl <- getMainBlockClass
    items <- mapM processListItems listItems
    return $ ol_ (class_ (cl ++ "post__block_ordered-list") : attributes c) $ mconcat items
    where
        char :: Text
        char = case numStyle of
            Decimal    -> "1"
            LowerAlpha -> "a"
            UpperAlpha -> "A"
            LowerRoman -> "i"
            UpperRoman -> "I"
            _          -> "1"
        attributes :: Int -> [Attribute]
        attributes s = [type_ char, id_ $ "p-" ++ tshow (s + 1)] ++
            [ start_ $ tshow startNum | startNum /= 1 ]

writeBlock (BulletList listItems) = withCountBlocksIncrement $ \c -> do
    cl <- getMainBlockClass
    items <- mapM processListItems listItems
    return $ ul_
        [ class_ $ cl ++ "post__block_unordered-list"
        , id_ $ "p-" ++ tshow c
        ] $ mconcat items

writeBlock (Header 1 attr inline) = withCountBlocksIncrement $ \c -> do
    cl <- getMainBlockClass
    inlines <- concatInlines inline
    return $ h1_
        ( class_ (cl ++ "post__block_header-1")
        : writeAttr attr ("p-" ++ tshow c)) inlines
writeBlock (Header 2 attr inline) = withCountBlocksIncrement $ \c -> do
    cl <- getMainBlockClass
    inlines <- concatInlines inline
    return $ h2_
        ( class_ (cl ++ "post__block_header-2")
        : writeAttr attr ("p-" ++ tshow c)) inlines
writeBlock (Header 3 attr inline) = withCountBlocksIncrement $ \c -> do
    cl <- getMainBlockClass
    inlines <- concatInlines inline
    return $ h3_
        ( class_ (cl ++ "post__block_header-3")
        : writeAttr attr ("p-" ++ tshow c)) inlines
writeBlock (Header 4 attr inline) = withCountBlocksIncrement $ \c -> do
    cl <- getMainBlockClass
    inlines <- concatInlines inline
    return $ h4_
        ( class_ (cl ++ "post__block_header-4")
        : writeAttr attr ("p-" ++ tshow c)) inlines
writeBlock (Header 5 attr inline) = withCountBlocksIncrement $ \c -> do
    cl <- getMainBlockClass
    inlines <- concatInlines inline
    return $ h5_
        ( class_ (cl ++ "post__block_header-5")
        : writeAttr attr ("p-" ++ tshow c)) inlines
writeBlock (Header _ attr inline) = withCountBlocksIncrement $ \c -> do
    cl <- getMainBlockClass
    inlines <- concatInlines inline
    return $ h6_
        ( class_ (cl ++ "post__block_header-6")
        : writeAttr attr ("p-" ++ tshow c)) inlines

writeBlock HorizontalRule = do
    cl <- getMainBlockClass
    return $ hr_ [class_ $ cl ++ "post__block_rule"]

writeBlock (Div (identifier, classes, others) blocks) = do
    cl <- getMainBlockClass
    items <- concatBlocks blocks
    let mapAttrs =
            writeAttr (identifier,
                if null classes
                    then [cl, "post__block_div"]
                    else classes,
                others) ""

    return $ div_ mapAttrs items

writeBlock Null = return mempty

writeBlock b = return $ toHtml (show (toConstr b) ++ " not implemented")

processListItems :: [Block] -> WriterState (Html ())
processListItems blocks = do
    items <- concatBlocks blocks
    return $ li_ items

writeAttr :: Attr -> Text -> [Attribute]
writeAttr (identifier, classes, others) defaultId =
    [id_ identifier | identifier /= ""] ++
    [id_ defaultId | identifier == "" && defaultId /= ""] ++
    [class_ classesString | classesString /= ""] ++
    map (uncurry makeAttribute) others
    where
        classesString = T.unwords classes

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

writeInline (Code attr code) = return $ code_ (writeAttr attr "") (toHtml code)

writeInline Space = return " "

writeInline LineBreak = return $ br_ []

writeInline SoftBreak = return " "

writeInline (Math InlineMath str) = return $ span_ [class_ "math"] $
    toHtml $ "\\(" ++ str ++ "\\)"

writeInline (Math DisplayMath str) = return $ span_ [class_ "math"] $
    toHtml $ "\\[" ++ str ++ "\\]"

writeInline (RawInline "html" str) = return $ toHtmlRaw str
writeInline (RawInline _ _) = return mempty

writeInline (Link attr inline target) = do
    inlines <- concatInlines inline
    options <- use writerOptions
    return $ a_
        (writeAttr attr "" ++
            [ href_ $ linkToAbsolute (options ^. renderType)
                (fst target) (options ^. siteDomain)
            , title_ $ snd target
            ]
        ) inlines

writeInline (Image attr inline target) =
    if | "http://www.youtube.com/watch?v=" `T.isPrefixOf` fst target ||
            "https://www.youtube.com/watch?v=" `T.isPrefixOf` fst target ->
                -- Youtube video
                writeYoutube attr inline target
       | "iframe:" `T.isPrefixOf` fst target ->
                writeIframe attr inline (BF.first (T.drop 7) target)
       | otherwise ->
                -- Just image
                writeImage attr inline target

writeInline (Note block) = do   -- TODO there should be a link to footer
    blocks <- concatBlocks block
    n <- use notesList
    options <- use writerOptions
    notesList %= (++ [blocks])
    let noteId = length n + 1
    return $ sup_
        [ id_ $ "note-" ++ (options ^. idPrefix) ++ tshow noteId
        , class_ "note-link"
        ] $ toHtml $ tshow noteId

writeInline (Span attr inline) = do
    inlines <- concatInlines inline
    return $ span_ (writeAttr attr "") inlines

writeInline i = return $ toHtml (show (toConstr i) ++ " not implemented")

writeYoutube :: Attr -> [Inline] -> Target -> WriterState (Html ())
writeYoutube attr inline target = do
    inlines <- concatInlines inline
    options <- use writerOptions

    return $
        case options ^. renderType of
            RenderAMP ->
                div_ (class_ " main__full-width post__block post__block_with-embed" : writeAttr attr "") $
                    div_ [class_ "post__figure-outer"] $
                        div_ [class_ "post__figure-inner post__embed"] $ do
                            ampYoutube_
                                [ data_ "videoid" (videoId $ fst target)
                                , term "layout" "responsive"
                                , width_ "480"
                                , height_ "270"
                                ] mempty
                            unless (null inline) $
                                p_ [class_ "figure-description"] inlines
            RenderRSS ->
                p_ (class_ " main__centered post__block post__block_para" : writeAttr attr "") $
                    a_ [href_ fullSrc] "[Ссылка на Youtube]"

            RenderNormal ->
                div_ (class_ " main__full-width post__block post__block_with-embed" : writeAttr attr "") $
                div_ [class_ "post__figure-outer"] $
                div_ [class_ "post__figure-inner post__embed"] $ do
                    iframe_
                        [ class_ "post__embed-lazy"
                        , data_ "src" embedSrc
                        , makeAttribute "allowfullscreen" "allowfullscreen"
                        ] mempty
                    noscript_ $
                        iframe_ [ src_ embedSrc
                                , term "allowfullscreen" "allowfullscreen"] mempty
                    unless (null inline) $
                        p_ [class_ "figure-description"] inlines
    where
        videoId url = T.takeWhile (/= '&') $ T.replace "http://www.youtube.com/watch?v=" "" $
            T.replace "https://www.youtube.com/watch?v=" "" url
        embedSrc = "https://www.youtube.com/embed/" ++
            videoId (fst target) ++ "?wmode=transparent"
        fullSrc = "https://www.youtube.com/watch?v=" ++
            videoId (fst target)

writeIframe :: Attr -> [Inline] -> Target -> WriterState (Html ())
writeIframe attr _ target = do
    options <- use writerOptions

    return $
        case options ^. renderType of
            RenderAMP ->
                div_ (class_ " main__full-width post__block post__block_with-embed" : writeAttr attr "") $
                    div_ [class_ "post__figure-outer"] $
                    div_ [class_ "post__figure-inner post__embed"] $
                    ampIframe_ [ src_ $ fst target
                        , term "frameborder" "0"
                        , term "allowfullscreen" "allowfullscreen"
                        , term "layout" "responsive"
                        , term "sandbox" "allow-scripts allow-same-origin allow-popups"
                        , width_ "480"
                        , height_ "270"
                        ] mempty

            RenderRSS ->
                p_ (class_ " main__centered post__block post__block_para" : writeAttr attr "") $
                    a_ [href_ $ fst target] (toHtml $ fst target)

            RenderNormal ->
                div_ (class_ " main__full-width post__block post__block_with-embed" : writeAttr attr "") $
                    div_ [class_ "post__figure-outer"] $
                    div_ [class_ "post__figure-inner post__embed"] $ do
                        iframe_ [ class_ "post__embed-lazy"
                                , data_ "src" $ fst target
                                , term "allowfullscreen" "allowfullscreen"] mempty
                        noscript_ $
                            iframe_ [ src_ $ fst target
                                    , term "allowfullscreen" "allowfullscreen"] mempty


writeImage :: Attr -> [Inline] -> Target -> WriterState (Html ())
writeImage attr inline target = do
    inlines <- concatInlines inline
    options <- use writerOptions

    return $
        case options ^. renderType of
            RenderAMP ->
                figure_
                    ([ id_ (extractId $ fst target)
                     , class_ $ " main__full-width post__block post__figure"
                        ++ if options ^. showFigureNumbers
                            then " post__figure_with-number"
                            else ""
                     ] ++ writeAttr attr "") $
                    div_ [class_ "post__figure-outer"] $
                        div_ [class_ "post__figure-inner"] $ do
                            img options
                            unless (null inline) $
                              p_ [class_ "post__figure-description"] inlines
            RenderRSS ->
                figure_
                    ([ id_ (extractId $ fst target)
                     , class_ " main__full-width post__block post__figure"
                     ] ++ writeAttr attr "") $
                    div_ [class_ "post__figure-outer"] $
                        div_ [class_ "post__figure-inner"] $ do
                            img options
                            unless (null inline) $
                              p_ [class_ "post__figure-description"] inlines

            RenderNormal ->
                figure_
                    ([ id_ (extractId $ fst target)
                     , class_ $ " main__full-width post__block post__figure"
                        ++ if options ^. showFigureNumbers
                            then " post__figure_with-number"
                            else ""
                        ++ if options ^. responsiveFigures
                            then " post__figure_responsive"
                            else ""
                     ] ++ writeAttr attr "") $
                    div_ [class_ $ "post__figure-outer"
                            ++ if options ^. responsiveFigures
                                then " post__figure-outer_responsive"
                                else ""] $
                        div_ [class_ $ "post__figure-inner"
                                ++ if options ^. responsiveFigures
                                    then " post__figure-inner_responsive"
                                    else ""] $ do
                            img options
                            unless (null inline) $
                              p_ [class_ "post__figure-description"] inlines


    where
        -- http://dikmax.name/images/travel/2014-06-eurotrip/rome-santa-maria-maggiore-1.jpg -> rome-santa-maria-maggiore-1
        extractId = T.init . fst . T.breakOnEnd "." . snd . T.breakOnEnd "/"

        img :: LucidWriterOptions -> Html ()
        img options =
            case options ^. renderType of
                RenderAMP ->
                    ampImg_
                        ([ class_ "post__figure-img_amp"
                        , src_ $ linkToAbsolute (options ^. renderType) (fst target)
                          (options ^. siteDomain)
                        , term "layout" "responsive"
                        , alt_ $ fixImageTitle $ snd target
                        ] ++ imgAttrs options)
                RenderRSS ->
                    img_ ([ class_ "post__figure-img"
                        , src_ $ linkToAbsolute (options ^. renderType) (fst target)
                          (options ^. siteDomain)
                        , alt_ $ fixImageTitle $ snd target
                        ] ++ imgAttrs options)
                RenderNormal -> do
                    picture_ [] $ do
                        avifSource
                        webpSource
                        img_ ([ class_ $ "post__figure-img"
                                ++ if options ^. responsiveFigures
                                    then " post__figure-img_responsive"
                                    else ""
                            , src_ $ linkToAbsolute (options ^. renderType) (fst target)
                                (options ^. siteDomain)
                            , alt_ $ fixImageTitle $ snd target
                            , loading_ "lazy"
                            ] ++ imgAttrs options)

        avifSource =
            when (".jpg" `T.isSuffixOf` fst target && "/" `T.isPrefixOf` fst target) $
                source_ [ makeAttribute "srcset" (fst target ++ ".avif"), type_ "image/avif"]

        webpSource =
            when ("/" `T.isPrefixOf` fst target) $
                source_ [ makeAttribute "srcset" (fst target ++ ".webp"), type_ "image/webp"]

        imgAttrs :: LucidWriterOptions -> [Attribute]
        imgAttrs options = case (options ^. commonData ^. imageMeta) $ fst target of
            Just meta ->
                [ width_ $ tshow $ meta ^. imageWidth
                , height_ $ tshow $ meta ^. imageHeight
                ] :: [Attribute]
            Nothing -> []

        fixImageTitle :: Text -> Text
        fixImageTitle title
            | T.take 4 title == "fig:" = T.drop 4 title
            | otherwise = title


getFooter :: WriterState (Html ())
getFooter = do
    n <- use notesList
    options <- use writerOptions
    return $
        if not $ null n
            then div_ [class_ "main__centered post__footnotes"] $ do
                hr_ []
                ol_ $ transformNotes n 1 ("note-" ++ (options ^. idPrefix))
            else mempty
            {- | not $ null $ notesList writerState -}
    where
        transformNotes :: [Html ()] -> Int -> Text -> Html ()
        transformNotes (n:ns) i prefix = do
            li_ [data_ "for" (prefix ++ tshow i)] n
            transformNotes ns (i+1) prefix
        transformNotes [] _ _ = mempty

linkToAbsolute :: RenderType -> Text -> Text -> Text
linkToAbsolute RenderRSS link "" = link
linkToAbsolute RenderRSS link domain
  | (T.length link >= 2) && (T.head link == '/') && T.head (T.tail link) /= '/' = domain ++ link
  | otherwise = link
linkToAbsolute _ link _ = link

withCountBlocksIncrement :: (Int -> WriterState a) -> WriterState a
withCountBlocksIncrement process = do
    countBlocks += 1
    cb <- use countBlocks
    process cb

getMainBlockClass :: WriterState Text
getMainBlockClass = do
    l <- use level
    return $ if l > 1 then "post__block " else "main__centered post__block "
