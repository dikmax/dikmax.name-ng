{-# LANGUAGE OverloadedStrings #-}

module LucidWriter (writeLucid, writeLucidString) where

import           Control.Monad.State
import           Data.Data
import           Data.Default
import           Data.Text.Lazy      hiding (map)
import           Lucid
import           Lucid.Base
import           Prelude             hiding (drop, head, init, length, null,
                                      tail, take, takeWhile)
import qualified Prelude             as P
import           Text.Pandoc


data LucidWriterOptions = LucidWriterOptions
    { idPrefix     :: Text
    , siteDomain   :: Text
    , debugOutput  :: Bool
    , renderForRSS :: Bool
    }

instance Default LucidWriterOptions where
    def = LucidWriterOptions
        { idPrefix     = ""
        , siteDomain   = ""
        , debugOutput  = False
        , renderForRSS = False
        }

data WriterStateData = WriterStateData
    { notesList     :: [Html ()]
    , rawData       :: Text
    , rawInline     :: Text
    , writerOptions :: LucidWriterOptions
    , countBlocks   :: Int
    }

emptyState :: WriterStateData
emptyState = WriterStateData
    { notesList     = []
    , rawData       = ""
    , rawInline     = ""
    , writerOptions = def
    , countBlocks   = 0
    }

type WriterState = State WriterStateData

writeLucidString :: LucidWriterOptions -> Pandoc -> String
writeLucidString options pandoc = unpack $ renderText $ writeLucid options pandoc

writeLucid :: LucidWriterOptions -> Pandoc -> Html ()
writeLucid options pandoc
    | debugOutput options =
        pre_ (toHtml $ writeNative def pandoc)
    | otherwise = evalState (writeLucid' pandoc) emptyState { writerOptions = updateOptions }
    where
        updateOptions = options
            { idPrefix = if idPrefix options == "" then "new" else idPrefix options
            }

writeLucid' :: Pandoc -> WriterState (Html ())
writeLucid' (Pandoc _ blocks) = do
    mainBlocks <- concatBlocks blocks
    return $ mainBlocks
    {-
    mainBlocks <- concatBlocks blocks
    footerBlocks <- getFooter
    return $ mainBlocks ++ footerBlocks
    -}

parseRawString :: Text -> Html ()
parseRawString = toHtmlRaw
{-
  | raw == "" = mempty
  | otherwise = either (\a -> [TextNode $ pack a]) extractData $ parseHTML "raw" $ encodeUtf8 raw
  where
    extractData (HtmlDocument _ _ content) = content
    extractData (XmlDocument _ _ content) = content
-}

concatBlocks :: [Block] -> WriterState (Html ())
concatBlocks blocks = do
    result <- mapM concatBlocks' blocks
    writerState <- get
    put writerState {rawData = ""}
    return $ do
        mconcat result
        parseRawString $ rawData writerState

concatBlocks' :: Block -> WriterState (Html ())
concatBlocks' block@(Plain _) = writeBlock block
concatBlocks' block@(RawBlock _ _) = writeBlock block
concatBlocks' block = do
    writerState <- get
    put writerState {rawData = ""}
    items <- writeBlock block
    return $ do
          parseRawString $ rawData writerState
          items

writeBlock :: Block -> WriterState (Html ())
writeBlock (Plain inline) = do
    inlines <- concatInlines inline
    -- modify (\s -> s { rawData = rawData s `append` decodeUtf8 (toByteString $ renderHtmlFragment UTF8 inlines) } )
    return mempty
writeBlock (Para inline) = do
    s <- get
    put s { countBlocks = countBlocks s + 1 }
    inlines <- concatInlines inline
    return $ case inline of
        [Image _ _] -> inlines
        _ -> p_ [id_ $ toStrict $ pack $ "p-" ++ show (countBlocks s + 1)] inlines

writeBlock (CodeBlock (identifier, classes, others) code) =
    return $ pre_ mapAttrs $ code_ mapAttrs $ toHtml code
  where
    mapAttrs = writeAttr (identifier, "sourceCode" : classes, others)
{-
writeBlock (RawBlock "html" str) = do
  modify (\s -> s { rawData = rawData s `append` pack str })
  return []
writeBlock (RawBlock _ _) = return []
writeBlock (BlockQuote blocks) = do
  items <- concatBlocks blocks
  return [ Element "blockquote" [] items ]
writeBlock (OrderedList (startNum, numStyle, _) listItems) = do
  s <- get
  put s { countBlocks = countBlocks s + 1 }
  items <- foldM processListItems [] listItems
  return [ Element "ol"
    ( [("type", case numStyle of
        Decimal    -> "1"
        LowerAlpha -> "a"
        UpperAlpha -> "A"
        LowerRoman -> "i"
        UpperRoman -> "I"
        _          -> "1"
      ), ("id", pack $ "p-" ++ show (countBlocks s + 1)) ] ++
      [ ("start", pack $ show startNum) | startNum /= 1 ]
    )
    items ]
writeBlock (BulletList listItems) = do
  s <- get
  put s { countBlocks = countBlocks s + 1 }
  items <- foldM processListItems [] listItems
  return [ Element "ul" [("id", pack $ "p-" ++ show (countBlocks s + 1))] items ]
writeBlock (DefinitionList _) = return [TextNode "DefinitionList not implemented"]
writeBlock (Header 1 _ inline) = do  -- TODO second header parameter
  s <- get
  put s { countBlocks = countBlocks s + 1 }
  inlines <- concatInlines inline
  return [Element "h2" [("id", pack $ "p-" ++ show (countBlocks s + 1))] inlines]
writeBlock (Header 2 _ inline) = do
  s <- get
  put s { countBlocks = countBlocks s + 1 }
  inlines <- concatInlines inline
  return [Element "h3" [("id", pack $ "p-" ++ show (countBlocks s + 1))] inlines]
writeBlock (Header 3 _ inline) = do
  s <- get
  put s { countBlocks = countBlocks s + 1 }
  inlines <- concatInlines inline
  return [Element "h4" [("id", pack $ "p-" ++ show (countBlocks s + 1))] inlines]
writeBlock (Header 4 _ inline) = do
  s <- get
  put s { countBlocks = countBlocks s + 1 }
  inlines <- concatInlines inline
  return [Element "h5" [("id", pack $ "p-" ++ show (countBlocks s + 1))] inlines]
writeBlock (Header _ _ inline) = do
  s <- get
  put s { countBlocks = countBlocks s + 1 }
  inlines <- concatInlines inline
  return [Element "h6" [("id", pack $ "p-" ++ show (countBlocks s + 1))] inlines]
writeBlock HorizontalRule = return [Element "hr" [] []]
writeBlock (Table {}) = return [TextNode "Table not implemented"]
writeBlock (Div attr blocks) = do
  items <- concatBlocks blocks
  return [Element "div" (writeAttr attr) items]
writeBlock Null = return []
-}
writeBlock b = return $ toHtml (show (toConstr b) ++ " not implemented")

{-
processListItems :: Html () -> [Block] -> WriterState (Html ())
processListItems nodes blocks = do
  items <- concatBlocks blocks
  return (nodes ++ [Element "li" [] items])
-}
writeAttr :: Attr -> [Attribute]
writeAttr (identifier, classes, others) =
  [id_ $ toStrict $ pack identifier | identifier /= ""] ++
  [class_ $ classesString | classesString /= ""] ++
  map (\(k, v) -> makeAttribute (toStrict $ pack k) (toStrict $ pack v)) others
  where
    classesString = toStrict $ intercalate " " $ map pack classes

concatInlines :: [Inline] -> WriterState (Html ())
concatInlines inlines = do
  result <- mapM concatInlines' inlines
  writerState <- get
  put writerState {rawInline = ""}
  return $ do
      mconcat result
      parseRawString (rawInline writerState)

concatInlines' :: Inline -> WriterState (Html ())
concatInlines' inline = do
  writerState <- get
  if rawInline writerState == ""
    then writeInline inline
    else do
      {- str <- writeRawInline inline
      put writerState
        { rawInline = rawInline writerState `append` str
        }-}
      return mempty

writeInline :: Inline -> WriterState (Html ())
writeInline (Str string) = return $ toHtml string
{-
writeInline (Emph inline) = do
  inlines <- concatInlines inline
  return [Element "em" [] inlines]
writeInline (Strong inline) = do
  inlines <- concatInlines inline
  return [Element "strong" [] inlines]
writeInline (Strikeout inline) = do
  inlines <- concatInlines inline
  return [Element "s" [] inlines]
writeInline (Superscript inline) = do
  inlines <- concatInlines inline
  return [Element "sup" [] inlines]
writeInline (Subscript inline) = do
  inlines <- concatInlines inline
  return [Element "sub" [] inlines]
writeInline (SmallCaps inline) = do
  inlines <- concatInlines inline
  return [Element "span" [("style", "font-variant: small-caps;")] inlines]
writeInline (Quoted SingleQuote inline) = do
  inlines <- concatInlines inline
  return $ [TextNode "'"] ++ inlines ++ [TextNode "'"]
writeInline (Quoted DoubleQuote inline) = do
  inlines <- concatInlines inline
  return $ [TextNode "«"] ++ inlines ++ [TextNode "»"]
writeInline (Cite _ _) = return [TextNode "Cite not implemented"]
-}
writeInline (Code attr code) = return $ code_ (writeAttr attr) (toHtml code)
writeInline Space = return " "
writeInline LineBreak = return $ br_ []
{-
writeInline (Math InlineMath str) = return
  [ Element "span" [("class", "math")]
    [ TextNode $ "\\(" `append` pack str `append` "\\)" ]
  ]
writeInline (Math DisplayMath str) = return
  [ Element "span" [("class", "math")]
    [ TextNode $ "\\[" `append` pack str `append` "\\]" ]
  ]
writeInline (RawInline "html" str) = do
  modify (\s -> s {rawInline = rawInline s `append` pack str})
  return []
writeInline (RawInline _ _) = return []
-}
writeInline (Link inline target) = do
  inlines <- concatInlines inline
  writerState <- get
  return $ a_
    [ href_ $ toStrict $ linkToAbsolute (renderForRSS (writerOptions writerState))
        (pack $ fst target) (siteDomain (writerOptions writerState))
    , title_ $ toStrict $ pack $ snd target
    ] inlines

writeInline (Image inline target) = do
    inlines <- concatInlines inline
    writerState <- get

    return $ if "http://www.youtube.com/watch?v=" `isPrefixOf` pack (fst target) ||
            "https://www.youtube.com/watch?v=" `isPrefixOf` pack (fst target)
        -- Youtube video
        then div_ [class_ "figure"] $ do
            div_ [class_ "embed-responsive embed-responsive-16by9"] $ do
                iframe_
                    [ src_ $ toStrict $ "https://www.youtube.com/embed/" `append`
                        videoId (pack $ fst target) `append` "?wmode=transparent"
                    , makeAttribute "allowfullscreen" "allowfullscreen"
                    , class_ "img-polaroid embed-responsive-item"
                    ] mempty
                if (null $ renderText inlines)
                    then p_ [class_ "figure-description"] inlines
                    else mempty
        else div_ [id_ $ toStrict (extractId $ pack $ fst target), class_ "figure"] $
            div_ [class_ "figure-inner"] $ do
                img_ [ src_ $ toStrict $ linkToAbsolute (renderForRSS (writerOptions writerState)) (pack $ fst target)
                        (siteDomain (writerOptions writerState))
                    , alt_ $ toStrict $ pack $ fixImageTitle $ snd target
                    , class_ "img-polaroid"
                    ]
                if (null $ renderText inlines)
                    then p_ [class_ "figure-description"] inlines
                    else mempty
  where
    videoId url = takeWhile (/= '&') $ replace "http://www.youtube.com/watch?v=" "" $
        replace "https://www.youtube.com/watch?v=" "" url
    -- http://dikmax.name/images/travel/2014-06-eurotrip/rome-santa-maria-maggiore-1.jpg -> rome-santa-maria-maggiore-1
    extractId = init . fst . breakOnEnd "." . snd . breakOnEnd "/"
{-
writeInline (Note block) = do
  blocks <- concatBlocks block
  writerState <- get
  put writerState {
    notesList = notesList writerState ++ [blocks]
  }
  let noteId = length (notesList writerState) + 1
  return
    [ Element "sup"
      [ ("id", "note-" `append` idPrefix (writerOptions writerState) `append` pack (show noteId))
      , ("class", "note-link")
      ]
      [ TextNode $ pack $ show noteId ]
    ]
writeInline (Span attr inline) = do
  inlines <- concatInlines inline
  return [ Element "span" (writeAttr attr) inlines ]
-}
writeInline i = return $ toHtml (show (toConstr i) ++ " not implemented")
{-
concatRawInlines :: [Inline] -> WriterState Text
concatRawInlines inlines = do
  writerState <- get
  put writerState {rawInline = ""}
  result <- foldM concatRawInlines' "" inlines
  put writerState {rawInline = rawInline writerState}
  return result

concatRawInlines' :: Text -> Inline -> WriterState Text
concatRawInlines' text inline = do
  str <- writeRawInline inline
  return $ text `append` str

writeRawAttr :: Attr -> Text
writeRawAttr attr =
  -- TODO value escaping
  intercalate " " $ map (\(k, v) -> k `append` "=\"" `append` v `append` "\"") $ writeAttr attr

writeRawInline :: Inline -> WriterState Text
writeRawInline (Str string) = return $ pack string
writeRawInline (Emph inline) = do
  inlines <- concatRawInlines inline
  return ("<em>" `append` inlines `append` "</em>")
writeRawInline (Strong inline) = do
  inlines <- concatRawInlines inline
  return ("<strong>" `append` inlines `append` "</strong>")
writeRawInline (Strikeout inline) = do
  inlines <- concatRawInlines inline
  return ("<s>" `append` inlines `append` "</s>")
writeRawInline (Superscript inline) = do
  inlines <- concatRawInlines inline
  return ("<sup>" `append` inlines `append` "</sup>")
writeRawInline (Subscript inline) = do
  inlines <- concatRawInlines inline
  return ("<sub>" `append` inlines `append` "</sub>")
writeRawInline (SmallCaps inline) = do
  inlines <- concatRawInlines inline
  return ("<span style=\"font-variant: small-caps;\">" `append` inlines `append` "</span>")
writeRawInline (Quoted SingleQuote inline) = do
  inlines <- concatRawInlines inline
  return ("'" `append` inlines `append` "'")
writeRawInline (Quoted DoubleQuote inline) = do
  inlines <- concatRawInlines inline
  return ("«" `append` inlines `append` "»")
writeRawInline (Cite _ _) = return "Cite not implemented"
writeRawInline (Code attr code) =
  return ("<code " `append` writeRawAttr attr `append` ">" `append` pack code `append` "</code>" )
writeRawInline Space = return " "
writeRawInline LineBreak = return "<br />"
writeRawInline (Math _ _) = return "Math not implemented"
writeRawInline (RawInline "html" str) = return $ pack str
writeRawInline (RawInline _ _) = return ""
writeRawInline (Link inline target) = do
  inlines <- concatRawInlines inline
  return ("<a " `append` writeRawAttr ("", [], [("href", fst target), ("title", snd target)]) `append` ">"
    `append` inlines `append` "</a>" )
  --writeInline (Image _ _) = [TextNode "Image not implemented"]
writeRawInline (Image inline target) = do
  inlines <- concatRawInlines inline
  return ("<div class=\"\">" `append`
    (if inline /= [] then "<p class=\"figure-description\">" `append` inlines `append` "</p>" else "")
    `append` "<img " `append`
    writeRawAttr ("", [], [ ("src", fst target)
        -- , ("title", fixImageTitle $ snd target)
        , ("alt", fixImageTitle $ snd target)
        , ("class", "img-polaroid")
        ]) `append` " />"
    )
writeRawInline (Note block) = do
  blocks <- concatBlocks block
  writerState <- get
  put writerState {
    notesList = notesList writerState ++ [blocks]
  }
  let noteId = length (notesList writerState) + 1
  return
    ("<sup id=\"node-" `append` idPrefix (writerOptions writerState) `append` pack (show noteId) `append`
      "\" class=\"note-link\">" `append` pack (show noteId) `append` "</sup>")
writeRawInline (Span attr inline) = do
  inlines <- concatRawInlines inline
  return ("<span " `append` writeRawAttr attr `append` ">" `append` inlines `append` "</span>" )


getFooter :: WriterState (Html ())
getFooter = do
  writerState <- get
  return
    div_ [class_ "footnotes"] $ do
        hr_
        ol_ $ transformNotes (notesList writerState) 1 ("note-" `append` idPrefix (writerOptions writerState))
         {- | not $ null $ notesList writerState -}
  where
    transformNotes :: [Html ()] -> Int -> Text -> Html ()
    transformNotes (n:ns) i prefix = do
        li_ [data_ "for" (prefix `append` pack (show i))] n
        transformNotes ns (i+1) prefix
    transformNotes [] _ _ = mempty
-}

-- TODO to local scope ?
fixImageTitle :: String -> String
fixImageTitle title
    | P.take 4 title == "fig:" = P.drop 4 title
    | otherwise = title

-- TODO to local scope ?
linkToAbsolute :: Bool -> Text -> Text -> Text
linkToAbsolute False link _ = link
linkToAbsolute True link "" = link
linkToAbsolute True link domain
  | (length link >= 2) && (head link == '/') && (head $ tail link) /= '/' = domain `append` link
  | otherwise = link
