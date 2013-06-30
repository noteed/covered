-- Markup Haskell source with program coverage and syntax highlighting.
-- Based on hpc-markup (BSD), Andy Gill and Colin Runciman, June 2006.
-- Based on hscolour (GPL).

module Main (main) where

import Control.Monad (when)
import Data.Array
import Data.List
import Data.Monoid
import Language.Haskell.HsColour.Anchors
import Language.Haskell.HsColour.Classify
import Language.Haskell.HsColour.HTML
import System.Environment (getArgs)
import System.IO (localeEncoding)
import Trace.Hpc.Mix
import Trace.Hpc.Tix
import Trace.Hpc.Util

------------------------------------------------------------------------------

main :: IO ()
main = do
  [name] <- getArgs
  generate False name

index_name, index_fun, index_alt, index_exp :: String
index_name = "index"
index_fun  = "definitions"
index_alt  = "alternatives"
index_exp  = "expressions"

generate :: Bool -> String -> IO ()
generate indices prog = do
  mtix <- readTix $ getTixFileName prog
  let Tix tixs = maybe (error $ "unable to find tix file for " ++ prog) id mtix
  helpers <- mapM helper tixs
  mapM_ (renderModule True True) helpers

  when indices $ do
    let mods = map summary helpers
 
    writeSummary index_name $ sortBy (\(n1,_,_) (n2,_,_) -> compare n1 n2) mods
 
    writeSummary index_fun $ sortBy (\(_,_,s1) (_,_,s2) ->
          compare (percent (topFunTicked s2) (topFunTotal s2))
                  (percent (topFunTicked s1) (topFunTotal s1)))
          mods
 
    writeSummary index_alt $ sortBy (\(_,_,s1) (_,_,s2) ->
          compare (percent (altTicked s2) (altTotal s2))
                  (percent (altTicked s1) (altTotal s1)))
          mods
 
    writeSummary index_exp $ sortBy (\(_,_,s1) (_,_,s2) ->
          compare (percent (expTicked s2) (expTotal s2))
                  (percent (expTicked s1) (expTotal s1)))
          mods

writeSummary :: String -> [(String, String, ModuleSummary)] -> IO ()
writeSummary filename mods = do
        putStrLn $ "Generating " ++ filename ++ ".html."

        writeFile (filename ++ ".html") $
            "<html>" ++
            "<head>" ++
            charEncodingTag ++
            "<style type=\"text/css\">" ++
            "table.bar { background-color: #f25913; }\n" ++
            "td.bar { background-color: #60de51;  }\n" ++
            "td.invbar { background-color: #f25913;  }\n" ++
            "table.dashboard { border-collapse: collapse  ; border: solid 1px black }\n" ++
            ".dashboard td { border: solid 1px black }\n" ++
            ".dashboard th { border: solid 1px black }\n" ++
            "</style>\n" ++
            "</head>" ++
            "<body>" ++
            "<table class=\"dashboard\" width=\"100%\" border=1>\n" ++
            "<tr>" ++
            "<th rowspan=2><a href=\"" ++ index_name ++ ".html\">module</a></th>" ++
            "<th colspan=3><a href=\"" ++ index_fun ++ ".html\">Top Level Definitions</a></th>" ++
            "<th colspan=3><a href=\"" ++ index_alt ++ ".html\">Alternatives</a></th>" ++
            "<th colspan=3><a href=\"" ++ index_exp ++ ".html\">Expressions</a></th>" ++
            "</tr>" ++
            "<tr>" ++
            "<th>%</th>" ++
            "<th colspan=2>covered / total</th>" ++
            "<th>%</th>" ++
            "<th colspan=2>covered / total</th>" ++
            "<th>%</th>" ++
            "<th colspan=2>covered / total</th>" ++
            "</tr>" ++
            concatMap showModuleSummary mods ++
            "<tr></tr>" ++
            showTotalSummary
              (mconcat [ modSummary | (_,_,modSummary) <- mods ]) ++
            "</table></body></html>\n"


charEncodingTag :: String
charEncodingTag =
    "<meta http-equiv=\"Content-Type\" " ++
          "content=\"text/html; " ++ "charset=" ++ show localeEncoding ++ "\">"

helper :: TixModule -> IO (String, String, Mix, Array Int Integer)
helper tix = do
  let moduleName = tixModuleName tix
      moduleTixs = tixModuleTixs tix
      fileName = moduleName ++ ".hs.html"
  mix <- readMix [".hpc"] (Right tix)
  let arr_tix :: Array Int Integer
      arr_tix = listArray (0, length moduleTixs - 1) moduleTixs
  return (moduleName, fileName, mix, arr_tix)

summary (moduleName, fileName, mix, arr_tix) =
  let (Mix _ _ _ _ mix') = mix
      tickedWith n = arr_tix ! n
      isTicked n = tickedWith n /= 0

      modSummary = foldr (.) id
             [ \ st ->
               case boxLabel of
                 ExpBox False
                        -> st { expTicked = ticked (expTicked st)
                              , expTotal = succ (expTotal st)
                              }
                 ExpBox True
                        -> st { expTicked = ticked (expTicked st)
                              , expTotal = succ (expTotal st)
                              , altTicked = ticked (altTicked st)
                              , altTotal = succ (altTotal st)
                              }
                 TopLevelBox _ ->
                           st { topFunTicked = ticked (topFunTicked st)
                              , topFunTotal = succ (topFunTotal st)
                              }
                 _ -> st
             | (gid,(_pos,boxLabel)) <- zip [0 ..] mix'
             , let ticked = if isTicked gid
                            then succ
                            else id
             ] $ mempty

  in modSummary `seq` (moduleName, fileName, modSummary)

renderModule anchor number (moduleName, fileName, mix, arr_tix) = do
  let (Mix origFile _ _ tabStop mix') = mix
      tickedWith n = arr_tix ! n
      isTicked n = tickedWith n /= 0

      info = [ (pos,theMarkup)
             | (gid,(pos,boxLabel)) <- zip [0 ..] mix'
             , let binBox = case (isTicked gid,isTicked (gid+1)) of
                               (False,False) -> []
                               (True,False)  -> [TickedOnlyTrue]
                               (False,True)  -> [TickedOnlyFalse]
                               (True,True)   -> []
             , let tickBox = if isTicked gid
                             then [IsTicked]
                             else [NotTicked]
             , theMarkup <- case boxLabel of
                                  ExpBox {} -> tickBox
                                  TopLevelBox {}
                                            -> TopLevelDecl False (tickedWith gid) : tickBox
                                  LocalBox {}   -> tickBox
                                  BinBox _ True -> binBox
                                  _             -> []
             ]

  -- add prefix to modName argument
  content <- readFile origFile

  putStrLn $ "Generating " ++ fileName ++ "."
  writeFile fileName $
    let f = if anchor then insertAnchors' else map (uncurry Token)
        content' = markup tabStop info $ f $ tokenise content
    in htmlHeader moduleName ++ hscolour' number content' ++ htmlClose

data Loc = Loc !Int !Int
         deriving (Eq,Ord,Show)

data Markup
     = NotTicked
     | TickedOnlyTrue
     | TickedOnlyFalse
     | IsTicked
     | TopLevelDecl
           Bool     -- display entry totals
           Integer
     deriving (Eq,Show)

data Token =
    Token TokenType String
  | Coverage (Maybe Markup)
  | Anchor String
  deriving (Eq,Show)

markup :: Int                  -- ^ tab size
       -> [(HpcPos, Markup)]    -- ^ random list of tick location pairs
       -> [Token]               -- ^ hscolour text (tokenized, and maybe with anchors) to mark up
       -> [Token]
markup tabStop mix tokens = addMarkup tabStop tokens (Loc 1 1) [] $ sortTicks mix

addMarkup :: Int                   -- tabStop
           -> [Token]               -- text to mark up
           -> Loc                   -- current location
           -> [(Loc, Markup)]       -- stack of open ticks, with closing location
           -> [(Loc, Loc, Markup)]  -- sorted list of tick location pairs
           -> [Token]

addMarkup _ [] _loc os [] = map (const closeTick') os
addMarkup tabStop cs loc ((o, _):os) ticks | loc > o =
  closeTick' : addMarkup tabStop cs loc os ticks

addMarkup tabStop cs loc os ((t1, t2, tik0):ticks) | loc == t1 =
  case os of
  ((_, tik'):_)
    | not (allowNesting tik0 tik')
    -> addMarkup tabStop cs loc os ticks -- already marked or bool within marked bool
  _ -> openTick' tik0 : addMarkup tabStop cs loc (addTo (t2, tik0) os) ticks

 where

  addTo (t,tik) []             = [(t,tik)]
  addTo (t,tik) ((t',tik'):xs) | t <= t'   = (t,tik):(t',tik'):xs
                               | otherwise = (t',tik):(t',tik'):xs

addMarkup tabStop0 cs loc os ((t1, _t2, _tik):ticks) | loc > t1 =
  -- throw away this tick, because it is from a previous place ??
  addMarkup tabStop0 cs loc os ticks

addMarkup tabStop0 (Token Space "\n":cs) loc@(Loc ln col) os@((Loc ln2 col2,_):_) ticks
  | ln == ln2 && col < col2
  = addMarkup tabStop0 (Token Space " ":Token Space "\n":cs) loc os ticks

addMarkup tabStop0 (c0:cs) loc@(Loc _ _) os ticks =
  if c0 == Token Space "\n" && os /= []
  then
    map (const closeTick') (downToTopLevel os) ++
    [c0] ++
    map (openTick' . snd) (reverse (downToTopLevel os)) ++
    addMarkup tabStop0 cs loc' os ticks
  else
    c0 : addMarkup tabStop0 cs loc' os ticks
  where
  loc' = incBy c0 loc

  incBy :: Token -> Loc -> Loc
  incBy (Token _ "\n") (Loc ln _c) = Loc (ln + 1) 1
  -- TODO s can contain tabs.
  incBy (Token _ s) (Loc ln c) = Loc ln (c + length s)
  incBy _ lc = lc

addMarkup tabStop cs loc os ticks = [Token String $ "ERROR: " ++ show (take 10 cs,tabStop,loc,take 10 os,take 10 ticks)]

sortTicks :: [(HpcPos, Markup)] -> [(Loc, Loc, Markup)]
sortTicks mix = sortBy
  (\(locA1, locZ1, _) (locA2, locZ2, _) -> (locA1, locZ2) `compare` (locA2, locZ1))
  locations
  where
    locations = [ (Loc ln1 c1, Loc ln2 c2, mark)
                | (pos, mark) <- mix
                , let (ln1, c1, ln2, c2) = fromHpcPos pos
                ]

openTick' :: Markup -> Token
openTick' = Coverage . Just

closeTick' :: Token
closeTick' = Coverage Nothing

openTopDecl :: String
openTopDecl = "<span class=\"decl\">"

downToTopLevel :: [(Loc,Markup)] -> [(Loc,Markup)]
downToTopLevel ((_,TopLevelDecl {}):_) = []
downToTopLevel (o : os)               = o : downToTopLevel os
downToTopLevel []                     = []


-- build in logic for nesting bin boxes

allowNesting :: Markup  -- innermost
            -> Markup   -- outermost
            -> Bool
allowNesting n m               | n == m = False -- no need to double nest
allowNesting IsTicked TickedOnlyFalse   = False
allowNesting IsTicked TickedOnlyTrue    = False
allowNesting _ _                        = True

------------------------------------------------------------------------------

data ModuleSummary = ModuleSummary
     { expTicked :: !Int
     , expTotal  :: !Int
     , topFunTicked :: !Int
     , topFunTotal  :: !Int
     , altTicked :: !Int
     , altTotal  :: !Int
     }
     deriving (Show)


showModuleSummary :: (String, String, ModuleSummary) -> String
showModuleSummary (modName, fileName, modSummary) =
  "<tr>\n" ++
  "<td>&nbsp;&nbsp;<tt>module <a href=\"" ++ fileName ++ "\">"
                              ++ modName ++ "</a></tt></td>\n" ++
   showSummary (topFunTicked modSummary) (topFunTotal modSummary) ++
   showSummary (altTicked modSummary) (altTotal modSummary) ++
   showSummary (expTicked modSummary) (expTotal modSummary) ++
  "</tr>\n"

showTotalSummary :: ModuleSummary -> String
showTotalSummary modSummary =
  "<tr style=\"background: #e0e0e0\">\n" ++
  "<th align=left>&nbsp;&nbsp;Program Coverage Total</tt></th>\n" ++
   showSummary (topFunTicked modSummary) (topFunTotal modSummary) ++
   showSummary (altTicked modSummary) (altTotal modSummary) ++
   showSummary (expTicked modSummary) (expTotal modSummary) ++
  "</tr>\n"

showSummary :: (Integral t, Show t) => t -> t -> String
showSummary ticked total =
                "<td align=\"right\">" ++ showP (percent ticked total) ++ "</td>" ++
                "<td>" ++ show ticked ++ "/" ++ show total ++ "</td>" ++
                "<td width=100>" ++
                    (case percent ticked total of
                       Nothing -> "&nbsp;"
                       Just w -> bar w "bar"
                     )  ++ "</td>"
     where
        showP Nothing = "-&nbsp;"
        showP (Just x) = show x ++ "%"
        bar 0 _     = bar 100 "invbar"
        bar w inner = "<table cellpadding=0 cellspacing=0 width=\"100\" class=\"bar\">" ++
                         "<tr><td><table cellpadding=0 cellspacing=0 width=\"" ++ show w ++ "%\">" ++
                              "<tr><td height=12 class=" ++ show inner ++ "></td></tr>" ++
                              "</table></td></tr></table>"

percent :: (Integral a) => a -> a -> Maybe a
percent ticked total = if total == 0 then Nothing else Just (ticked * 100 `div` total)


instance Monoid ModuleSummary where
  mempty = ModuleSummary
                  { expTicked = 0
                  , expTotal  = 0
                  , topFunTicked = 0
                  , topFunTotal  = 0
                  , altTicked = 0
                  , altTotal  = 0
                  }
  mappend (ModuleSummary eTik1 eTot1 tTik1 tTot1 aTik1 aTot1)
          (ModuleSummary eTik2 eTot2 tTik2 tTot2 aTik2 aTot2)
     = ModuleSummary (eTik1 + eTik2) (eTot1 + eTot2) (tTik1 + tTik2) (tTot1 + tTot2) (aTik1 + aTik2) (aTot1 + aTot2)


------------------------------------------------------------------------------
-- From hscolour, which is under GPL.

-- | Formats Haskell source code using HTML with font tags.
hscolour' :: Bool         -- ^ Whether to number lines.
         -> [Token]      -- ^ Haskell source code.
         -> String       -- ^ Coloured Haskell source code.
hscolour' number = wrap
  . unlines . map wrap' . lines
  . (if number then addLines else id)
  . unlines . map (pre . code) . lines
  . concatMap renderToken

addLine :: Int -> String -> String
addLine n xs = "<span class=\"lineno\" id=\"n" ++ show n ++ "\"><a href=\"#n" ++ show n ++ "\">" ++ show n ++ "</a></span>" ++ xs

addLines :: String -> String
addLines = unlines . map (uncurry addLine) . zip [1 :: Int ..] . lines

insertAnchors' :: [(TokenType, String)] -> [Token]
insertAnchors' = map (either Anchor (uncurry Token)) . insertAnchors

wrap :: String -> String
wrap = ("<div class=container><div>"++) . (++"</div></div>")

wrap' :: String -> String
wrap' = ("<div class=code>"++) . (++"</div>")

pre :: String -> String
pre = ("<pre>"++) . (++"</pre>")

code :: String -> String
code = ("<code>"++) . (++"</code>")

renderToken :: Token -> String
renderToken (Coverage Nothing) = closeTick
renderToken (Coverage (Just m)) = openTick m
renderToken (Token t s) = colourise' t
  (if t == Comment then renderComment s else escape s)
renderToken (Anchor s) = "<a name=\"" ++ s ++ "\"></a>"

closeTick :: String
closeTick = "</span>"

openTick :: Markup -> String
openTick NotTicked       = "<span class=\"nottickedoff\">"
openTick IsTicked        = "<span class=\"istickedoff\">"
openTick TickedOnlyTrue  = "<span class=\"tickonlytrue\">"
openTick TickedOnlyFalse = "<span class=\"tickonlyfalse\">"
openTick (TopLevelDecl False _) = openTopDecl
openTick (TopLevelDecl True 0)
         = "<span class=\"funcount\">-- never entered</span>" ++
           openTopDecl
openTick (TopLevelDecl True 1)
         = "<span class=\"funcount\">-- entered once</span>" ++
           openTopDecl
openTick (TopLevelDecl True n0)
         = "<span class=\"funcount\">-- entered " ++ showBigNum n0 ++ " times</span>" ++ openTopDecl
  where showBigNum n | n <= 9999 = show n
                     | otherwise = showBigNum' (n `div` 1000) ++ "," ++ showWith (n `mod` 1000)
        showBigNum' n | n <= 999 = show n
                      | otherwise = showBigNum' (n `div` 1000) ++ "," ++ showWith (n `mod` 1000)
        showWith n = take 3 $ reverse $ ("000" ++) $ reverse $ show n


colourise' :: TokenType -> String -> String
colourise' Space    = id
colourise' Comment  = ("<span class=hs-comment>" ++) . (++ "</span>")
colourise' Keyword  = \s ->
  if s == "import"
  then ("<span class=hs-import>" ++ s ++ "</span>")
  else ("<span class=hs-keyword>" ++ s ++ "</span>")
colourise' Keyglyph = ("<span class=hs-keyglyph>" ++) . (++ "</span>")
colourise' Layout   = ("<span class=hs-layout>" ++) . (++ "</span>")
colourise' Conid    = ("<span class=hs-conid>" ++) . (++ "</span>")
colourise' Varid    = ("<span class=hs-varid>" ++) . (++ "</span>")
colourise' Conop    = ("<span class=hs-conop>" ++) . (++ "</span>")
colourise' Varop    = ("<span class=hs-varop>" ++) . (++ "</span>")
colourise' String   = ("<span class=hs-string>" ++) . (++ "</span>")
colourise' Char     = ("<span class=hs-char>" ++) . (++ "</span>")
colourise' Number   = ("<span class=hs-numer>" ++) . (++ "</span>")
colourise' Cpp      = ("<span class=hs-cpp>" ++) . (++ "</span>")
colourise' Error    = ("<span class=hs-error>" ++) . (++ "</span>")
colourise' Definition = ("<span class=hs-definition>" ++) . (++ "</span>")

htmlHeader ::  String -> String
htmlHeader title = unlines
  [ "<!DOCTYPE HTML>"
  , "<html>"
  , "<head>"
  , "<meta charset=\"utf-8\">"
  , "<!-- Generated by Covered, https://github.com/noteed/covered -->"
  , "<title>" ++ title ++ "</title>"
  , "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">"
  , "<link rel=\"stylesheet\" type=\"text/css\" href=\"css/bootstrap.min.css\">"
  , "<link rel=\"stylesheet\" type=\"text/css\" href=\"css/font-awesome.min.css\">"
  , "<link rel=\"stylesheet\" type=\"text/css\" href=\"css/style.css\">"
  , "<link rel=\"stylesheet\" type=\"text/css\" href=\"css/bootstrap-responsive.min.css\">"
  , "</head>"
  , "<body>"
  ]

htmlClose ::  String
htmlClose  = "\n</body>\n</html>"
