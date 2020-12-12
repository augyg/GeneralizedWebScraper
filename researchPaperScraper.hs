{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}

module PaperScraper where

import Network.HTTP (getRequest, getResponseBody, simpleHTTP)
import Network.HTTP.Base hiding (GET, POST)
import Network.HTTP.Client.TLS (tlsManagerSettings)

import Data.Proxy 
-- import Network.HTTP.Req
import Data.Text (Text, unpack, pack)
import Data.Text.Encoding (encodeUtf8)

import Network.HTTP.Client (CookieJar, requestBody, method, destroyCookieJar, responseCookieJar, httpLbs, RequestBody (RequestBodyLBS), parseRequest, newManager, responseBody)

import Text.URI (mkURI, URI, uriQuery, uriPath, unRText)
import Data.Maybe (catMaybes, fromJust)
import Text.Parsec hiding (lower)
import GHC.Err (undefined)
import Data.Functor.Classes (eq1)
import Data.List.Extra (lower)
import Data.List (isInfixOf, isSuffixOf)
import qualified Data.List.NonEmpty as NE (last)
-- import Text.RawString.QQ (r)

import Replace.Megaparsec (findAll)
import qualified Text.Megaparsec as MParsec

scrapeUwoDbLinks :: a
scrapeUwoDbLinks = undefined


main :: IO ()
main = do
  manager <- newManager tlsManagerSettings
  
  request <- parseRequest "https://www.google.com/"
  response <- httpLbs request manager

  print (responseBody response)
  



signin :: IO ()
 signin = do
  mgrHttps <- newManager tlsManagerSettings

  requ <- parseRequest "https://login.proxy1.lib.uwo.ca/login"
  let request = requ { method = "POST"
                     , requestBody = RequestBodyLBS "url=a&user=mmorin32&pass=AceRocks!123"}

  response <- httpLbs request mgrHttps
  let cookies = responseCookieJar response

  mapM_ print (destroyCookieJar cookies)


-- |future impl
-- |maybe make data Elem' = ... 
-- runParserOnHTML :: BsResponse -> [Text]
-- runParserOnHTML scraParser res = do
  -- let body = responseBody res
  -- | ensure types of stream and body are the same 
  -- runScraper scraParser body

  
readSiteTree' :: FilePath -> IO [(String, Bool)] -- or [Text] not sure 
readSiteTree' treefile = do
  str <- readFile treefile
  let parser = sepBy (parser') (char ',')
      parser' = do
        url <- many (noneOf ['='])
        _ <- char '|'
        checked <- string "True" <|> string "False"
        case checked of
          "True" -> return (url, True)
          "False" -> return (url, False)
           -- userError "invalid management of Sitetree file"
        
  return $ case (parse parser "irrelevant bs" str) of
    Right list -> list
    Left err -> []
  
-- | I could just make this sorted lines when writing to it, then I dont need to traverse a tree
-- |
-- | eg.
-- | (base urls omitted for quickread)
-- |
-- | home=checked
-- | home/page1=checked
-- | home/zpage=checked
-- | journals=notChecked
-- | journals/aaaa=notChecked
-- | journals/medical=
-- | journals/zebras
-- | ...


updSTree :: FilePath -> [(Text, Bool)] -> IO ()
updSTree treefile newTree = writeFile "test.txt" "" >> updateSiteTree treefile newTree

-- will write whole new site tree 
--take in tuple: (Text, Bool) where UriPath is preconfirmed as being same baseURL
-- | meant to take result of newUrlList 
updateSiteTree :: FilePath -> [(Text, Bool)] -> IO ()
updateSiteTree treefile (x:[]) = appendFile treefile (toStrTree x)
updateSiteTree treefile (x:newTree) = appendFile treefile (toStrTree x <> ",")
                                      >> updateSiteTree treefile newTree

toStrTree :: (Text, Bool) -> String
toStrTree x' = unpack (fst x') <> "|" <> show (snd x') 

-- write as sorted lines (see above)
-- in future add in sorting for precedence




----------------------------------------------------------------------------------------------------
-- |Parsers


-- PSEUDO
-- urls = do
  -- a <- hrefParser 
  -- return validUrls a

type SiteTree = [(Text, Bool)]

-- | do
-- | numQueryparams == 0
-- | all path segements DO NOT contain a dot ie a file unless .pdf
-- | uriAuthority DOES contain baseURL
-- | Comparing the path (type is NonEmpty (String) list) to any in file/tree
-- |    -> means we can easily just mkURI with the read-in url then compare with eq1 func

-- | maybe we still check for about or help before using mkURI


-- | with relative -> we know that uriAuthority will be nothing

-- | the Authoritative base URL that we should use should be passed in when we leave the UWO res DB
-- | directory page to get to the site


newUrlList :: [Maybe Text] -> [(Text, Bool)] -> [(Text, Bool)]
newUrlList newUrls oldUrls = fmap (, False) (catMaybes newUrls) <> oldUrls

-- | Input is meant to be right from 
usefulNewUrls :: String -> SiteTree -> [Text] -> [Maybe HrefURI]
usefulNewUrls _ _ [] = []
usefulNewUrls baseUrl tree (link:links) = (maybeUsefulNewUrl baseUrl (unpack link) tree) : usefulNewUrls baseUrl tree links
  

type HrefURI = String

numberOfQueryParamsIsZero :: Maybe URI -> Maybe Int
numberOfQueryParamsIsZero uri = case uri of
  Just uri' -> if length (uriQuery uri') == 0 then Just 0 else Nothing 
  Nothing -> Nothing 
  -- fmap length (fmap uriQuery uri)


-- fmap (/= 0)

  -- also need to cut out diff base URLs still
  --  -> Could just be elem baseUrl href
  -- | elem True (urlContains url ["javascript", "about", "help", "#"]) = Nothing  
  -- | otherwise =


-- noAboutOrHelpOrFileExtOrJavaScriptOrHashOrQueryParams' :: [Text] -> [Text]
-- AND Has base url 
maybeUsefulNewUrl :: String -> HrefURI -> SiteTree -> Maybe HrefURI
maybeUsefulNewUrl baseUrl url tree =
  case (elem True (urlContains url ["javascript", "about", "help", "#"])) of

                                   --- ^^^ should this include ':' ?

    True -> Nothing
    False -> case (numberOfQueryParamsIsZero uri) of
               Just 0 -> case (isInfixOf baseUrl url) of
                        True ->
                          case elem '.' (getLastPath url) of
                            True -> case (isSuffixOf "html" (getLastPath url) || isSuffixOf "pdf" (getLastPath url)) of
                                      True -> case urlIsNew tree url of
                                                True -> Just url 
                                                False -> Nothing 
                                      False -> Nothing
                     
                            False -> case urlIsNew tree url of
                                       True -> Just url 
                                       False -> Nothing
                        False -> Nothing
               Nothing -> Nothing 

  where uri :: Maybe URI
        uri = mkURI (pack url)

        urlContains :: String -> [String] -> [Bool]
        urlContains url icases = fmap ((flip isInfixOf) (lower url)) icases

        getLastPath url = unpack (unRText (NE.last (snd (fromJust (fromJust (fmap uriPath (mkURI (pack url))))))))


urlIsNew :: SiteTree -> HrefURI -> Bool
urlIsNew [] uri = True
urlIsNew (branch:tree) uri
  | eq1 (fmap uriPath (mkURI' (pack uri))) (fmap uriPath (mkURI' (fst branch))) = False
  | otherwise = urlIsNew tree uri
  where
    mkURI' :: Text -> Maybe URI
    mkURI' url = mkURI url


-- urlIsNew :: SiteTree -> HrefURI -> m Bool
-- urlIsNew [] uri = return True -- succcess
-- urlIsNew (branch:tree) uri = do
--   branch' <- mkURI (fst branch)
--   uri' <- mkURI uri
--   case eq1 (uriPath uri') (uriPath branch') of
--     True -> return False --since we want new 
--     False -> urlIsNew tree uri

  --EXPLANATION
  
  -- the only part of read in urls is the path, we know that all of them will be from the site
  --  since we will have already checked this
  -- THEREFORE : treefile will be a mix of relative and absolute urls

  --         Href ie PATH | Checked?
  -- https://site.com/path|True
  --                /path2|True 
  --                 path3|False
  --
  -- at this point we only act on Path ; not whether its been checked?



                       
catEithers :: [Either a b] -> [b]
catEithers [] = []
catEithers (x:xs) = case x of
                      Left err -> catEithers xs
                      Right a -> a : catEithers xs


-----------------------------------------------------------------------------------------------------
-- | needs to use many for multiple links
hrefParser :: Stream s m Char => ParsecT s u m (String, String)
hrefParser = attrParser (Attr "href")
-- snd OR fmap snd for multiple then analyze URI

-- | for generalization sake
attrParser :: Stream s m Char => AttrsP -> ParsecT s u m (String, String)
attrParser attr = do
      _ <- space
      attr' <- case attr of
                 AnyAttr -> many alphaNum --any alphanumeric
                 Attr texty -> string texty

      -- | Note: the below is optional 
      _ <- char '='
      content <- attrValue 
      return (attr', content)

attrValue :: Stream s m Char => ParsecT s u m [Char]
attrValue = between (char '"') (char '"') (many1 (noneOf ['"']))

-- this could be extensible to scrapePDFLink
attrValue' :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
attrValue' parser = between (char '"') (char '"') parser

data AttrsError = IncorrectAttrs deriving Show

data AttrsP = Attr String
            | AnyAttr
            deriving (Eq, Show, Ord)

type AttrVal = String

data AttrsPNew = XPath [(AttrsP, AttrVal)] | Attrs [AttrsP] | AnyAttrs

attrsValuesExist :: [(String, String)] -> [AttrsPNew] -> Either AttrsError [(String, String)] 
attrsValuesExist = undefined

attrsExist :: [(String, String)] ->  [AttrsP] -> Either AttrsError [(String, String)]
attrsExist attrsOut [AnyAttr] = Right attrsOut
attrsExist attrsOut [] = Right attrsOut 
attrsExist attrsOut (Attr nextAttr:attrsIn)
  | elem nextAttr (fmap fst attrsOut) = attrsExist attrsOut attrsIn
  | otherwise = Left IncorrectAttrs 
  -- Im not sure this will continue to the next element though

-- | Note: for future open source -> need to create BsResponse stream instance in order to
-- | ensure it doesnt hang on source with no '>' character 
-- just really for elem parser to add selectiveness of particular attrs
attrsParser :: Stream s m Char => [AttrsP] -> ParsecT s u m (Either AttrsError [(String, String)])
attrsParser attrs = do 
  attrPairs <- manyTill (optionMaybe (attrParser AnyAttr)) (char '>' <|> char '/')
  return (attrsExist (catMaybes attrPairs) attrs) --all of them if I:Attrs in O:Attrs
  -- | 1. parse the attrs
  -- | 2. case attrs of
  -- |         [] -> return
  -- |         (x:xs) -> elem x [list of attrs expr] .. if true, continue else this is
  -- |                                                not a valid element -> break out of parsing elem

-- this should still return the entire text somehow, maybe a tuple of (fullInner, match)

-- | for Forms: (simple and complex)
-- | Element types: [<textarea>, <input>, <button>, <select>{<option value="">}]
-- | Element --> HTML types: ["submit",

-- | NOTES
-- if href="#" on form -> just means scroll to top 


-- https://search-proquest-com.proxy1.lib.uwo.ca/abicomplete/results/3EA3E85FF6A14618PQ/1?accountid=15115


  --this is also not meant for when the inner text should contain innerElements
  --in this case, one should say `anyToken` for inner html then apply a parser to the innerHTML
  -- attribute of the parsed Elem' record


                   --FUTURE: SHOULD BE AUTOSET TO ,TextOnly String, IF MATCHING ANY ELEM 
-- data InnerTextOptions = TextOnly String | HtmlNesting String --- | SomeNested Int String
-- | SomeNested would be allow up to the given int number of nestings
-- |   generally allows for full range of discrepancy over nesting
-- |   **this also allows a user to grab some level of the context they desire
-- |      -> lets say they want to see how some pattern is surrounded, they could specify 3
-- |         which allows for them to see up to 3 layers of nesting, if its nested that deep 

-- |HTML is like a list
-- | at each node of the top level, the test/parser would be run and return a result if match found
-- |  according to options

-- | -IN CASE OF WHOLE HTML, note that this returns the whole html if HTMLNesting ++ Match found ++
-- |  any element is allowed by higher level func

-- | I believe the way to differentiate between to relate the first 2 opts (SomeNested is future
-- | concern) is to somehow allow everything (such as with HtmlNesting opt) vs everything but
-- | a closing tag where the inner string /= the current opening string

-- | So for example to get an element containing "Browse" do TextOnly "Browse"
-- | eg2 search form -> form: f1 = HtmlNesting "search" in runScraper (Elem="form", f1)  

data InnerTextResult = InnerTextResult { match :: String
                                       , fullInner :: String -- set to Nothing if TextOnly
                                       } deriving Show
  --Note that the fullInner would be useful for stepping in cases as well needing some pieces of the
  -- whole, in the case, that theres a match (like a form: want a form if it has search in it, then
  -- we'd want to use certain html elements within the form such as <input> or <select>

  --so result would be : InnerTextResult { match = "Search", fullInner = """<input> ... <select> """ } 
  
-- | NOTE
-- |
-- | May need 2 functions,
-- |   1. that does check inside a match for also matches
-- |   2. that doesn't check inside for match ie desire the same "level" of nesting
-- | 
-- | AND in case of forms that may be a search, it would be helpful to pass a ParsecT s u m a 
-- |    -> Maybe as simple still as: return this <form></form> if "search" is somewhere inside it
-- |    -> Cuz it would be very challenging to know or be flexible enough for 
  
-- | In case 1, just recursively apply the elemParser to the inner HTML/text


--helper func for elem parser -> should take string of what the tag is 

  
-- make option to return avail but this will likely be used like:

-- _ <- parseEndElem in order to ensure 




type Elem = String

-- type AnyElem = undefined



data Elem' = Elem' { el :: Elem -- change to Elem?
                   , attrs :: [(String, Maybe String)]
                   , innerHtmlMatch :: String --will be "" if not specified
                   , innerHtmlFull :: String
                   } deriving Show
             

                -- would be single item in list for "AnyElem"
                                                       -- might call this AttrResult
parseOpeningTag :: Stream s m Char => ParsecT s u m String -> [AttrsP] -> ParsecT s u m (Elem, [(String, String)])
parseOpeningTag elemOpts attrsSubset = do
  -- _ <- MParsec.manyTill anyToken (char '<' >> elemOpts >> attrsParser attrsSubset) -- the buildElemsOpts [Elem]
  _ <- char '<'
  elem <- elemOpts
  attrs <- attrsParser attrsSubset

  -- _ <- char '>'


  --why not just take out first line and do directly 

  case attrs of
    Left IncorrectAttrs -> parserZero
    Right whateva -> return (elem, whateva)

  -- | For elemsOpts, will either be
  -- | Parser: (anyChar)
  -- | Parser: (buildElemsOpts elems) 


elemToStr :: Elem' -> String
elemToStr elem = "<" <> el elem <> buildAttrs (attrs elem) <> ">" <> innerHtmlFull elem <> "</" <> el elem <> ">"
  where
    buildAttrs [] = ""
    buildAttrs (attr:attrs) = " " <> fst attr <> "=" <> snd attr <> buildAttrs attrs
    

-- | I also need a way to turn off 
parseInnerHTMLAndEndTag :: Stream s m Char => Elem -> Maybe (ParsecT s u m String) -> ParsecT s u m InnerTextResult
parseInnerHTMLAndEndTag elem innerPattern = do

  let baseParser endParse = do
        _ <- char '>'
        (pre, patternFound) <- MParsec.manyTill_ (try sameElTag <|> p) (f innerPattern)
        (post, _) <- MParsec.manyTill_  (try sameElTag <|> p) endParse
    
      anyTagInner = baseParser (try (char '<'
                                     >> (optional (char '/'))
                                     >> some anyChar
                                     >> (char ' ' <|> char '>')))
        
                                                               
                     --could really be written to
                        --openingHead OR any closing tag
                         --Any closing tag eqn needs to be abstracted from current eqn

      
        
      normal = baseParser (try (string ("</" <> elem <> ">")))
      
  _ <- try (string "/>") <|> normal <|> anyTagInner 
  --x <- eitherP (try (string "/>")) (normal <|> anyTagInner)
  --case x of
  --    a -> case elem of {Just a -> parserZero; Nothing -> return InnerTextResult}
  --    b -> return InnerTextResults 


    
  let f :: Stream s m Char => Maybe (ParsecT s u m String) -> ParsecT s u m String
      f x = case x of
              Just pat -> pat
              Nothing -> string ""

      sameElTag = do
        el <- elemParser [] Nothing (Just [elem])
        return $ elemToStr el

      p = do
        a <- anyToken
        return (a : [])

  -- Note: we can parse these better with eitherP
  -- eitherP :: Alternative m => m a -> m b -> m (Either a b)

      -- then use case statement to deal with case (A: sub-elem | B: anychar)
        --if sub-elem -> put in list --then--> 
  
  (pre, patternFound) <- MParsec.manyTill_ (try sameElTag <|> p) (f innerPattern)
  (post, _) <- MParsec.manyTill_  (try sameElTag <|> p) (try (string ("</" <> elem <> ">")))

  return $ InnerTextResult { match = patternFound
                           , fullInner = mconcat pre <> patternFound <> mconcat post }



-- | FUTURE USE CASES: buildElemsOpts :: [ParsecT s u m a] -> ParsecT s u m a -- using <|>
buildElemsOpts :: Stream s m Char => [Elem] -> ParsecT s u m String
-- buildElemsOpts [] = <----- i dont think i need this
buildElemsOpts [] = parserZero
buildElemsOpts (x:elemsAllow) = try (string x) <|> (buildElemsOpts elemsAllow)

  
  


-- |          attrs (Attr | AnyAttr)   maybe discr elem
elemParser :: Stream s m Char => [AttrsP] -> Maybe (ParsecT s u m String) -> Maybe [Elem] -> ParsecT s u m Elem'
elemParser attrs innerSpec elemList = do
  let mkElemParser :: Stream s m Char => Maybe [Elem] -> ParsecT s u m String
      mkElemParser x = case x of
                         Nothing -> MParsec.some (noneOf [' ', '>'])
                         Just elemsOpts -> buildElemsOpts elemsOpts


  -- | May rename parseOpeningTag to elemHeadParser
  -- |  -> Case of input tag: <input ...."> DONE ie no innerhtml or end tag
  -- |  what if we also allowed for any tag to structurally have no innerHTML+Ending?
  -- |   -> kinda like an option over a range of indices -> if gets to end then assume only opening
  -- |       -> in this case: innerHTML = pure ()

  -- | ABOVE WOULD FAIL -> <li> is allowed inner text **without a closing tag

  -- | need to also check for ending "/" for self closing tags
  -- | need to also allow for attr section to just be the attribute with no value -- `optional`
  (elem', attrs') <- parseOpeningTag (mkElemParser elemList) attrs
  
  --note that at this point, there is a set elem' to match

  -- | Given elems like "input" ... should this be `optional` ?

  -- | for handling cases with no closing tag
  -- (try (skipManyTill endTag) <|> -- endtag DNE -> or some parent tag

  -- DO i need a dataset of parent-child specific relationships? 
  
  inner <- parseInnerHTMLAndEndTag elem' innerSpec

  -- Maybe above should not parse '>' at all -> especially given case "/>" at end of head 

  return $ Elem' elem' attrs' (match inner) (fullInner inner)
  -- return (Elem' "div" [("action", "/search")] "stuff")   


    -- Note: this can be passed to findAll func in megaparsec as is

  -- | check elem start
  -- | check attrs
  -- | close starting tag then check inner html
  -- | start closing tag and maybe confirm that that the elem is the same as the opening tag
  -- |   -> if not i believe this is an error
  -- | end closing tag and combine the text

  -- | Output = <'elem1' attr1="..." ... >InnerHTML</'elem1'>


-- --****************
  -- note that this could even be used for mining text on page

  -- eg: skipMany (elementWithInnerText "the") -> Element -then-> innerText _elem

  -- and of course, you could have more checks than: only "the"

  -- searchTextOfInnerElemTextFunc [x]

--PLACEHOLDER
type Url = Text

buildFormUrl :: a -> Url
buildFormUrl = undefined

buildFormUrlCombos :: a -> [Url]
buildFormUrlCombos = undefined
-- | Returns all possible combinations of a form url based on the <select></select> tags it finds

-- ^^
getSelectOptions = undefined
-- > will return a List of List of options to enumerate 

findBrowseLinks :: a
findBrowseLinks = undefined

findAdvancedSearchLinks :: a
findAdvancedSearchLinks = undefined

findPdfLinks :: a
findPdfLinks  = undefined
-- will either be directly downloadable or is another page
--can we check for prefix "<html>.." to determine the case?
-- | I believe this is where haskell ends due to simplicity reasons 

searchIsBigorSmall :: a -- compare to some index
searchIsBigorSmall = undefined

isUWODBloginPage :: a
isUWODBloginPage = undefined

isListingPage :: a
isListingPage = undefined

findPagination :: a
findPagination = undefined
--on listing page

structuredBrowsingLinksExist :: a
structuredBrowsingLinksExist = undefined
-- perhaps we should have some recursive func that scopes in and tries to find the same elem+attrs
-- for each level of nesting, to see if at any point, theres some level of repetition potentially
-- indicating that this is a list of similar things




data ScrapeError = NoLinksOnPage
                 | NoAdvSearchJustBasic
                 | NoSearchOrBrowseOnSite
                 | CouldntFindResultsWithSearchOrBrowse
                 | CouldntPaginate
                 | PaginationMaybeComplete
                 | ItemHasNoFindablePdf
                 | PDFRequestFailed




findAllTill = do
  let f :: ParsecT s u m [Either a b]
      f = manyTill (eitherP anyChar (opt1 <|> opt2 <|>...)) desiredClosingTag

      f' = do
        x <- f
        return $ catEithers x
        
findAllBetween = undefined

-- parseFullHTML :: forall a. Elem' a => BsResponse -> [a] --or record 


-- | both scrapeLinks and elemParser should check each index until the start of it works
-- | ie either **href** or **<Elem**

-- scrapeLinks <|> anyToken
-- elemParser "div" <|> anyToken




-- either begin parsing the element or href and find that it is what youre looking for
-- OR -> skip to start of next element

-- generalized1 :: Char -> ParsecT s u m a -> ParsecT s u m a
-- generalized1 firstChar pattern' = do
  -- try pattern' <|> skipMany1 (satisfy (/= firstChar))
  -- case out of
    -- () -> pattern'
    -- a  -> return a --is pattern




-- either begin parsing the element or href and find that it is what youre looking for
-- OR -> skip to start of next element

-- generalizedMany :: Char -> ParsecT s u m a -> ParsecT s u m [a]
-- generalizedMany firstChar pattern' = many generalized1  
                                                                 --any token except '<'

       --pattern           start of it       
-- gen :: ParsecT s u m a -> ParsecT s u m a -> ParsecT s u m a 





--Note: how do we get a computer to recognize, Boolean-ly if some representation is x?
  -- where x is some statement?

  --As humans we do this visually : is this object red? yes or no

  --A computer must do the same with "local vision" that theoretically must be as parsing algorithm
  -- aka Bool `followedby` Bool2 `followedBy` Bool3 ... Bool_n

    --where some Bools are of set-wise funcs and some are singleton-options AKA / therfore some
    --        sequential combo of (Set 1 of Any <-> Set (totalCount) of Any)

    --Therefore -> this logic generalizes processing to both humans and computers,; the full set of
    -- higher level processors 



 

-- try string "</X>" <|> try  (string "</" >> some anyChar >> string ">") <|> 




-- x <- try (char "<" >> anyChar >> space) <|> anyToken
-- case prefix x '<' of
--   True -> case nexttoken 'of' x is '/' of
--             False -> note that we are one nesting deeper 
--             True -> note that we are one nesting less deep
  
            
--   False -> parse anyToken / continue with current algorithm
--            -- we may have parsed this `anyToken` already anyways


-- also:
--   x <- anyToken
--   y <- anyToken

--   case x ++ y of
--     "</" -> html: put state (-1) --nesting
--     "<alpha"-> html: put state (+1) 
--     _ -> continue




"/>" <|> try (innerHTML >> endTag) <|> try (innerHTML >> anyFuckingStartingTag)

in the first case, any looking for innerHTML should throw error 

