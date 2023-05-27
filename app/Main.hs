{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Prelude hiding (init)

import Control.Applicative (Alternative(..),(<**>))
import Control.Monad (forM,forM_)
import Data.Char (ord,isDigit,isAscii,isAlpha,isAlphaNum)
import Data.Functor ((<&>))
import Data.List (intercalate,isSuffixOf)
import Data.Texpr (Texprs,Texpr(..),CtorName,ctorNameToString,ctorNameFromString)
import Options.Applicative (bashCompleter,completer)
import Options.Applicative (metavar,help,short,long)
import Options.Applicative (Parser,ParserInfo,execParser,info)
import Options.Applicative (progDescDoc,fullDesc,header,helper)
import Options.Applicative (switch,strArgument)
import System.Exit (exitFailure)
import System.IO (hPutStrLn,hPrint,stderr)
import Text.Location (Position(..),FwdRange,maybeFwd,verbosePos)
import Text.Location.String (startInput)
import Text.ParserCombinators.ReadP (ReadP,readP_to_S)
import Text.Tbnf (CompiledTbnf)

import qualified Data.CharSet as CS
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Prettyprinter as PP
import qualified Text.ParserCombinators.ReadP as Read
import qualified Text.Tbnf.IO as Tbnf
import qualified Text.Tbnf.Read.String as String
import qualified Text.Tbnf.Read.Texpr as Texpr

data Options = Options
  { isInputTexpr :: Bool
  , stageFiles :: [FilePath]
  , isOutputShort :: Bool
  }
  deriving (Show)

parseOpts :: Parser Options
parseOpts = do
  isInputTexpr <- switch
    (  long "read-texpr"
    <> short 't'
    <> help "treat input as a serialized texpr rather than a text string"
    )
  isOutputShort <- switch
    (  long "short-output"
    <> short 's'
    <> help "omit location information from the output texpr"
    )
  stageFiles <- some $ strArgument
    (  metavar "FILES..."
    <> completer (bashCompleter "file")
    )
  pure $ Options
    { isInputTexpr
    , stageFiles
    , isOutputShort
    }

optParser :: ParserInfo Options
optParser = info (parseOpts <**> helper)
  (  fullDesc
  <> progDescDoc (Just doc)
    -- "Takes an input file from stdin and produces t-exprs on stdout.\n\n\
    -- \Parsing is done with the passed files;\n\
    -- \  files ending in `.tbnf` are TBNF grammars,\n\
    -- \  files ending in `.tbnf-ill` are illiterate TBNF grammars,\n\
    -- \  those ending in `.trwl` are t-expr rewriters (TODO).\n\
    -- \Input is fed through the parsers/rewriters in order."
  <> header "tbnf - flexible parser system producing t-exprs"
  )
  where
  doc = PP.vsep
    [ PP.sep
      [ "Takes an input file from stdin and produces t-exprs on stdout."
      , "The reader pipeline specified by FILES, performed in-order"
      , "The list of recognized file extensions is:"
      ]
    , PP.indent 2 $ PP.vsep
      [ "- `.tbnf`: TBNF grammars"
      , "- `.trwl` are t-expr rewriters (TODO)"
      , PP.hang 4 $ PP.sep
        [ "- `.tbnf-ill` are illiterate TBNF grammars"
        , "These are useful for prototyping, where one may not want to write birds feet everywhere."
        ]
      ]
    ]

data Stage
  = Parse CompiledTbnf

data Acc
  = StrAcc String
  | TexprAcc Texprs

main :: IO ()
main = do
  opts <- execParser optParser
  stages <- forM opts.stageFiles $ \case
    file
      | ".tbnf" `isSuffixOf` file -> do
        Tbnf.readFile file >>= \case
          Right g -> pure $ Parse g
          Left (Left err) ->
            hPutStrLn stderr (renderError (Just file) err.reason) >> exitFailure
          Left (Right err) -> hPrint stderr err >> exitFailure -- TODO print this much more nicely thanks!
      | ".tbnf-ill" `isSuffixOf` file -> do
        (Tbnf.fromString . unlines . map ("> " <>) . lines) <$> readFile file >>= \case
          Right g -> pure $ Parse g
          Left (Left err) ->
            hPutStrLn stderr (renderError (Just file) err.reason) >> exitFailure
          Left (Right err) -> hPrint stderr err >> exitFailure -- TODO print this much more nicely thanks!
      | otherwise -> do
        hPutStrLn stderr $ "unrecognized file extension on file " ++ show file
        exitFailure
  acc0 <- if opts.isInputTexpr
    then TexprAcc <$> do
      inp <- getContents
      case filter (null . snd) $ readP_to_S readTexprs inp of
        ((ts, ""):_) -> pure ts
        other -> hPutStrLn stderr "input texpr is corrupt" >> hPrint stderr other >> exitFailure
    else StrAcc <$> getContents
  r <- forAcc stages acc0 $ \case
    Parse g -> \case
      StrAcc str -> case String.runReader g (startInput str) of
        Right (ts, _) -> pure $ TexprAcc ts
        Left err -> hPutStrLn stderr (renderError Nothing err.reason) >> exitFailure
      TexprAcc ts -> case Texpr.runReader g ts of
        Right (ts', _) -> pure $ TexprAcc ts'
        Left err -> hPutStrLn stderr (renderError Nothing err.reason) >> exitFailure
  ts <- case r of
    StrAcc _ -> error "tbnf pipeline failed to produce texprs"
    TexprAcc ts -> pure ts
  forM_ ts $ putStrLn . render opts.isOutputShort (Just 1)

forAcc :: (Monad m) => [a] -> b -> (a -> b -> m b) -> m b
forAcc [] z _ = pure z
forAcc (x:xs) z f = do
  z' <- f x z
  forAcc xs z' f

-- TODO some real pretty printing, but this'll do for now
render ::
     Bool -- ^ supress location
  -> Maybe Int -- ^ indentation depth, or disable indentation
  -> Texpr -> String
render noLocation depth t =
  let loc = if noLocation then "" else renderLoc t.loc
      (indent, depth') = case depth of
        Nothing -> (" ", Nothing)
        Just i -> ('\n' : replicate (2*i) ' ', Just (i + 1))
   in case t of
    Atom _ str -> concat
      [loc, renderLeaf str]
    Combo _ ctor [] -> concat
      [ loc, "(", ctorNameToString ctor, ")" ]
    Combo _ ctor [child@(Atom _ _)] -> concat
      [ loc, "(", ctorNameToString ctor, " ", render noLocation depth' child, ")" ]
    Combo _ ctor children -> concat
      [ loc
      , "("
      , ctorNameToString ctor
      , concatMap (indent++) (render noLocation depth' <$> children)
      , ")"
      ]

renderLoc :: FwdRange -> String
renderLoc l = concat [renderPos l.anchor, "-", renderPos l.position]
  where
  renderPos :: Position -> String
  renderPos p = concat [show p.nChars, ":", show p.line, ":", show p.col]

renderLeaf :: String -> String
renderLeaf str = concat ["\"", concatMap renderChar str, "\""]
  where
  renderChar :: Char -> String
  renderChar '\n' = "\\n"
  renderChar '\"' = "\\\""
  renderChar '\\' = "\\\\"
  renderChar '\DEL' = "\\DEL"
  renderChar c
    | '\NUL' <= c && c <= '\US' = '\\' : (iso2047 !! ord c)
    | otherwise = [c]

iso2047 :: [String]
iso2047 =
  [ "NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "BEL"
  , "BS",  "HT",  "LF",  "VT",  "FF",  "CR",  "SO",  "SI"
  , "DLE", "DC1", "DC2", "DC3", "DC4", "NAK", "SYN", "ETB"
  , "CAN", "EM",  "SUB", "ESC", "FS",  "GS",  "RS",  "US"
  ]

-- TODO use a pretty-printer
renderError ::
     Maybe FilePath
  -> String.Reason
  -> String
renderError fileName reason =
  let locLine = concat
        [ "Read error"
        , case fileName of
            Nothing -> " at "
            Just it -> " in file \'" ++ it ++ "\', "
        , verbosePos reason.expectAt
        ]
      expectations = intercalate ", " $ concat
        [ Map.assocs reason.expectingByName <&> \(name, _) -> name -- TODO show subreason
        , if CS.null reason.expectingChars then [] else
            ["one of the characters " ++ CS.render reason.expectingChars]
        , if null reason.expectingKeywords then [] else
            [  "one of the strings "
            ++ intercalate ", " (fmap show $ Set.toList reason.expectingKeywords)
            ]
        , if null reason.expectingCtors then [] else
            [  "a texpr with one of the constructors "
            ++ intercalate ", " (ctorNameToString <$> Set.toList reason.expectingCtors)
            ]
        , if not reason.expectingEndOfInput then []
            else ["end of input"]
        ]
      unexpectations = if Set.null reason.unexpected then ""
        else intercalate ", " (fmap show $ Set.toList reason.unexpected)
   in concat
      [ locLine
      , ":"
      , if null unexpectations then "" else "\n  unexpected: " ++ unexpectations
      , "\n  expecting: " ++ expectations
      ]

readTexprs :: ReadP [Texpr]
readTexprs = do
  readSpace
  readTexpr `Read.endBy` readSpace

readTexpr :: ReadP Texpr
readTexpr = do
  loc <- readLoc
  ($ loc) <$> (readCombo Read.<++ readAtom)
  where
  readLoc :: ReadP FwdRange
  readLoc = do
    start <- readPos
    Read.char '-'
    end <- readPos
    maybe Read.pfail pure $ maybeFwd start end
  readPos :: ReadP Position
  readPos = do
    nChars <- read <$> Read.munch1 isDigit
    Read.char ':'
    line <- read <$> Read.munch1 isDigit
    Read.char ':'
    col <- read <$> Read.munch1 isDigit
    pure $ Pos {nChars,line,col}
  readAtom :: ReadP (FwdRange -> Texpr)
  readAtom = do
    Read.char '\"'
    str <- Read.many $ Read.choice
      ( ( Read.satisfy (\c -> ' ' <= c && c /= '\DEL') )
      : ( '\n' <$ Read.string "\\n" )
      : ( '\"' <$ Read.string "\\\"" )
      : ( '\\' <$ Read.string "\\\\" )
      : ( '\DEL' <$ Read.string "\DEL" )
      : ( flip map (zip ['\0'..] iso2047) $ \(c, name) ->
          c <$ Read.string ('\\':name) )
      )
    Read.char '\"'
    pure $ \loc -> Atom loc str
  readCombo :: ReadP (FwdRange -> Texpr)
  readCombo = do
    Read.char '('
    readSpace
    ctor <- readCtor
    children <- Read.many (readSpace1 >> readTexpr)
    readSpace
    Read.char ')'
    pure $ \loc -> Combo loc ctor children
  readCtor :: ReadP CtorName
  readCtor = do
    init <- Read.satisfy (\c -> isAscii c && (isAlpha c || c == '_'))
    cont <- Read.munch (\c -> isAscii c && (isAlphaNum c || c == '_'))
    let internalErr = error "internal error deserializing texpr"
    maybe internalErr pure $ ctorNameFromString (init:cont)

readSpace :: ReadP ()
readSpace = () <$ Read.munch (\c -> c `elem` (" \n\t\r" :: String))
readSpace1 :: ReadP ()
readSpace1 = () <$ Read.munch1 (\c -> c `elem` (" \n\t\r" :: String))
