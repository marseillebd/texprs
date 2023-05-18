{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import Control.Applicative (Alternative(..),(<**>))
import Control.Monad (forM,forM_)
import Data.Char (ord)
import Data.Functor ((<&>))
import Data.List (intercalate,isSuffixOf)
import Data.Texpr (Texprs,Texpr(..),ctorNameToString)
import Options.Applicative (bashCompleter,completer)
import Options.Applicative (metavar,help,short,long)
import Options.Applicative (Parser,ParserInfo,execParser,info)
import Options.Applicative (progDesc,fullDesc,header,helper)
import Options.Applicative (switch,strArgument)
import System.Exit (exitFailure)
import System.IO (hPutStrLn,hPrint,stderr)
import Text.Location (Position(..),verbosePos)
import Text.Location.String (startInput)
import Text.Tbnf (CompiledTbnf)

import qualified Data.CharSet as CS
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Texpr as Texpr
import qualified Text.Tbnf.IO as Tbnf
import qualified Text.Tbnf.Read.String as String

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
    <> help "TODO treat input as a serialized texpr rather than a text string"
    )
  isOutputShort <- switch
    (  long "short-output"
    <> short 'V'
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
  <> progDesc
    "Takes an input file from stdin and produces t-exprs on stdout.\n\
    \Parsing is done with the passed files;\n\
    \  files ending in `.tbnf` are TBNF grammars,\n\
    \  those ending in `.trw` are t-expr rewriters (TODO).\n\
    \Input is fed through the parsers/rewriters in order."
  <> header "tbnf - flexible parser system producing t-exprs"
  )

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
          Left (Left err) -> hPrint stderr err.reason >> exitFailure -- TODO print this much more nicely thanks!
          Left (Right err) -> hPrint stderr err >> exitFailure
      | otherwise -> do
        hPutStrLn stderr $ "unrecognized file extension on file " ++ show file
        exitFailure
  inp <- getContents
  let acc0 = StrAcc inp
  r <- forAcc stages acc0 $ \case
    Parse g -> \case
      StrAcc str -> case String.runReader g (startInput str) of
        Right (ts, _) -> pure $ TexprAcc ts
        Left err -> hPrint stderr err.reason >> exitFailure -- TODO print this much more nicely thanks!
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
  let loc = if noLocation then "" else renderLocation (Texpr.start t)
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


renderLocation :: Position -> String
renderLocation p = concat [show p.nChars, ":", show p.line, ":", show p.col]

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
            ["one of the characters " ++ show reason.expectingChars]
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
      , ":\n  unexpected: " ++ unexpectations
      , ":\n  expecting:" ++ expectations
      ]
