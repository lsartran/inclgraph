{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (FilePath)
import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Foldable (foldlM)
import qualified Data.Map as Map
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import qualified Data.Attoparsec.Text as P
import System.Environment (getArgs)
import System.Directory (doesFileExist, getCurrentDirectory)
import System.Console.GetOpt
import System.IO.Error (catchIOError, isDoesNotExistError)
import qualified Filesystem.Path.CurrentOS as Path
import qualified Data.Graph.Inductive.Graph as G
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Query.ArtPoint
import qualified Data.GraphViz as GraphViz
import qualified Data.GraphViz.Commands as GraphViz.Commands
import qualified Data.GraphViz.Attributes as GraphViz.Attributes

-- import Debug.Trace

--------------------------------------------------------------------------------
-- PARSING
--------------------------------------------------------------------------------

type FilePath = Path.FilePath

data IncludeLine =
    SystemHeaderIncludeLine T.Text
    | SourceFileIncludeLine T.Text
    | NotAIncludeLine ()
    deriving (Show, Eq)

data Include =
    SystemHeaderInclude FilePath
    | SourceFileInclude FilePath
    deriving (Show, Eq)

filePathFromInclude (SystemHeaderInclude fp) = fp
filePathFromInclude (SourceFileInclude fp) = fp

notAIncludeLine :: P.Parser IncludeLine
notAIncludeLine = liftM NotAIncludeLine $ P.skipWhile ((/=) '\n') <* (P.endOfLine <|> P.endOfInput)

lineParser :: P.Parser IncludeLine
lineParser = fileIncludeLine <|> headerIncludeLine
    where   fileIncludeLine = liftM SourceFileIncludeLine $ includeLine '"' '"'
            headerIncludeLine = liftM SystemHeaderIncludeLine $ includeLine '<' '>'
            includeLine c1 c2 = P.string "#include" *> P.skipSpace *> (P.char c1 *> P.takeWhile1 (P.notInClass (c2 : "\n")) <* P.char c2) <* (P.endOfLine <|> P.endOfInput)

fileParser :: P.Parser [IncludeLine]
fileParser = P.manyTill (lineParser <|> notAIncludeLine) P.endOfInput

includeLines :: IncludeLine -> Maybe Include
includeLines r =
    case r of
        SystemHeaderIncludeLine t -> Just $ SystemHeaderInclude $ Path.fromText $ t
        SourceFileIncludeLine t -> Just $ SourceFileInclude $ Path.fromText $ t -- Path.collapse $ pathToSourceFileNode (Path.append (Path.directory fp) (Path.fromText t))
        NotAIncludeLine () -> Nothing

includeLinesFromContents :: T.Text -> [Include]
includeLinesFromContents content =
    case P.parseOnly (fileParser) content of
        Left s -> error s
        Right l -> catMaybes $ map includeLines l

--------------------------------------------------------------------------------
-- INCLUSIONS PROCESSING
--------------------------------------------------------------------------------

-- Here, fp must point to a file that exists
includeLinesFromFilePath :: FilePath -> IO [Include]
includeLinesFromFilePath fp =
    fmap includeLinesFromContents $ TIO.readFile $ Path.encodeString $ fp
{-
        where
            f name = catchIOError (g name) (\e -> if isDoesNotExistError e then return Nothing else ioError e) where
            g name = do
                cnt <- TIO.readFile name
                return $ Just cnt
-}

findIncludeInDir :: FilePath -> Include -> IO (Maybe FilePath)
findIncludeInDir includePath include =
    let fp = Path.collapse $ Path.append includePath (filePathFromInclude include) in
        do
            exists <- doesFileExist (Path.encodeString fp)
            case exists of
                True -> return (Just fp)
                False -> do
                    return Nothing

findInclude :: [FilePath] -> FilePath -> Include -> IO (Maybe FilePath)
findInclude includePaths filePath include =
    let actualIncludePaths = (Path.directory filePath):includePaths in
        do
            foundFiles <- mapM ((flip findIncludeInDir) include) actualIncludePaths
            case catMaybes foundFiles of
                [] -> do
                    putStrLn $ "Warning: couldn't find " ++ (show include) ++ " included from " ++ (show filePath)
                    return Nothing
                x:xs -> return $ Just x

includedFilesFromFilePath :: [FilePath] -> FilePath -> IO [FilePath]
includedFilesFromFilePath includePaths filePath = do
    includes <- includeLinesFromFilePath filePath
    maybeIncludedFiles <- mapM (findInclude includePaths filePath) includes
    return $ catMaybes maybeIncludedFiles

--------------------------------------------------------------------------------
-- GRAPH BUILDING
--------------------------------------------------------------------------------

newtype IncludeNode = IncludeNode (FilePath,[FilePath])
    deriving (Show, Eq, Ord)

newtype IncludeGraph = IncludeGraph [IncludeNode]
    deriving (Show, Eq, Ord)

emptyGraph = IncludeGraph []

isInGraph :: FilePath -> IncludeGraph -> Bool
isInGraph fp (IncludeGraph nodes) =
    isJust $ L.lookup fp [(fp, includedFiles) | IncludeNode (fp, includedFiles) <- nodes]

addToGraph :: FilePath -> [FilePath] -> IncludeGraph -> IncludeGraph
addToGraph fp included (IncludeGraph nodes) =
    IncludeGraph $ (IncludeNode (fp,included)):nodes

buildGraphFromFile' :: [FilePath] -> FilePath -> IncludeGraph -> IO IncludeGraph
buildGraphFromFile' includePaths sourceFile graph =
    case (isInGraph sourceFile graph) of
        True -> return graph -- nothing to do
        False -> do
            includedFiles <- includedFilesFromFilePath includePaths sourceFile
            buildGraphFromFiles' includePaths includedFiles (addToGraph sourceFile includedFiles graph)

buildGraphFromFiles' :: [FilePath] -> [FilePath] -> IncludeGraph -> IO IncludeGraph
buildGraphFromFiles' includePaths sourceFiles graph = foldlM (flip (buildGraphFromFile' includePaths)) graph sourceFiles

normalisePathInNode :: Path.FilePath -> Path.FilePath -> Path.FilePath
normalisePathInNode prefix fp =
    case Path.stripPrefix prefix fp of
        Nothing -> fp ---trace (show (prefix, fp, node)) node
        Just x -> x

normalisePaths :: IncludeGraph -> IncludeGraph
normalisePaths (IncludeGraph nodes) =
    let fps = Path.commonPrefix [ fp | IncludeNode (fp,_) <- nodes ]
        norm = normalisePathInNode fps in
    IncludeGraph [IncludeNode (norm a,[norm b | b <- l]) | IncludeNode (a,l) <- nodes]

buildGraphFromFiles includePaths sourceFiles = normalisePaths <$> buildGraphFromFiles' includePaths sourceFiles emptyGraph

--------------------------------------------------------------------------------
-- GraphViz
--------------------------------------------------------------------------------

includeGraphToVerticesEdges :: IncludeGraph -> Gr Path.FilePath () --([G.LNode IncludeNode], [G.UEdge])
includeGraphToVerticesEdges (IncludeGraph l) =
    let lgraph_tmp = [(i,n,children) | (i,IncludeNode (n,children)) <- zip [1..] l]
        lgraph_map = L.foldl' (\m (i,n,children) -> Map.insert n i m) Map.empty lgraph_tmp
        lgraph = [((i,n),[(fromJust j,c) | c <- children, let j = Map.lookup c lgraph_map, isJust j]) | (i,n,children) <- lgraph_tmp]
        lvertices = map fst lgraph
        ledges = [(i,j,()) | ((i,n),jl) <- lgraph, (j,_) <- jl] in
    G.mkGraph lvertices ledges

--formatNode (n, SystemHeaderNode t) = [GraphViz.Attributes.textLabel (TL.snoc (TL.cons '<' $ TL.fromStrict t) '>')]
--formatNode (n, SourceFileNode fp) = [GraphViz.Attributes.textLabel (TL.snoc (TL.cons '"' $ TL.fromStrict $ Path.encode fp) '"')]
formatNode (n, fp) = [GraphViz.Attributes.textLabel $ TL.fromStrict $ Path.encode fp]

includeGraphToDot :: Gr Path.FilePath () -> GraphViz.DotGraph G.Node
includeGraphToDot graph =
    GraphViz.graphToDot params graph
    where
        params = GraphViz.nonClusteredParams { GraphViz.fmtNode = formatNode }

--------------------------------------------------------------------------------
-- command line interface
--------------------------------------------------------------------------------

data CmdlineFlag = Help | OutputFile String | IncludeDir String deriving (Show, Eq)

options :: [OptDescr CmdlineFlag]
options =
    [
        Option  ['h']   ["help"]    (NoArg Help)                "Display help message",
        Option  ['I']   []          (ReqArg IncludeDir "DIR")   "Add DIR to include search path",
        Option  ['o']   ["output"]  (OptArg outp "FILE")        "Write output to FILE"
    ]

outp = OutputFile . fromMaybe "output.pdf"

generateGraphVizFromFilenames :: [String] -> [String] -> IO ()
generateGraphVizFromFilenames includeDirs fnames = do
    inclgraph <- buildGraphFromFiles (map Path.decodeString includeDirs) (map Path.decodeString fnames)
    void $ GraphViz.Commands.runGraphvizCommand GraphViz.Commands.Dot (includeGraphToDot $ includeGraphToVerticesEdges inclgraph) GraphViz.Commands.Pdf "output.pdf"

getIncludes [] = []
getIncludes ((IncludeDir dir):opts) = dir:(getIncludes opts)
getIncludes ((_):opts) = (getIncludes opts)

main = do
    args <- getArgs
    case getOpt Permute options args of
        (o,n,[]) -> do
            --putStrLn $ show o
            generateGraphVizFromFilenames (getIncludes o) n
        (_,_,errs) -> ioError (userError $ concat errs ++ usageInfo header options)
    where header = "Usage: inclgraph [OPTIONS...] files"
