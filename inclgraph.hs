{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (FilePath)
import Control.Applicative
import Control.Monad
import Data.Maybe
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
import qualified Filesystem.Path.CurrentOS as Path
import qualified Data.Graph.Inductive.Graph as G
import Data.Graph.Inductive.PatriciaTree (Gr)
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

includeLinesFromFilePath :: FilePath -> IO [Include]
includeLinesFromFilePath fp =
    fmap includeLinesFromContents $ TIO.readFile $ Path.encodeString $ fp

--------------------------------------------------------------------------------
-- GRAPH BUILDING
--------------------------------------------------------------------------------

newtype IncludeNode = IncludeNode (FilePath, [FilePath])
    deriving (Show, Eq, Ord)

newtype IncludeGraph = IncludeGraph [IncludeNode]
    deriving (Show, Eq, Ord)

emptyIncludeGraph = IncludeGraph []

{-

addSourceFileToIncludeGraph :: IncludeGraph -> [Path.FilePath] -> Path.FilePath -> IO IncludeGraph
addSourceFileToIncludeGraph graph searchpaths filepath =
    let includelines = 

includeGraphFromFilename' :: IncludeGraph -> [Path.FilePath] -> Path.FilePath -> IO IncludeGraph
includeGraphFromFilename' graph@(IncludeGraph adjlist) fp =
    let encoded_fp = Path.encodeString fp in do
        file_exists <- doesFileExist encoded_fp
        if file_exists then do
            --putStrLn encoded_fp
            content <- TIO.readFile encoded_fp
            nodes <- return $ includeNodesFromFileContents fp content
            let new_graph = IncludeGraph $ (pathToSourceFileNode fp, nodes):adjlist in
                includeGraphFromNodes' new_graph nodes
        else
            --putStrLn ("Warning: " ++ encoded_fp ++ " not found") >> return graph
            putStrLn ("Warning: " ++ encoded_fp ++ " not found, adding anyway") >> return (IncludeGraph $ (pathToSourceFileNode fp, []):adjlist)

includeGraphFromNodes' :: IncludeGraph -> [IncludeNode] -> IO IncludeGraph
includeGraphFromNodes' graph [] = return graph
includeGraphFromNodes' graph@(IncludeGraph adjlist) (n:ns) =
    let action =
            case (L.lookup n adjlist, n) of
                (Just _, _) -> return graph
                (Nothing, SourceFileNode fp) -> includeGraphFromFilename' graph fp
                (Nothing, SystemHeaderNode _) -> return $ IncludeGraph adjlist -- $ IncludeGraph $ (n, []):graph_edges
                --(Nothing, SystemHeaderNode _) -> return $ IncludeGraph $ (n, []):adjlist
    in action >>= (flip includeGraphFromNodes' ns)

includeGraphFromNodes :: [IncludeNode] -> IO IncludeGraph
includeGraphFromNodes = liftM normalisePaths . includeGraphFromNodes' (IncludeGraph [])

normalisePathInNode :: Path.FilePath -> IncludeNode -> IncludeNode
normalisePathInNode prefix node@(SystemHeaderNode _) = node
normalisePathInNode prefix node@(SourceFileNode fp) =
    case Path.stripPrefix prefix fp of
        Nothing -> node ---trace (show (prefix, fp, node)) node
        Just x -> SourceFileNode x

filePathFromNode (SystemHeaderNode _) = Nothing
filePathFromNode (SourceFileNode fp) = Just fp

normalisePaths :: IncludeGraph -> IncludeGraph
normalisePaths (IncludeGraph adjlist) =
    let fps = Path.commonPrefix $ catMaybes $ map (filePathFromNode . fst) adjlist
        norm = normalisePathInNode fps in
    IncludeGraph [(norm a,[norm b | b <- l]) | (a,l) <- adjlist]

--------------------------------------------------------------------------------
-- GraphViz
--------------------------------------------------------------------------------

includeGraphToVerticesEdges :: IncludeGraph -> Gr IncludeNode () --([G.LNode IncludeNode], [G.UEdge])
includeGraphToVerticesEdges (IncludeGraph l) =
    let lgraph_tmp = [(i,n,children) | (i,(n,children)) <- zip [1..] l]
        lgraph_map = L.foldl' (\m (i,n,children) -> Map.insert n i m) Map.empty lgraph_tmp
        lgraph = [((i,n),[(fromJust j,c) | c <- children, let j = Map.lookup c lgraph_map, isJust j]) | (i,n,children) <- lgraph_tmp]
        lvertices = map fst lgraph
        ledges = [(i,j,()) | ((i,n),jl) <- lgraph, (j,_) <- jl] in
    G.mkGraph lvertices ledges

formatNode (n, SystemHeaderNode t) = [GraphViz.Attributes.textLabel (TL.snoc (TL.cons '<' $ TL.fromStrict t) '>')]
--formatNode (n, SourceFileNode fp) = [GraphViz.Attributes.textLabel (TL.snoc (TL.cons '"' $ TL.fromStrict $ Path.encode fp) '"')]
formatNode (n, SourceFileNode fp) = [GraphViz.Attributes.textLabel $ TL.fromStrict $ Path.encode fp]

includeGraphToDot :: IncludeGraph -> GraphViz.DotGraph G.Node
includeGraphToDot graph =
    GraphViz.graphToDot params $ includeGraphToVerticesEdges graph
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

generateGraphVizFromFilenames :: [String] -> IO ()
generateGraphVizFromFilenames fnames = do
    inclgraph <- (includeGraphFromNodes . map (pathToSourceFileNode . Path.decodeString)) fnames
    void $ GraphViz.Commands.runGraphvizCommand GraphViz.Commands.Dot (includeGraphToDot $ normalisePaths inclgraph) GraphViz.Commands.Pdf "output.pdf"

main = do
    args <- getArgs
    case getOpt Permute options args of
        (o,n,[]) -> generateGraphVizFromFilenames n
        (_,_,errs) -> ioError (userError $ concat errs ++ usageInfo header options)
    where header = "Usage: inclgraph [OPTIONS...] files"

-}
