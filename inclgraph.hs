{-# LANGUAGE OverloadedStrings #-}

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
import System.Directory (doesFileExist)
import qualified Filesystem.Path.CurrentOS as Path
import qualified Data.Graph.Inductive.Graph as G
import Data.Graph.Inductive.PatriciaTree (Gr)
import qualified Data.GraphViz as GraphViz
import qualified Data.GraphViz.Commands as GraphViz.Commands
import qualified Data.GraphViz.Attributes as GraphViz.Attributes

import Debug.Trace

data IncludeLine =
    SystemHeaderIncludeLine T.Text
    | SourceFileIncludeLine T.Text
    | NotAnIncludeLine ()
    deriving (Show, Eq)

data IncludeNode =
    SystemHeaderNode T.Text
    | SourceFileNode Path.FilePath
    deriving (Show, Eq, Ord)

newtype IncludeGraph = IncludeGraph [(IncludeNode, [IncludeNode])]
    deriving (Show)

notAnIncludeLine :: P.Parser IncludeLine
notAnIncludeLine = liftM NotAnIncludeLine $ P.skipWhile ((/=) '\n') <* (P.endOfLine <|> P.endOfInput)

lineParser :: P.Parser IncludeLine
lineParser = fileIncludeLine <|> headerIncludeLine
    where   fileIncludeLine = liftM SourceFileIncludeLine $ includeLine '"' '"'
            headerIncludeLine = liftM SystemHeaderIncludeLine $ includeLine '<' '>'
            includeLine c1 c2 = P.string "#include" *> P.skipSpace *> (P.char c1 *> P.takeWhile1 (P.notInClass (c2 : "\n")) <* P.char c2) <* (P.endOfLine <|> P.endOfInput)

fileParser :: P.Parser [IncludeLine]
fileParser = P.manyTill (lineParser <|> notAnIncludeLine) P.endOfInput

pathToSourceFileNode :: Path.FilePath -> IncludeNode
pathToSourceFileNode fp =
    SourceFileNode $ Path.collapse fp

includeLineToNode :: Path.FilePath -> IncludeLine -> Maybe IncludeNode
includeLineToNode fp l =
    case l of
        SystemHeaderIncludeLine t -> Just $ SystemHeaderNode $ t
        SourceFileIncludeLine t -> Just $ pathToSourceFileNode (Path.append (Path.directory fp) (Path.fromText t))
        NotAnIncludeLine () -> Nothing

includeNodesFromFileContents :: Path.FilePath -> T.Text -> [IncludeNode]
includeNodesFromFileContents fp a =
    case P.parseOnly (fileParser) a of
        Left s -> error s
        Right l -> catMaybes $ map (includeLineToNode fp) l

includeGraphFromFilename' :: IncludeGraph -> Path.FilePath -> IO IncludeGraph
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
            putStrLn ("Warning: " ++ encoded_fp ++ " not found") >> return graph

includeGraphFromNodes' :: IncludeGraph -> [IncludeNode] -> IO IncludeGraph
includeGraphFromNodes' graph [] = return graph
includeGraphFromNodes' graph@(IncludeGraph adjlist) (n:ns) =
    let action =
            case (L.lookup n adjlist, n) of
                (Just _, _) -> return graph
                (Nothing, SourceFileNode fp) -> includeGraphFromFilename' graph fp
                --(Nothing, SystemHeaderNode _) -> return $ IncludeGraph adjlist -- $ IncludeGraph $ (n, []):graph_edges
                (Nothing, SystemHeaderNode _) -> return $ IncludeGraph $ (n, []):adjlist
    in action >>= (flip includeGraphFromNodes' ns)

includeGraphFromNodes :: [IncludeNode] -> IO IncludeGraph
includeGraphFromNodes = liftM normalisePaths . includeGraphFromNodes' (IncludeGraph [])

normalisePathInNode :: Path.FilePath -> IncludeNode -> IncludeNode
normalisePathInNode prefix node@(SystemHeaderNode _) = node
normalisePathInNode prefix node@(SourceFileNode fp) = SourceFileNode $ fromJust $ Path.stripPrefix prefix fp

filePathFromNode (SystemHeaderNode _) = Nothing
filePathFromNode (SourceFileNode fp) = Just fp

normalisePaths :: IncludeGraph -> IncludeGraph
normalisePaths (IncludeGraph adjlist) =
    let fps = Path.commonPrefix $ catMaybes $ map (filePathFromNode . fst) adjlist
        norm = normalisePathInNode fps in
    IncludeGraph [(norm a,[norm b | b <- l]) | (a,l) <- adjlist]

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

main = do
    inclgraph <- (includeGraphFromNodes . map (pathToSourceFileNode . Path.decodeString)) =<< getArgs
    GraphViz.Commands.runGraphvizCommand GraphViz.Commands.Dot (includeGraphToDot $ normalisePaths inclgraph) GraphViz.Commands.Pdf "output.pdf"


