{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Monad
import qualified Data.Attoparsec.ByteString.Char8 as P
import qualified Data.ByteString.Char8 as C
import System.Environment
import System.FilePath

data Include =
    HeaderInclude C.ByteString
    | FileInclude C.ByteString
    | NotAnInclude ()
    deriving (Show)

newtype IncludeGraph = IncludeGraph [(FilePath, [Include], [Include])]

notAnIncludeLine :: P.Parser Include
notAnIncludeLine = liftM NotAnInclude $ P.skipWhile ((/=) '\n') <* (P.endOfLine <|> P.endOfInput)

lineParser :: P.Parser Include
lineParser = fileIncludeLine <|> headerIncludeLine
    where   fileIncludeLine = liftM FileInclude $ includeLine '"' '"'
            headerIncludeLine = liftM HeaderInclude $ includeLine '<' '>'
            includeLine c1 c2 = P.string "#include" *> P.skipSpace *> (P.char c1 *> P.takeWhile1 (P.notInClass (c2 : "\n")) <* P.char c2) <* (P.endOfLine <|> P.endOfInput)

fileParser :: P.Parser [Include]
fileParser = P.manyTill (lineParser <|> notAnIncludeLine) P.endOfInput

isActualInclude :: Include -> Bool
isActualInclude (FileInclude _) = True
isActualInclude (HeaderInclude _) = True
isActualInclude (NotAnInclude _) = False

filterActualIncludes :: [Include] -> [Include]
filterActualIncludes = filter isActualInclude

getIncludesFromCode :: C.ByteString -> [Include]
getIncludesFromCode a =
    case P.parseOnly (fileParser) a of
        Left s -> error s
        Right l -> filterActualIncludes l

getIncludeGraphFromFilename :: FilePath -> IO (FilePath, FilePath, FilePath, [Include])
getIncludeGraphFromFilename filepath = do
    (filename, directory) <- return $ splitFileName filepath
    code <- C.readFile filepath
    includes <- return $ getIncludesFromCode code
    return (filepath, directory, filename, includes)

getIncludeGraphFromFilenames' :: [FilePath] -> IncludeGraph -> IncludeGraph
getIncludeGraphFromFilenames' filepaths graph = undefined

main = do
    args <- getArgs
    getIncludeGraphFromFilename (args !! 0) >>= (return . show) >>= putStrLn
