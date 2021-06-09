import Data.List
import Data.Char
import Data.Binary
import System.Environment
import System.Directory
import System.FilePath.Posix
import System.IO

import Utils (saveTree, addAppendix, binToCharList, charListToBin, removeAppendix, loadTree)
import HuffmanTree (encodeText, decodeText, buildTree, HuffmanTree)

main :: IO ()
main = do
  args <- getArgs
  let originFile = args !! 1
  let destinyFile = args !! 2
  fileExists <- doesFileExist originFile
  dirExists <- doesDirectoryExist originFile
  destinyExists <- doesDirectoryExist $ takeDirectory destinyFile
  if length args /= 3 then error "Missing arguments: usage ./hs-huffman compress/uncompress filePath fileDestiny"
  else if not destinyExists then error "Destiny path not exist."
  else if and [(not fileExists), (not destinyExists)] then error "File not found."
  else
    case head args of
      "compress" -> compress originFile destinyFile
      "uncompress" -> uncompress originFile destinyFile
      _ -> putStrLn "Error: first argument must be compress/uncompress"

-- compressing
-- load file -> build tree -> save tree -> encode text -> add appendix to bin -> convert bin to charlist -> save file

compress :: String -> String -> IO ()
compress origin destiny = do
  content <- readFile origin
  let tree = buildTree content
  createDirectory destiny
  saveTree (destiny++"/tree.bin") tree
  let encodedContent = binToCharList $ addAppendix $ encodeText content tree
  encodeFile (destiny++"/compressed.bin") encodedContent
  putStrLn "Compressed File"

-- decompressing
-- load file -> load tree -> convert char to bin -> remove appendix -> decode text -> save file

uncompress :: String -> String -> IO ()
uncompress origin destiny = do
  content <- decodeFile (origin++"/compressed.bin")
  tree <- loadTree (origin++"/tree.bin")
  let decodedContent = decodeText (removeAppendix $ charListToBin content) tree
  putStrLn decodedContent
  writeFile destiny decodedContent
