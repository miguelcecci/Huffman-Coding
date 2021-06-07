import Data.List
import Data.Char
import Data.Binary
import System.Environment
import System.Directory

import Utils
import HuffmanTree (encodeText, decodeText, buildTree, HuffmanTree)

main :: IO ()
main = do
  args <- getArgs
  fileExists <- doesFileExist $ head $ tail args
  if not $ fileExists then
    error "File not found"
  else
    case head args of
      "compress" -> putStrLn "compress"
      "uncompress" -> putStrLn "uncompress"
      _ -> putStrLn "usage arg1:compress/uncompress arg2:filePath"
--
-- examples 

exp_text :: String
exp_text = "Integer euismod ac turpis ut feugiat. Duis luctus odio turpis, a ornare metus fermentum sit amet. Vivamus viverra hendrerit dictum. Vivamus porta, leo et mattis pharetra, augue nunc finibus velit, id sollicitudin orci odio ultrices augue. Nam libero ligula, convallis sed suscipit nec, fermentum sed nunc. Etiam justo eros, rutrum sit amet libero eget, euismod rutrum dui. Duis ut nisi non orci finibus faucibus. Aliquam risus lorem, bibendum et nulla ut, maximus blandit nibh. Nam eget aliquam metus. Vestibulum vel ante non tellus faucibus suscipit. Donec nec nulla in urna aliquet mattis in ac mauris. Donec euismod, diam in accumsan blandit, ipsum purus aliquet risus, ut dictum velit sapien luctus enim. Vestibulum eros dui, feugiat sit amet iaculis in, aliquet et justo. Morbi pellentesque bibendum consequat. Aliquam iaculis gravida neque et interdum."

exp_tree :: HuffmanTree
exp_tree = buildTree exp_text 




-- compressing
-- load file -> build tree -> save tree -> encode text -> add appendix to bin -> convert bin to charlist -> save file
--
-- decompressing
-- load file -> load tree -> convert char to bin -> remove appendix -> decode text -> save file

-- compress :: String -> IO ()
-- compress path =

