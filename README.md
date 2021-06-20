## Huffman Coding

Haskell implementation of huffman coding algorithm for lossless compresssion.

executable location: Huffman-Coding/dist/build/hs-huffman/hs-huffman

## Executing

Compressing:
```
$ hs-huffman compress path_to_file/file_name path_to_destination/destination_name
```

Uncompressing:
```
$ hs-huffman uncompress path_to_file/file_name path_to_destination/destination_name
```

## Benchmark

Tested in lorem Ipsum generated files

![alt text](https://github.com/miguelcecci/Huffman-Coding/blob/master/benchmarks/plot/img_plots.png?raw=true)

## Compressed files

After running the command for compression, a directory will be generated containing 2 files, one is the compressed file, and the other is the huffman tree geneated, wich is used to uncompress the file.


