streaming-fusion
====

streaming-fusion offers an api that is similar to the streaming library, with performance at par with vector and streamly.
It doesn't sacrifice the streaming characteristic to achieve this performance. This sample program from michaelt illustrates this:
```Haskell
>>> wrapped a = "       " ++ a
>>> :{
        S.printStream 
        $ S.map wrapped
        $ intercalates (S.yield "------") 
        $ S.group 
        $ S.take 5 S.stdinLn
  :}
1
"       1"
2
"       ------"
"       2"
2
"       2"
3
"       ------"
"       3"
1
"       ------"
"       1"
"       ------"
```
This above program doesn't accumulate, it is a streaming one. The stream type could be thought of as a generalisation of vector / streamly's Stream type with a functor layer. It is defined in `StreamingD` module. 

The implementation is heavily adapted from streamly, and vector. The library can be easily benchmarked using [streaming-benchmarks](https://github.com/composewell/streaming-benchmarks).
