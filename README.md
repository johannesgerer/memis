# memis: efficient manual image sorting

*memis* is a little tool written in Haskell. It allows to efficiently
rename and sort image files into directories, via a keyboard
controlled web-interface.

## Installation

```shell
cabal update         #optional
cabal sandbox init   #optional

cabal install
```

## Usage

First start the server

```shell
memis [source folder with images] [target folder]
```

and then navigate to [`http://localhost:8081`](http://localhost:8081).
