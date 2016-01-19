mlsp-lang
=========

A programming language work-in-progress. This language will compile to the JVM but will have a functional base and type system inspired by Haskell, with many syntax elements and some object-oriented principles inspired by Ruby. The language is statically typed but infers type signatures in most cases. It uses a special type flag, called a "writable" flag, to indicate whether a particular variable is mutable or not. This allows functionally pure code to be written and verified at compile-time (`(List[$a]) -> List[$a]`) while also allowing mutable algorithms to be easily written, simply by flagging the values as mutable (`(List[$a]!) -> {}`).

The compiler will, when finished, take Ruby-ish syntax and read it using the Haskell parser Parsec. The Haskell program will be responsible for parsing, compiling, and resolving all references, with the end result being assembly-style code which will be finished off by Jasmin, the popular JVM bytecode assembler.

Current Progress
----------------

The language is currently in its very early stages. The parser (written in Haskell using Parsec) is mostly finished, but the compiling and semantic checking are only just mere stubs.

Example Code
------------

The parser is not nearly finished, so the code written below will not currently parse. However, it is a rough idea of what the end goal of the language is.

```
class Matrix[$a]
  @matrixContents :: List[List[$a]]
end

concept AdditiveMonoid
  plus :: ($, $) -> $
end

instance AdditiveMonoid Matrix[$a]
  def plus(x, y) = x.matrixContents + y.matrixContents
end

def main = begin
  matrix1 = Matrix.new [[1, 2], [3, 4]]
  matrix2 = Matrix.new [[1, 0], [0, 1]]
  print matrix1 + matrix2
end
```
