---
title      : "How to run uu-parsinglib without hitting the ground..."
date       : 2016-03-17 12:00:00
categories : [compsci]
tags       : [haskell, parser combinators, uu-parsinglib]
---

*This post is the first in a series of collaborations between
[Stijn van Drongelen][Rhymoid] and me, in which we explore the parser
combinator library [uu-parsinglib][uu-parsinglib] in an effort to make
it more accessible to the general public.*

There's a bit of an odd duck amongst the various Haskell parser
combinator libraries. Its name is uu-parsinglib, and it was developed,
for the most part, by Doaitse Swierstra, profesor emiratus at Utrecht
University, where both Stijn and I happened to take our degree.
The library has some beautiful features, absent in the more popular
libraries like parsec and attoparsec, but it also is rather
bare-bones, focusing on the core parser implementation. Consequently,
it lacks some features which make the likes of parsec more
user-friendly and accessible.

In this series, we will implement several parsers using uu-parsinlib,
explore its various features, and deal with some problems that come up
in practice. Our preliminary planning is:

  - Getting started, parsing CSS Selectors (this post);
  - Parsing JSON with `Data.Text`;
  - Online parsing, returning results as soon as possible;
  - Error reporting;



``` haskell
module HowToHitTheGroundRunning where

import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.BasicInstances
import Text.ParserCombinators.UU.Utils
```

<div class="hidden">
``` haskell
import Numeric
import Control.Applicative
import Data.Char
import Text.Inflections
```
</div>

|-----------------|---------------------------------------------------------------------|
| **Example**     | **Meaning**                                                         |
|:----------------|:--------------------------------------------------------------------|
| E               | an element of type E                                                |
| E:link          | an E element being the source anchor of a hyperlink of which        |
| E:visited       | the target is not yet visited (:link) or already visited (:visited) |
| E:active        | an E element during certain user actions                            |
| E:hover         |                                                                     |
| E:focus         |                                                                     |
| E::first-line   | the first formatted line of an E element                            |
| E::first-letter | the first formatted letter of an E element                          |
| E.warning       | an E element whose class is "warning" (the document                 |
|                 | language specifies how class is determined).                        |
| E#myid          | an E element with ID equal to "myid".                               |
| E F             | an F element descendant of an E element                             |
|-----------------|---------------------------------------------------------------------|
{:style="margin: 1ex auto;"}

(Taken from <https://www.w3.org/TR/css3-selectors/>.)

``` haskell
type Ident = String

data Selector where
  Id            :: Ident -> Selector
  Class         :: Ident -> Selector
  Element       :: Ident -> Selector
  PseudoClass   :: PseudoClass   -> Selector
  PseudoElement :: PseudoElement -> Selector
  And           :: [Selector] -> Selector
  In            :: Selector -> Selector -> Selector
  deriving (Show)

data PseudoClass
   = Link | Visited | Active | Hover | Focus
   deriving (Show,Eq,Bounded,Enum)

data PseudoElement
   = FirstLetter | FirstLine
   deriving (Show,Eq,Bounded,Enum)
```

``` haskell
pSelector :: Parser Selector
pSelector = foldr1 In <$> pSome pSimpleSelector
```

``` haskell
pSimpleSelector :: Parser Selector
pSimpleSelector = lexeme (pIncElem <|> pExcElem)
  where
    pIncElem = And <$> pSome pOther
    pExcElem = And <$> ((:) <$> pElement <*> pMany pOther)
    pOther   = pId <|> pClass <|> pPseudoClass <|> pPseudoElement
```

``` haskell
pId, pClass, pElement :: Parser Selector
pId      = Id      <$> (pSym '#' *> pIdent)
pClass   = Class   <$> (pSym '.' *> pIdent)
pElement = Element <$> pIdent
```

``` haskell
pPseudoClass :: Parser Selector
pPseudoClass = PseudoClass <$> pAny mkParser [minBound .. maxBound]
  where
    mkParser x = pToken (":"++ toDashed (show x)) *> pure x
```

<div class="hidden">
``` haskell
pPseudoElement :: Parser Selector
pPseudoElement = PseudoElement <$> pAny mkParser [minBound .. maxBound]
  where
    mkParser x = pToken ("::"++ toDashed (show x)) *> pure x
```
</div>

    h        [0-9a-f]
    nonascii [\240-\377]
    unicode  \\{h}{1,6}(\r\n|[ \t\r\n\f])?
    escape   {unicode}|\\[^\r\n\f0-9a-f]
    nmstart  [_a-z]|{nonascii}|{escape}
    nmchar   [_a-z0-9-]|{nonascii}|{escape}
    ident    -?{nmstart}{nmchar}*

(Taken from <https://www.w3.org/TR/CSS21/grammar.html>.)

<div class="hidden">
``` haskell
pBackslash :: Parser Char
pBackslash = pSym '\\' --- comment intentionally left blank.
```
</div>

``` haskell
pHexDigit :: Parser Char
pHexDigit = pSatisfy isHexDigit (Insertion "[0-9a-f]" '0' 5)

pNonAscii :: Parser Char
pNonAscii = pRange (chr 160, maxBound) -- parses [\240-\4177777]
```

``` haskell
pUnicode :: Parser Char
pUnicode = decodeUTF8 <$> (pBackslash *> pBetween 1 6 pHexDigit <* pOptSpace)
  where
    decodeUTF8 :: String -> Char
    decodeUTF8 str = let [(hex,_)] = readHex str in chr hex
    pOptSpace :: Parser ()
    pOptSpace = () <$ pToken "\r\n" <|> () <$ pAnySym " \t\r\n\f" `opt` ()
```

``` haskell
pEscape :: Parser Char
pEscape = pUnicode <|> (pBackslash *> pNotHexDigitOrSpace)
  where
    pNotHexDigitOrSpace = pSatisfy
      (\x -> not (isHexDigit x || isSpace x))
      (Insertion "[^0-9a-f\\s]" 'y' 5)
```

``` haskell
pNameStart, pNameChar :: Parser Char
pNameStart = pSym '_' <|> pLetter <|> pNonAscii <|> pEscape
pNameChar  = pSym '-' <|> pDigit <|> pNameStart

pIdent :: Parser String
pIdent = pOptPrefix ((:) <$> pSym '-') ((:) <$> pNameStart <*> pMany pNameChar)
  where
    pOptPrefix p q = must_be_non_empty "pOptPrefix" p ((p `opt` id) <*> q)
```

---

[Rhymoid]: https://github.com/rhymoid
[uu-parsinglib]: https://hackage.haskell.org/package/uu-parsinglib
