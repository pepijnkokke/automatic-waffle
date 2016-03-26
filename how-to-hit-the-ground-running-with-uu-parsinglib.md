---
title      : "How to Hit the Ground Running with uu-parsinglib"
date       : 2016-03-17 12:00:00
categories : [compsci]
tags       : [haskell, parser combinators]
---

Boop. Boop. Boop. Boop.

{% highlight haskell %}
module HowToHitTheGroundRunning where

import Text.Inflections (toDashed)
import Text.ParserCombinators.UU (pAny)
import Text.ParserCombinators.UU.BasicInstances (Parser,pRange)
import Text.ParserCombinators.UU.Utils (runParser,pSymbol,pAnySym)
{% endhighlight %}

<div class="hidden">
{% highlight haskell %}
import System.Exit (exitFailure)
{% endhighlight %}
</div>

|-----------------|-----------------|-------------------------------------------------|
| **Selector**    |**Example**      | **Example description**                         |
|:----------------|:----------------|:------------------------------------------------|
| #id             | #firstname      | Selects the element with id="firstname"         |
| .class          | .intro          | Selects all elements with class="intro"         |
| element         | p               | Selects all <p> elements                        |
| element,element | div, p          | Selects all <div> elements and all <p> elements |
| element element | div p           | Selects all <p> elements inside <div> elements  |
| :active         | a:active        | Selects the active link                         |
| ::first-letter  | p::first-letter | Selects the first letter of every <p> element   |
| ::first-line    | p::first-line   | Selects the first line of every <p> element     |
| :hover          | a:hover         | Selects links on mouse over                     |
| :link           | a:link          | Selects all unvisited links                     |
| :visited        | a:visited       | Selects all visited links                       |
|-----------------|-----------------|-------------------------------------------------|

(Taken from <http://www.w3schools.com/cssref/css_selectors.asp>.)

{% highlight haskell %}
type Identifier = String

data Selector where
  Id            :: Identifier -> Selector
  Class         :: Identifier -> Selector
  Element       :: Identifier -> Selector
  PseudoClass   :: PseudoClass   -> Selector
  PseudoElement :: PseudoElement -> Selector
  And           :: Selector -> Selector -> Selector
  Or            :: Selector -> Selector -> Selector
  In            :: Selector -> Selector -> Selector

data PseudoClass
   = Active | Hover | Link | Visited
   deriving (Show,Eq,Bounded,Enum)

data PseudoElement
   = FirstLetter | FirstLine
   deriving (Show,Eq,Bounded,Enum)
{% endhighlight %}

    h        [0-9a-f]
    nonascii [\240-\377]
    unicode  \\{h}{1,6}(\r\n|[ \t\r\n\f])?
    escape   {unicode}|\\[^\r\n\f0-9a-f]
    nmstart  [_a-z]|{nonascii}|{escape}
    nmchar   [_a-z0-9-]|{nonascii}|{escape}
    ident    -?{nmstart}{nmchar}*

(Taken from <https://www.w3.org/TR/CSS21/grammar.html>.)

{% highlight haskell %}
pHexDigit, pNonAscii :: Parser Char
pHexDigit = pAnySym "0123456789abcdef"
pNonAscii = pRange ('\240','\377')
{% endhighlight %}


{% highlight haskell %}
pPseudoClass :: Parser PseudoClass
pPseudoClass = pAny mkParser [minBound .. maxBound]
  where
    mkParser x = pSymbol (":"++ toDashed (show x)) *> pure x
{% endhighlight %}

{% highlight haskell %}
test_pPseudoClass = runParser [] pPseudoClass ":active"  == Active
                 && runParser [] pPseudoClass ":hover"   == Hover
                 && runParser [] pPseudoClass ":link"    == Link
                 && runParser [] pPseudoClass ":visited" == Visited
{% endhighlight %}

<div class="hidden">
{% highlight haskell %}
pPseudoElement :: Parser PseudoElement
pPseudoElement = pAny mkParser [minBound .. maxBound]
  where
    mkParser x = pSymbol ("::"++ toDashed (show x)) *> pure x
{% endhighlight %}

{% highlight haskell %}
test_pPseudoElement = parse pPseudoElement "::first-letter" == FirstLetter
                   && parse pPseudoElement "::first-line"   == FirstLine
{% endhighlight %}
</div>

<div class="hidden">
{% highlight haskell %}
parse :: Parser a -> String -> a
parse p str = runParser [] p str

main :: IO ()
main | not test_pPseudoClass   = exitFailure
     | not test_pPseudoElement = exitFailure
     | otherwise               = return ()
{% endhighlight %}
</div>
