---
title      : "How to hit the ground running with uu-parsinglib"
date       : 2016-03-17 12:00:00
categories : [haskell]
tags       : [haskell, parser combinators, uu-parsinglib]
---

{% highlight haskell %}
module HowToHitTheGroundRunning where
{% endhighlight %}

+-----------------+-----------------+-------------------------------------------------+
| **Selector**    |**Example**      | **Example description**                         |
+-----------------+-----------------+-------------------------------------------------+
| .class          | .intro          | Selects all elements with class="intro"         |
| #id             | #firstname      | Selects the element with id="firstname"         |
| element         | p               | Selects all <p> elements                        |
| element,element | div, p          | Selects all <div> elements and all <p> elements |
| element element | div p           | Selects all <p> elements inside <div> elements  |
| :active         | a:active        | Selects the active link                         |
| ::first-letter  | p::first-letter | Selects the first letter of every <p> element   |
| ::first-line    | p::first-line   | Selects the first line of every <p> element     |
| :hover          | a:hover         | Selects links on mouse over                     |
| :link           | a:link          | Selects all unvisited links                     |
| :visited        | a:visited       | Selects all visited links                       |
+-----------------+-----------------+-------------------------------------------------+

{% highligh   haskell %}
main :: IO ()
main = pu  S  rLn "I   works!"
{% endhighligh   %}
