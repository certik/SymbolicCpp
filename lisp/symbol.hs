

--    SymbolicC++ : An object oriented computer algebra system written in C++
--
--    Copyright (C) 2008 Yorick Hardy and Willi-Hans Steeb
--
--    This program is free software; you can redistribute it and/or modify
--    it under the terms of the GNU General Public License as published by
--    the Free Software Foundation; either version 2 of the License, or
--    (at your option) any later version.
--
--    This program is distributed in the hope that it will be useful,
--    but WITHOUT ANY WARRANTY; without even the implied warranty of
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--    GNU General Public License for more details.
--
--    You should have received a copy of the GNU General Public License along
--    with this program; if not, write to the Free Software Foundation, Inc.,
--    51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.


-- symbol.hs

-- Every program has module Main
module Main where

-- A symbolic expression over a type a (eg. Double) is a 
-- symbol, a number with value, a sum of symbolic expressions
-- or a product of symbolic expressions
data Symbolic a = Symbol String
                | Number a
                | Sum [ Symbolic a ]
                | Product [ Symbolic a ]

-- tests for the different cases

isSymbol  (Symbol x)  = True
isSymbol  x           = False
isNumber  (Number x)  = True
isNumber  x           = False
isSum     (Sum x)     = True
isSum     x           = False
isProduct (Product x) = True
isProduct x           = False

-- addition a + b
add a b = Sum [ a, b ]
-- multiplication a * b
mul a b = Product [ a, b ]

-- control for recursion
ifrec again f = if again then f False else (\x -> x)

-- expand flattens the tree for a symbolic expression
--  and applies the distributive property for multiplication
-- the arguments are a boolean value which specifies
--  whether the expansion should continue recursively
--  and an expression to expand

split p [] = ([], [], [])
split p (a : as) = if p a then ([], [a], as)
                   else (a : x, y, z) where (x, y, z) = split p as

expand1 again (Sum [s])     = expand s
expand1 again (Product [p]) = expand p
expand1 again (Sum summands)
 = (ifrec again expand1) (Sum newlist)
   where expandabsorb x = absorb (expand x)
         absorb (Sum s) = s
         absorb x       = [x]
         newlist        = foldl1 (++) (map expandabsorb summands)
expand1 again (Product factors)
 = if or (map isSum factors) then expand1 again (Sum (map mul s))
   else (ifrec again expand1) (Product newlist)
   where (a, [Sum s], b)    = split isSum factors
         mul                = if (null a) && (null b) then \x -> x
                              else \x -> Product (a++ [x] ++ b)
         expandabsorb x     = absorb (expand x)
         absorb (Product p) = p
         absorb x           = [x]
         newlist            = foldl1 (++) (map expandabsorb factors)
expand1 again x = x

expand x = expand1 True x

-- tostring converts a symbolic expression
-- to a string representation
tostring (Symbol s)                  = s
tostring (Number n)                  = show n
tostring (Sum     [])                = ""
tostring (Product [])                = ""
tostring (Sum     ((Symbol s) : [])) = s
tostring (Product ((Symbol s) : [])) = s
tostring (Sum     ((Number n) : [])) = (show n)
tostring (Product ((Number n) : [])) = (show n)
tostring (Sum     (a : []))          = "(" ++ (tostring a) ++ ")"
tostring (Product (a : []))          = "(" ++ (tostring a) ++ ")"
tostring (Sum     (first : rest))
 = summand1 ++ "+" ++ summand2
   where summand1 = tostring (Sum (first : []))
         summand2 = tostring (Sum rest)
tostring (Product (first : rest))
 = factor1 ++ factor2
   where factor1 = tostring (Product (first : []))
         factor2 = tostring (Product rest)


-- Examples

-- explicit type
a :: Symbolic Integer
a = Symbol "a"
-- inferred type b :: Symbolic a
b = Symbol "b"
-- inferred type c :: Symbolic Integer
c = let { (+) = add; (*) = mul } in (Number 3) * (a + b) * a
-- inferred type d :: Symbolic a
d = Symbol "d"
-- inferred type e :: Symbolic Double
e = let { (+) = add; (*) = mul } in (Number 3.0) * d * (d + d)

-- every program must have a main expression
-- that evaluates to some IO
main = let (+) = add
           (*) = mul 
          in do putStrLn (tostring c)
                putStrLn (tostring (expand c))
                putStrLn (tostring (expand (a+b+a)))
                putStrLn (tostring e)
