{-# LANGUAGE TypeFamilies, QuasiQuotes #-}

module Language.Lojban.Parser.ZasniGerna.Parser (
) where

import Text.Papillon
import Data.Maybe

testParse :: String -> Either String String
testParse src
	| Right (r, _) <- parsed = Right r
	| Left l <- parsed = Left $ showParseError l
	where
	parsed = runError $ gerna_short_rafsi $ gerna_parse src

showParseError :: ParseError (Pos String) Gerna_Derivs -> String
showParseError pe =
	unwords (map (showReading d) ns) ++ (if null ns then "" else " ") ++
	m ++ c ++ " at position: " ++ show p
	where
	[c, m, _] = ($ pe) `map` [peCode, peMessage, peComment]
	ns = peReading pe
	d = peDerivs pe
	p = pePositionS pe

showReading :: Gerna_Derivs -> String -> String
showReading d n
	| n == "gerna_char", Right (c, _) <- runError $ gerna_char d = show c
	| otherwise = "yet: " ++ n

maybeCons :: Maybe a -> [a] -> [a]
maybeCons mx xs = maybe xs (: xs) mx

[papillon|

prefix: "gerna_"

short_rafsi :: String
	= c1:C v:V c2:C					{ [c1, v, c2] }
	/ cc:CC v:V					{ cc ++ [v] }
	/ c:C vv:VV r:R?				{ c : vv ++
								maybeToList r }
	/ c:C v1:V h:h v2:V r:R?			{ c : v1 : h : v2 :
								maybeToList r }

syllable :: String
	= o:onset !_:y n:nucleus c:coda?		{ o ++ n ++ maybeToList c }

coda :: Char
	= !_:onset c:C					{ c }

onset :: String = o:
	( h:h					{ [h] }
	/ c:C? i:I				{ maybeCons c [i] }
	/ a:affricate				{ a }
	/ s:sibilant? m:middle? l:liquid?	{ catMaybes [s, m, l] }
 ) &_:nucleus						{ o }

sibilant :: Char
	= c:c						{ c }
	/ s:s !_:x					{ s }
	/ j:j !_:n !_:l !_:r				{ j }
	/ z:z !_:n !_:l !_:r				{ z }

middle :: Char
	= p:p						{ p }
	/ b:b						{ b }
	/ f:f						{ f }
	/ v:v						{ v }
	/ m:m						{ m }
	/ t:t !_:l					{ t }
	/ d:d !_:l					{ d }
	/ n:n !_:l !_:r					{ n }
	/ k:k						{ k }
	/ g:g						{ g }
	/ x:x						{ x }

liquid :: Char
	= l:l						{ l }
	/ r:r						{ r }

CC :: String
	= &_:onset c1:C c2:C			{ [c1, c2] }

CR :: String
	= c:C r:R					{ [c, r] }
	/ r:r n:n &_:C					{ [r, n] }
	/ r:r l:l &_:n					{ [r, l] }
	/ r:r l:l &_:affricate				{ [r, l] }
	/ n:n l:l &_:r					{ [n, l] }

R :: Char
	= r:r &_:C					{ r }
	/ n:n &_:r					{ n }

C :: Char
	= v:voiced					{ v }
	/ u:unvoiced					{ u }
	/ l:l						{ l }
	/ m:m						{ m }
	/ n:n						{ n }
	/ r:r						{ r }

affricate :: String
	= _:t _:c					{ "tc" }
	/ _:t _:s					{ "ts" }
	/ _:d _:j					{ "dj" }
	/ _:d _:z					{ "dz" }

voiced :: Char
	= b:b						{ b }
	/ d:d						{ d }
	/ g:g						{ g }
	/ j:j						{ j }
	/ v:v						{ v }
	/ z:z						{ z }

unvoiced :: Char
	= c:c						{ c }
	/ f:f						{ f }
	/ k:k						{ k }
	/ p:p						{ p }
	/ s:s						{ s }
	/ t:t						{ t }
	/ x:x						{ x }

l :: Char = 'l' !_:h !_:l				{ 'l' }
m :: Char = 'm' !_:h !_:m !_:z				{ 'm' }
n :: Char = 'n' !_:h !_:n !_:affricate			{ 'n' }
r :: Char = 'r' !_:h !_:r				{ 'r' }

b :: Char = 'b' !_:h !_:b !_:unvoiced			{ 'b' }
d :: Char = 'd' !_:h !_:d !_:unvoiced			{ 'd' }
g :: Char = 'g' !_:h !_:g !_:unvoiced			{ 'g' }
v :: Char = 'v' !_:h !_:v !_:unvoiced			{ 'v' }
j :: Char = 'j' !_:h !_:j !_:z !_:unvoiced		{ 'j' }
z :: Char = 'z' !_:h !_:z !_:j !_:unvoiced		{ 'z' }

s :: Char = 's' !_:h !_:s !_:c !_:voiced		{ 's' }
c :: Char = 'c' !_:h !_:c !_:s !_:x !_:voiced		{ 'c' }
x :: Char = 'x' !_:h !_:x !_:c !_:k !_:voiced		{ 'x' }
k :: Char = 'k' !_:h !_:k !_:x !_:voiced		{ 'k' }
f :: Char = 'f' !_:h !_:f !_:voiced			{ 'f' }
p :: Char = 'p' !_:h !_:p !_:voiced 			{ 'p' }
t :: Char = 't' !_:h !_:t !_:voiced			{ 't' }
h :: Char = '\'' &_:nucleus				{ '\'' }
I :: Char = c:(i:i { i } / u:u { u }) &_:nucleus	{ c }

nucleus :: String
	= v:V						{ [v] }
	/ vv:VV						{ vv }
	/ y:y						{ [y] }

VV :: String = vv:(
	_:a _:i { "ai" } / _:a _:u { "au" } / _:e _:i { "ei" } / _:o _:i {"oi" }
 ) !_:nucleus !_:I					{ vv }
					

V :: Char = v:(a:a { a } / e:e { e } / i:i { i } / o:o { o } / u:u { u })
							{ v }

a :: Char = 'a'						{ 'a' }
e :: Char = 'e'						{ 'e' }
i :: Char = 'i'						{ 'i' }
o :: Char = 'o'						{ 'o' }
u :: Char = 'u'						{ 'u' }
y :: Char = 'y' !_:nucleus				{ 'y' }

Y :: String
	= ys:('y' { 'y' })+ !_:nucleus			{ ys }
	/ space						{ "" }

non_space :: Char
	= !_:space c					{ c }

space :: ()
	= c:[c `elem` ".\t\n\r?!\x0020"]

|]
