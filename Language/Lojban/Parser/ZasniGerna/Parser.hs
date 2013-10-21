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
	parsed = runError $ gerna_cmevla $ gerna_parse src

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
	| n == "char", Right (c, _) <- runError $ gerna_char d = show c
	| otherwise = "yet: " ++ n

maybeCons :: Maybe a -> [a] -> [a]
maybeCons mx xs = maybe xs (: xs) mx

[papillon|

prefix: "gerna_"

VAU :: String = _:Y* &_:cmavo r:(v:v a:a u:u { [v, a, u] }) &_:post_cmavo
							{ r }

VEI :: String = _:Y* &_:cmavo r:(v:v e:e i:i { [v, e, i] }) &_:post_cmavo
							{ r }

VEhO :: String = _:Y* &_:cmavo r:(v:v e:e h:h o:o { [v, e, h, o] }) &_:post_cmavo
							{ r }

VUhO :: String = _:Y* &_:cmavo r:(v:v u:u h:h o:o { [v, u, h, o] }) &_:post_cmavo
							{ r }

XI :: String = _:Y* &_:cmavo r:(x:x i:i { [x, i] }) &_:post_cmavo
							{ r }

ZEI :: String = _:Y* &_:cmavo r:(z:z e:e i:i { [z, e, i] }) &_:post_cmavo
							{ r }

ZIhE :: String = _:Y* &_:cmavo r:(z:z i:i h:h e:e { [z, i, h, e] }) &_:post_cmavo
							{ r }

ZO :: String = _:Y* &_:cmavo r:(z:z o:o { [z, o] }) &_:post_cmavo
							{ r }

ZOI :: String = _:Y* &_:cmavo r:
	( z:z o:o i:i				{ [z, o, i] }
	/ l:l a:a h:h o:o			{ [l, a, h, o] }
 ) &_:post_cmavo					{ r }

ZOhU :: String = _:Y* &_:cmavo r:(z:z o:o h:h u:u { [z, o, h, u] }) &_:post_cmavo
							{ r }

-- ****** MORPHOLOGY ******

cmevla :: String
	= j:jbocme					{ j }
	/ z:zifcme					{ z }

jbocme :: String = &_:zifcme s:
	(o:onset n:nucleus c:coda? { o ++ n ++ maybeToList c })+ &_:space
							{ concat s }

zifcme :: String = !_:h cs:
	( v:V					{ [v] }
	/ vv:VV					{ vv }
	/ y:y					{ [y] }
	/ i:I					{ [i] }
	/ h:h					{ [h] }
	/ c:C !_:space				{ [c] }
 )* c:C &_:space					{ concat cs ++ [c] }

cmavo :: String
	= {- !_:cmevla -} !_:CVCy_lujvo c:C? i:I? n:nucleus
		hns:(h:h n:nucleus { h : n })* &_:post_cmavo
							{ catMaybes [c, i] ++
								n ++ concat hns }

CVCy_lujvo :: ()
	= _:C _:V _:C _:y _:initial_rafsi*
		_:(_:final_rafsi / _:gismu / _:fuhivla / _:type_3_fuhivla)

post_cmavo :: () = _:space / _:cmavo / _:brivla

brivla :: String
	= g:gismu					{ g }
	/ !_:h f:fuhivla				{ f }
	/ t3f:type_3_fuhivla				{ t3f }
	/ l:lujvo					{ l }

lujvo :: String = !_:cmavo !_:h ir:initial_rafsi+ b:
	( fr:final_rafsi			{ fr }
	/ g:gismu				{ g }
	/ f:fuhivla				{ f }
	/ t3f:type_3_fuhivla			{ t3f }
 )							{ concat ir ++ b }

type_3_fuhivla :: String
	= {- !_:cmevla -} c:classifier s:syllable+ &_:space
							{ concat $ c : s }

fuhivla :: String
	= {- !_:cmevla -} !_:cmavo !_:rafsi_string !_:slinkuhi
		s0:syllable ss:syllable+ &_:space	{ concat $ s0 : ss }

gismu :: String
	= fr:full_rafsi &_:space			{ fr }

final_rafsi :: String
	= {- !_:cmevla -} sr: short_rafsi &_:space	{ sr }

initial_rafsi :: String
	= ylr:y_less_rafsi				{ ylr }
	/ yr:y_rafsi					{ yr }
	/ fr:fuhivla_rafsi				{ fr }
	/ t3r:type_3_rafsi				{ t3r }
	/ br:brivla_rafsi				{ br }

brivla_rafsi :: String
	= !_:cmavo !_:slinkuhi s0:syllable ss:syllable+ h:h y:y
							{ concat (s0 : ss) ++
								[h, y] }

type_3_rafsi :: String
	= c:classifier s:syllable* o:onset y:y		{ c ++ concat s ++ o ++
								[y] }

fuhivla_rafsi :: String
	= !_:cmavo !_:rafsi_string !_:slinkuhi s:syllable+ o:onset y:y
							{ concat s ++ o ++ [y] }

slinkuhi :: String
	= c:C rs:rafsi_string				{ c : rs }

rafsi_string :: String = ylrs:y_less_rafsi* b:
	( g:gismu				{ g }
	/ f:final_rafsi				{ f }
	/ yr:y_rafsi				{ yr }
	/ cc:CC y:y				{ cc ++ [y] }
	/ h:h y:y				{ [h, y] }
	/ fr:full_rafsi h:h y:y			{ fr ++ [h, y] }
 )							{ concat ylrs ++ b }

y_less_rafsi :: String
	= sr:short_rafsi &_:rafsi_string		{ sr }

classifier :: String
	= c1:C v:V c2:C cr:CR				{ c1 : v : c2 : cr }
	/ cc:CC v:V cr:CR				{ cc ++ v : cr }
	/ c:C v:V cr:CR					{ c : v : cr }
	/ yr:y_rafsi r:R				{ yr ++ [r] }

full_rafsi :: String
	= c1:C v1:V c2:C c3:C v2:V			{ [c1, v1, c2, c3, v2] }
	/ cc:CC v1:V c:C v2:V				{ cc ++ [v1, c, v2] }

y_rafsi :: String
	= c1:C v:V c2:C c3:C y:y			{ [c1, v, c2, c3, y] }
	/ cc:CC v:V c:C y:y				{ cc ++ [v, c, y] }
	/ c1:C v:V c2:C y:y				{ [c1, v, c2, y] }

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
	= t:t c:c					{ [t, c] }
	/ t:t s:s					{ [t, s] }
	/ d:d j:j					{ [d, j] }
	/ d:d z:z					{ [d, z] }

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

VV :: String = vv:
	( a:a i:i { [a, i] } / a:a u:u { [a, u] }
	/ e:e i:i { [e, i] } / o:o i:i { [o, i] }) !_:nucleus !_:I
							{ vv }
					

V :: Char = v:(a:a { a } / e:e { e } / i:i { i } / o:o { o } / u:u { u })
	!_:nucleus					{ v }

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
	= c:[c `elem` ".\t\n\r?!\x0020"] / !_

|]
