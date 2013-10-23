{-# LANGUAGE TypeFamilies, QuasiQuotes #-}

module Language.Lojban.Parser.ZasniGerna.Parser (
	testParse
) where

import Text.Papillon
import Data.Maybe

testParse :: String -> Either String (Text, Terminator)
testParse src
	| Right (r, _) <- parsed = Right r
	| Left l <- parsed = Left $ showParseError l
	where
	parsed = runError $ gerna_text $ gerna_parse src

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

data Text
	= Rel Text Relative
	| KOhA [String] String Free
	| LI Initiator Mex Terminator
	| LU Initiator Text Terminator
	| Clause [String] Word Free
	| Lergum [Lerfu] Terminator
	deriving Show

data Relative
	= GOIKOhA [String] String Free Text
	| NR
	deriving Show

data Mex
	= Stub String
	deriving Show

data Lerfu
	= Lerfu [String] Word Free
	deriving Show

data Initiator
	= Init [String] String Free
	deriving Show

data Terminator
	= Term [String] String Free
	| NT
	deriving Show

data Free
	= UI [String] String Free
	| NF
	deriving Show

data Word
	= BUStr Word [([BU], [ZEI])] [BU]
	| ZEIStr Word [([ZEI], [BU])] [ZEI]
	| ZO String
	| LOhU [String]
	| ZOI String String String
	| Word String
	deriving Show

data ZEI = ZEI String deriving Show
data BU = BU deriving Show

[papillon|

prefix: "gerna_"

-- ****** A. GRAMMAR ******

-- 1. Text Paragraphs Statement

text :: (Text, Terminator)
	= _:words_SU* _:SI_tail* ps:post p:paragraphs f:FAhO_?
							{ (p, fromMaybe NT f) }

-- stub
paragraphs :: Text = s:bare_sumti { s }

-- 2. Sentence Bridi

-- 3. Term Sumti

bare_sumti :: Text = s:
	-- ( d:description
	( li:LI_ m:mex lo:LOhO_?		{ LI li m $ fromMaybe NT lo }
	/ z:ZO_word_				{ z }
	/ lu:LU_ p:paragraphs li:LIhU_?		{ LU lu p $ fromMaybe NT li }
	/ l:LOhU_words_LEhU_			{ l }
	/ z:ZOI_anything_			{ z }
	/ k:KOhA_				{ k }
	/ {- !_:tag !_:selbri -} ls:(l:lerfu { l })+ b:BOI_?
						{ Lergum ls $ fromMaybe NT b }
	-- / {- !_:tag !_:selbri -} ln:(l:LAhE_ { l } / n:NAhE_ b:BO_ { n b })
	-- 	r:rels? s:sumti _:LUhU_?
 ) r:rels?						{ maybe s (Rel s) r }

-- 4. Mex

-- stub
mex :: Mex = p:PA { Stub p }

lerfu :: Lerfu
	= b:BY_						{ b }
	/ b:word_BU_					{ b }

-- 5. Relative

-- stub
rels :: Relative = (b, g, f):GOI_ k:KOhA_ { GOIKOhA b g f k }

-- 6. Selbri Tanru unit

-- 7. Link args

-- 8. Connective

-- 9. Tense Modal

-- 10. Free modifier

free :: Free
	= u:UI_						{ u }

-- vocative :: Free
--	= 

-- ****** B. LOW LEVEL GRAMMAR ******

--- 11. SELMAhO ---

FAhO_ :: Terminator = pr:pre f:FAhO ps:post		{ Term pr f ps }

GOI_ :: ([String], String, Free) = pr:pre g:GOI ps:post	{ (pr, g, ps) }

BOI_ :: Terminator = pr:pre b:BOI ps:post		{ Term pr b ps }

BY_ :: Lerfu = pr:pre b:BY ps:lerfu_post		{ Lerfu pr (Word b) ps }

KOhA_ :: Text = pr:pre k:KOhA ps:post			{ KOhA pr k ps }

LI_ :: Initiator = pr:pre l:LI ps:post			{ Init pr l ps }

LIhU_ :: Terminator = pr:pre l:LIhU ps:post		{ Term pr l ps }

LOhO_ :: Terminator = pr:pre l:LOhO ps:post		{ Term pr l ps }

LU_ :: Initiator = pr:pre l:LU ps:post			{ Init pr l ps }

UI_ :: Free = pr:pre u:UI ps:post			{ UI pr u ps }

--- 12. Pseudo SELMAhO ---

word_ZEI_word_ :: Text = pr:pre z:word_ZEI_word ps:post	{ Clause pr z ps }

word_BU_ :: Lerfu = pr:pre b:word_BU ps:post		{ Lerfu pr b ps }

ZO_word_ :: Text = pr:pre z:ZO_word ps:post		{ Clause pr z ps }

LOhU_words_LEhU_ :: Text = pr:pre l:LOhU_words_LEhU ps:post
							{ Clause pr l ps }

ZOI_anything_ :: Text = pr:pre z:ZOI_anything ps:post	{ Clause pr z ps }

--- 13. Word Modifiers ---

pre :: [String] = bs:(_:word_SI* b:BAhE { b })* _:word_SI*
							{ bs }

post :: Free = !_:BU_tail !_:ZEI_tail f:free?		{ fromMaybe NF f }

-- number_post :: () = !_:BU_tail !_:ZEI_tail (!_:PA_ _:free)?

lerfu_post :: Free = !_:BU_tail !_:ZEI_tail fr:(!_:lerfu f:free { f })?
							{ fromMaybe NF fr }

-- vocative_post :: () = !_:BU_tail !_:ZEI_tail (!_:vocative _:free)?

-- ****** C. MAGIC WORD CONSTRUCTS ******

words_SU :: [Word] = ws:
	( z:word_ZEI_word			{ z }
	/ b:word_BU				{ b }
	/ z:ZO_word				{ z }
	/ l:LOhU_words_LEhU			{ l }
	/ z:ZOI_anything			{ z }
	/ !_:SU a:any_word			{ Word a }
 )* _:SU_tail						{ ws }

word_SI :: Word = w:
	( z:word_ZEI_word			{ z }
	/ b:word_BU				{ b }
	/ z:ZO_word				{ z }
	/ l:LOhU_words_LEhU			{ l }
	/ z:ZOI_anything			{ z }
	/ a:any_word				{ Word a }
 ) _:SI_tail						{ w }

word_BU :: Word = w:
	( z:ZO_word				{ z }
	/ l:LOhU_words_LEhU			{ l }
	/ z:ZOI_anything			{ z }
	/ a:any_word				{ Word a }
 ) bzs:
 	( bts:(bt:BU_tail { bt })*
		zts:(zt:ZEI_tail { zt })+	{ (bts, zts) }
 )* b:BU_tail+						{ BUStr w bzs b }

word_ZEI_word :: Word = w:
	( z:ZO_word				{ z }
	/ l:LOhU_words_LEhU			{ l }
	/ z:ZOI_anything			{ z }
	/ a:any_word				{ Word a }
 ) zbs:
	( zts:(zt:ZEI_tail { zt } )*
		bts:(bt:BU_tail { bt })+	{ (zts, bts) }
 )* z:ZEI_tail+						{ ZEIStr w zbs z }

SU_tail :: () = _:word_SI* _:SU

SI_tail :: () = _:word_SI* _:SI

BU_tail :: BU = _:word_SI* _:BU				{ BU }

ZEI_tail :: ZEI = _:word_SI* _:ZEI w:any_word		{ ZEI w }

ZO_word :: Word = _:ZO w:any_word			{ ZO w }

LOhU_words_LEhU :: Word = _:LOhU ws:
	(!_:LEhU w:any_word			{ w }
 )* _:LEhU						{ LOhU ws }

ZOI_anything :: Word = z:ZOI sep:any_word str:
	( !w:any_word[w == sep] c:anything	{ c }
 )* sep':any_word[sep == sep']				{ ZOI z sep $ unwords str }

-- ****** D. WORDS ******

anything :: String = _:Y* ns:non_space+			{ ns }

any_word :: String
	= c:CMEVLA					{ c }
	/ b:BRIVLA					{ b }
	/ c:CMAVO					{ c }

CMEVLA :: String = _:Y* c:cmevla			{ c }
BRIVLA :: String = _:Y* b:brivla			{ b }
CMAVO :: String = _:Y* c:cmavo				{ c }

BAI :: String = _:Y* &_:cmavo r:
	( c:c a:a h:h a:a			{ [c, a, h, a] }
	/ p:p u:u h:h i:i			{ [p, u, h, i] }
	/ n:n u:u h:h o:o			{ [n, u, h, o] }
	/ k:k a:a h:h e:e			{ [k, a, h, e] }
	/ c:c u:u h:h e:e			{ [c, u, h, e] }
	/ n:n a:a u:u				{ [n, a, u] }
	/ r:r u:u h:h i:i			{ [r, u, h, i] }
	/ t:t a:a h:h e:e			{ [t, a, h, e] }
	/ d:d i:i h:h i':i			{ [d, i, h, i'] }
	/ n:n a:a h:h o:o			{ [n, a, h, o] }
	/ v:v e:e h:h u:u			{ [v, e, h, u] }
	/ v:v e:e h:h a:a			{ [v, e, h, a] }
	/ v:v e:e h:h i:i			{ [v, e, h, i] }
	/ v:v e:e h:h e:e			{ [v, e, h, e] }
	/ v:v i:i h:h i:i			{ [v, i, h, i] }
	/ v:v i:i h:h a:a			{ [v, i, h, a] }
	/ v:v i:i h:h u:u			{ [v, i, h, u] }
	/ v:v i:i h:h e:e			{ [v, i, h, e] }
	/ v:v i:i				{ [v, i] }
	/ v:v a:a				{ [v, a] }
	/ v:v u:u				{ [v, u] }
	/ d:d u:u h:h a:a			{ [d, u, h, a] }
	/ b:b e:e h:h a:a			{ [b, e, h, a] }
	/ n:n e:e h:h u:u			{ [n, e, h, u] }
	/ v:v u:u h:h a:a			{ [v, u, h, a] }
	/ g:g a:a h:h u:u			{ [g, a, h, u] }
	/ t:t i:i h:h a:a			{ [t, i, h, a] }
	/ n:n i:i h:h a:a			{ [n, i, h, a] }
	/ c:c a:a h:h u:u			{ [c, a, h, u] }
	/ z:z u:u h:h a:a			{ [z, u, h, a] }
	/ r:r i:i h:h u:u			{ [r, i, h, u] }
	/ r:r u:u h:h u:u			{ [r, u, h, u] }
	/ r:r e:e h:h o:o			{ [r, e, h, o] }
	/ t:t e:e h:h e':e			{ [t, e, h, e'] }
	/ b:b u:u h:h u':u			{ [b, u, h, u'] }
	/ n:n e:e h:h a:a			{ [n, e, h, a] }
	/ p:p a:a h:h o:o			{ [p, a, h, o] }
	/ n:n e:e h:h i:i			{ [n, e, h, i] }
	/ t:t o:o h:h o':o			{ [t, o, h, o'] }
	/ z:z o:o h:h i:i			{ [z, o, h, i] }
	/ z:z e:e h:h o:o			{ [z, e, h, o] }
	/ z:z o:o h:h a:a			{ [z, o, h, a] }
	/ f:f a:a h:h a':a			{ [f, a, h, a'] }
	/ z:z e:e h:h u:u			{ [z, e, h, u] }
	/ z:z e:e h:h a:a			{ [z, e, h, a] }
	/ z:z e:e h:h i:i			{ [z, e, h, i] }
	/ z:z e:e h:h e:e			{ [z, e, h, e] }
	/ c:c o:o h:h i:i			{ [c, o, h, i] }
	/ p:p u:u h:h o:o			{ [p, u, h, o] }
	/ c:c o:o h:h u:u			{ [c, o, h, u] }
	/ m:m o:o h:h u:u			{ [m, o, h, u] }
	/ c:c a:a h:h o:o			{ [c, a, h, o] }
	/ c:c o:o h:h a:a			{ [c, o, h, a] }
	/ d:d e:e h:h a:a			{ [d, e, h, a] }
	/ b:b a:a h:h o:o			{ [b, a, h, o] }
	/ d:d i:i h:h a:a			{ [d, i, h, a] }
	/ z:z a:a h:h o:o			{ [z, a, h, o] }
	/ z:z u:u				{ [z, u] }
	/ z:z a:a				{ [z, a] }
	/ z:z i:i				{ [z, i] }
	/ b:b a:a				{ [b, a] }
	/ p:p u:u				{ [p, u] }
	/ c:c a:a				{ [c, a] }
	/ k:k i:i				{ [k, i] }
	/ d:d u:u h:h o:o			{ [d, u, h, o] }
	/ s:s i:i h:h u:u			{ [s, i, h, u] }
	/ z:z a:a u:u				{ [z, a, u] }
	/ k:k i:i h:h i':i			{ [k, i, h, i'] }
	/ d:d u:u h:h i:i			{ [d, u, h, i] }
	/ c:c u:u h:h u':u			{ [c, u, h, u'] }
	/ t:t u:u h:h i:i			{ [t, u, h, i] }
	/ t:t i:i h:h u:u			{ [t, i, h, u] }
	/ d:d i:i h:h o:o			{ [d, i, h, o] }
	/ j:j i:i h:h u:u			{ [j, i, h, u] }
	/ r:r i:i h:h a:a			{ [r, i, h, a] }
	/ n:n i:i h:h i':i			{ [n, i, h, i'] }
	/ m:m u:u h:h i:i			{ [m, u, h, i] }
	/ k:k i:i h:h u:u			{ [k, i, h, u] }
	/ v:v a:a h:h u:u			{ [v, a, h, u] }
	/ k:k o:o i:i				{ [k, o, i] }
	/ c:c a:a h:h i:i			{ [c, a, h, i] }
	/ t:t a:a h:h i:i			{ [t, a, h, i] }
	/ p:p u:u h:h e:e			{ [p, u, h, e] }
	/ j:j a:a h:h i:i			{ [j, a, h, i] }
	/ k:k a:a i:i				{ [k, a, i] }
	/ b:b a:a i:i				{ [b, a, i] }
	/ f:f i:i h:h e:e			{ [f, i, h, e] }
	/ d:d e:e h:h i:i			{ [d, e, h, i] }
	/ c:c i:i h:h o:o			{ [c, i, h, o] }
	/ m:m a:a u:u				{ [m, a, u] }
	/ m:m u:u h:h u':u			{ [m, u, h, u'] }
	/ r:r i:i h:h i':i			{ [r, i, h, i'] }
	/ r:r a:a h:h i:i			{ [r, a, h, i] }
	/ k:k a:a h:h a:a			{ [k, a, h, a] }
	/ p:p a:a h:h u:u			{ [p, a, h, u] }
	/ p:p a:a h:h a:a			{ [p, a, h, a] }
	/ l:l e:e h:h a:a			{ [l, e, h, a] }
	/ k:k u:u h:h u:u			{ [k, u, h, u] }
	/ t:t a:a i:i				{ [t, a, i] }
	/ b:b a:a u:u				{ [b, a, u] }
	/ m:m a:a h:h i:i			{ [m, a, h, i] }
	/ c:c i:i h:h e:e			{ [c, i, h, e] }
	/ f:f a:a u:u				{ [f, a, u] }
	/ p:p o:o h:h i:i			{ [p, o, h, i] }
	/ c:c a:a u:u				{ [c, a, u] }
	/ m:m a:a h:h e:e			{ [m, a, h, e] }
	/ c:c i:i h:h u:u			{ [c, i, h, u] }
	/ r:r a:a h:h a':a			{ [r, a, h, a'] }
	/ p:p u:u h:h a:a			{ [p, u, h, a] }
	/ l:l i:i h:h e:e			{ [l, i, h, e] }
	/ l:l a:a h:h u:u			{ [l, a, h, u] }
	/ b:b a:a h:h i:i			{ [b, a, h, i] }
	/ k:k a:a h:h i:i			{ [k, a, h, i] }
	/ s:s a:a u:u				{ [s, a, u] }
	/ f:f a:a h:h e:e			{ [f, a, h, e] }
	/ b:b e:e h:h i:i			{ [b, e, h, i] }
	/ t:t i:i h:h i':i			{ [t, i, h, i'] }
	/ j:j a:a h:h e:e			{ [j, a, h, e] }
	/ g:g a:a h:h a':a			{ [g, a, h, a'] }
	/ v:v a:a h:h o:o			{ [v, a, h, o] }
	/ j:j i:i h:h o:o			{ [j, i, h, o] }
	/ m:m e:e h:h a:a			{ [m, e, h, a] }
	/ d:d o:o h:h e:e			{ [d, o, h, e] }
	/ j:j i:i h:h e:e			{ [j, i, h, e] }
	/ p:p i:i h:h o:o			{ [p, i, h, o] }
	/ g:g a:a u:u				{ [g, a, u] }
	/ z:z u:u h:h e:e			{ [z, u, h, e] }
	/ m:m e:e h:h e':e			{ [m, e, h, e'] }
	/ r:r a:a i:i				{ [r, a, i] }
 ) &_:post_cmavo					{ r }

BAhE :: String = _:Y* &_:cmavo r:
	( b:b a:a h:h e:e			{ [b, a, h, e] }
	/ z:z a:a h:h e:e			{ [z, a, h, e] }
 ) &_:post_cmavo
							{ r }

BE :: String = _:Y* &_:cmavo r:(b:b e:e { [b, e] }) &_:post_cmavo
							{ r }

BEI :: String = _:Y* &_:cmavo r:(b:b e:e i:i { [b, e, i] }) &_:post_cmavo
							{ r }

BEhO :: String = _:Y* &_:cmavo r:(b:b e:e h:h o:o { [b, e, h, o] }) &_:post_cmavo
							{ r }

BO :: String = _:Y* &_:cmavo r:(b:b o:o { [b, o] }) &_:post_cmavo
							{ r }

BOI :: String = _:Y* &_:cmavo r:(b:b o:o i:i { [b, o, i] }) &_:post_cmavo
							{ r }

BU :: String = _:Y* &_:cmavo r:(b:b u:u { [b, u] }) &_:post_cmavo
							{ r }

BY :: String = _:Y* &_:cmavo r:
	( t:t e:e i:i				{ [t, e, i] }
	/ f:f o:o i:i				{ [f, o, i] }
	/ c:c e:e h:h a:a			{ [c, e, h, a] }
	/ l:l a:a u:u				{ [l, a, u] }
	/ z:z a:a i:i				{ [z, a, i] }
	/ t:t a:a u:u				{ [t, a, u] }
	/ j:j o:o h:h o:o			{ [j, o, h, o] }
	/ r:r u:u h:h o:o			{ [r, u, h, o] }
	/ g:g e:e h:h o:o			{ [g, e, h, o] }
	/ j:j e:e h:h o:o			{ [j, e, h, o] }
	/ l:l o:o h:h a:a			{ [l, o, h, a] }
	/ n:n a:a h:h a:a			{ [n, a, h, a] }
	/ s:s e:e h:h e:e			{ [s, e, h, e] }
	/ t:t o:o h:h a:a			{ [t, o, h, a] }
	/ g:g a:a h:h e:e			{ [g, a, h, e] }
	/ y:y h:h y:y				{ [y, h, y] }
	/ b:b y:y				{ [b, y] }
	/ c:c y:y				{ [c, y] }
	/ d:d y:y				{ [d, y] }
	/ f:f y:y				{ [f, y] }
	/ g:g y:y				{ [g, y] }
	/ j:j y:y				{ [j, y] }
	/ k:k y:y				{ [k, y] }
	/ l:l y:y				{ [l, y] }
	/ m:m y:y				{ [m, y] }
	/ n:n y:y				{ [n, y] }
	/ p:p y:y				{ [p, y] }
	/ r:r y:y				{ [r, y] }
	/ s:s y:y				{ [s, y] }
	/ t:t y:y				{ [t, y] }
	/ v:v y:y				{ [v, y] }
	/ x:x y:y				{ [x, y] }
	/ z:z y:y				{ [z, y] }
 ) &_:post_cmavo					{ r }

CEI :: String = _:Y* &_:cmavo r:(c:c e:e i:i { [c, e, i] }) &_:post_cmavo
							{ r }

CO :: String = _:Y* &_:cmavo r:(c:c o:o { [c, o] }) &_:post_cmavo
							{ r }

CU :: String = _:Y* &_:cmavo r:(c:c u:u { [c, u] }) &_:post_cmavo
							{ r }

DOI :: String = _:Y* &_:cmavo r:(d:d o:o i:i { [d, o, i] }) &_:post_cmavo
							{ r }

DOhU :: String = _:Y* &_:cmavo r:(d:d o:o h:h u:u { [d, o, h, u] }) &_:post_cmavo
							{ r }

FA :: String = _:Y* &_:cmavo r:
	( f:f a:a i:i				{ [f, a, i] }
	/ f:f a:a				{ [f, a] }
	/ f:f e:e				{ [f, e] }
	/ f:f o:o				{ [f, o] }
	/ f:f u:u				{ [f, u] }
	/ f:f i:i h:h a:a			{ [f, i, h, a] }
	/ f:f i:i				{ [f, i] }
 ) &_:post_cmavo					{ r }

FAhO :: String = _:Y* &_:cmavo r:(f:f a:a h:h o:o { [f, a, h, o] }) &_:post_cmavo
							{ r }

FEhU :: String = _:Y* &_:cmavo r:(f:f e:e h:h u:u { [f, e, h, u] }) &_:post_cmavo
							{ r }

FIhO :: String = _:Y* &_:cmavo r:(f:f i:i h:h o:o { [f, i, h, o] }) &_:post_cmavo
							{ r }

GA :: String = _:Y* &_:cmavo r:
	( g:g e:e h:h i:i			{ [g, e, h, i] }
	/ g:g e:e				{ [g, e] }
	/ g:g o:o				{ [g, o] }
	/ g:g a:a				{ [g, a] }
	/ g:g u:u				{ [g, u] }
 ) &_:post_cmavo					{ r }

GEhU :: String = _:Y* &_:cmavo r:(g:g e:e h:h u:u { [g, e, h, u] }) &_:post_cmavo
							{ r }

GI :: String = _:Y* &_:cmavo r:(g:g i:i { [g, i] }) &_:post_cmavo
							{ r }

GIhA :: String = _:Y* &_:cmavo r:
	( g:g i:i h:h e:e			{ [g, i, h, e] }
	/ g:g i:i h:h i:i			{ [g, i, h, i] }
	/ g:g i:i h:h o:o			{ [g, i, h, o] }
	/ g:g i:i h:h a:a			{ [g, i, h, a] }
	/ g:g i:i h:h u:u			{ [g, i, h, u] }
 ) &_:post_cmavo					{ r }

GOI :: String = _:Y* &_:cmavo r:
	( n:n o:o h:h u:u			{ [n, o, h, u] }
	/ n:n e:e				{ [n, e] }
	/ g:g o:o i:i				{ [g, o, i] }
	/ p:p o:o h:h u:u			{ [p, o, h, u] }
	/ p:p e:e				{ [p, e] }
	/ p:p o:o 				{ [p, o] }
 ) &_:post_cmavo					{ r }

GOhA :: String = _:Y* &_:cmavo r:
	( m:m o:o				{ [m, o] }
	/ n:n e:e i:i				{ [n, e, i] }
	/ g:g o:o h:h u:u			{ [g, o, h, u] }
	/ g:g o:o h:h o':o			{ [g, o, h, o'] }
	/ g:g o:o h:h i:i			{ [g, o, h, i] }
	/ n:n o:o h:h a:a			{ [n, o, h, a] }
	/ g:g o:o h:h e:e			{ [g, o, h, e] }
	/ g:g o:o h:h a:a			{ [g, o, h, a] }
	/ d:d u:u				{ [d, u] }
	/ b:b u:u h:h a:a			{ [b, u, h, a] }
	/ b:b u:u h:h e:e			{ [b, u, h, e] }
	/ b:b u:u h:h i:i			{ [b, u, h, i] }
	/ c:c o:o h:h e:e			{ [c, o, h, e] }
 ) &_:post_cmavo					{ r }

SelmahoI :: String = _:Y* &_:cmavo r:(i:i { [i] }) &_:post_cmavo	{ r }

JAI :: String = _:Y* &_:cmavo r:(j:j a:a i:i { [j, a, i] }) &_:post_cmavo
							{ r }

JOI :: String = _:Y* &_:cmavo r:
	( f:f a:a h:h u:u			{ [f, a, h, u] }
	/ p:p i:i h:h u:u			{ [p, i, h, u] }
	/ j:j o:o i:i				{ [j, o, i] }
	/ c:c e:e h:h o:o			{ [c, e, h, o] }
	/ c:c e:e				{ [c, e] }
	/ j:j o:o h:h u:u			{ [j, o, h, u] }
	/ k:k u:u h:h a:a			{ [k, u, h, a] }
	/ j:j o:o h:h e:e			{ [j, o, h, e] }
	/ j:j u:u h:h e:e			{ [j, u, h, e] }
	/ j:j o:o h:h i:i			{ [j, o, h, i] }
	/ j:j e:e h:h i:i			{ [j, e, h, i] }
	/ j:j e:e				{ [j, e] }
	/ j:j o:o				{ [j, o] }
	/ j:j a:a				{ [j, a] }
	/ j:j u:u				{ [j, u] }
	/ a:a					{ [a] }
	/ e:e					{ [e] }
	/ j:j i:i				{ [j, i] }
	/ o:o					{ [o] }
	/ u:u					{ [u] }
	/ m:m i:i h:h i':i			{ [m, i, h, i'] }
	/ b:b i:i h:h o:o			{ [b, i, h, o] }
	/ b:b i:i h:h i':i			{ [b, i, h, i'] }
	/ g:g e:e h:h a:a			{ [g, e, h, a] }
	/ f:f u:u h:h u':u			{ [f, u, h, u'] }
	/ p:p i:i h:h i':i			{ [p, i, h, i'] }
	/ f:f e:e h:h i:i			{ [f, e, h, i] }
	/ v:v u:u h:h u':u			{ [v, u, h, u'] }
	/ s:s u:u h:h i:i			{ [s, u, h, i] }
	/ j:j u:u h:h u:u			{ [j, u, h, u] }
	/ g:g e:e i:i				{ [g, e, i] }
	/ p:p a:a h:h i:i			{ [p, a, h, i] }
	/ f:f a:a h:h i:i			{ [f, a, h, i] }
	/ t:t e:e h:h a:a			{ [t, e, h, a] }
	/ c:c u:u h:h a:a			{ [c, u, h, a] }
	/ v:v a:a h:h a':a			{ [v, a, h, a'] }
	/ n:n e:e h:h o:o			{ [n, e, h, o] }
	/ d:d e:e h:h o:o			{ [d, e, h, o] }
	/ f:f e:e h:h a:a			{ [f, e, h, a] }
	/ s:s a:a h:h o:o			{ [s, a, h, o] }
	/ r:r e:e h:h a:a			{ [r, e, h, a] }
	/ r:r i:i h:h o:o			{ [r, i, h, o] }
	/ s:s a:a h:h i:i			{ [s, a, h, i] }
	/ p:p i:i h:h a:a			{ [p, i, h, a] }
	/ s:s i:i h:h i:i			{ [s, i, h, i] }
 ) &_:post_cmavo					{ r }

KE :: String = _:Y* &_:cmavo r:(k:k e:e { [k, e] }) &_:post_cmavo
							{ r }

KEhE :: String = _:Y* &_:cmavo r:(k:k e:e h:h e':e { [k, e, h, e'] }) &_:post_cmavo
							{ r }

KEI :: String = _:Y* &_:cmavo r:(k:k e:e i:i { [k, e, i] }) &_:post_cmavo
							{ r }

KOhA :: String = _:Y* &_:cmavo r:
	( d:d a:a h:h u:u			{ [d, a, h, u] }
	/ d:d a:a h:h e:e			{ [d, a, h, e] }
	/ d:d i:i h:h u:u			{ [d, i, h, u] }
	/ d:d i:i h:h e:e			{ [d, i, h, e] }
	/ d:d e:e h:h u:u			{ [d, e, h, u] }
	/ d:d e:e h:h e:e			{ [d, e, h, e] }
	/ d:d e:e i:i				{ [d, e, i] }
	/ d:d o:o h:h i:i			{ [d, o, h, i] }
	/ m:m i:i h:h o:o			{ [m, i, h, o] }
	/ m:m a:a h:h a:a			{ [m, a, h, a] }
	/ m:m i:i h:h a:a			{ [m, i, h, a] }
	/ d:d o:o h:h o':o			{ [d, o, h, o'] }
	/ k:k o:o h:h a:a			{ [k, o, h, a] }
	/ f:f o:o h:h u:u			{ [f, o, h, u] }
	/ k:k o:o h:h e:e			{ [k, o, h, e] }
	/ k:k o:o h:h i:i			{ [k, o, h, i] }
	/ k:k o:o h:h o':o			{ [k, o, h, o'] }
	/ k:k o:o h:h u:u			{ [k, o, h, u] }
	/ f:f o:o h:h a:a			{ [f, o, h, a] }
	/ f:f o:o h:h e:e			{ [f, o, h, e] }
	/ f:f o:o h:h i:i			{ [f, o, h, i] }
	/ f:f o:o h:h o':o			{ [f, o, h, o'] }
	/ v:v o:o h:h a:a			{ [v, o, h, a] }
	/ v:v o:o h:h e:e			{ [v, o, h, e] }
	/ v:v o:o h:h i:i			{ [v, o, h, i] }
	/ v:v o:o h:h o:o			{ [v, o, h, o] }
	/ v:v o:o h:h u:u			{ [v, o, h, u] }
	/ r:r u:u				{ [r, u] }
	/ r:r i:i				{ [r, i] }
	/ r:r a:a				{ [r, a] }
	/ t:t a:a				{ [t, a] }
	/ t:t u:u				{ [t, u] }
	/ t:t i:i				{ [t, i] }
	/ z:z i:i h:h o:o			{ [z, i, h, o] }
	/ k:k e:e h:h a:a			{ [k, e, h, a] }
	/ m:m a:a				{ [m, a] }
	/ z:z u:u h:h i:i			{ [z, u, h, i] }
	/ z:z o:o h:h e:e			{ [z, o, h, e] }
	/ c:c e:e h:h u:u			{ [c, e, h, u] }
	/ d:d a:a				{ [d, a] }
	/ d:d e:e				{ [d, e] }
	/ d:d i:i				{ [d, i] }
	/ k:k o:o				{ [k, o] }
	/ m:m i:i				{ [m, i] }
	/ d:d o:o				{ [d, o] }
 ) &_:post_cmavo					{ r }

KU :: String = _:Y* &_:cmavo r:(k:k u:u { [k, u] }) &_:post_cmavo
							{ r }

KUhO :: String = _:Y* &_:cmavo r:(k:k u:u h:h o:o { [k, u, h, o] }) &_:post_cmavo
							{ r }

LAhE :: String = _:Y* &_:cmavo r:
	( t:t u:u h:h a:a			{ [t, u, h, a] }
	/ l:l u:u h:h a:a			{ [l, u, h, a] }
	/ l:l u:u h:h o:o			{ [l, u, h, o] }
	/ l:l a:a h:h e:e			{ [l, a, h, e] }
	/ v:v u:u h:h i:i			{ [v, u, h, i] }
	/ l:l u:u h:h i:i			{ [l, u, h, i] }
	/ l:l u:u h:h e:e			{ [l, u, h, e] }
 ) &_:post_cmavo					{ r }

LE :: String = _:Y* &_:cmavo r:
	( l:l a:a i:i				{ [l, a, i] }
	/ l:l a:a h:h i:i			{ [l, a, h, i] }
	/ l:l a:a				{ [l, a] }
	/ l:l e:e i:i				{ [l, e, i] }
	/ l:l o:o i:i				{ [l, o, i] }
	/ l:l e:e h:h i:i			{ [l, e, h, i] }
	/ l:l o:o h:h i:i			{ [l, o, h, i] }
	/ l:l o:o				{ [l, o] }
	/ l:l e:e				{ [l, e] }
 ) &_:post_cmavo					{ r }

LEhU :: String = _:Y* &_:cmavo r:(l:l e:e h:h u:u { [l, e, h, u] }) &_:post_cmavo
							{ r }

LI :: String = _:Y* &_:cmavo r:(l:l i:i { [l, i] }) &_:post_cmavo
							{ r }

LIhU :: String = _:Y* &_:cmavo r:(l:l i:i h:h u:u { [l, i, h, u] }) &_:post_cmavo
							{ r }

LOhO :: String = _:Y* &_:cmavo r:(l:l o:o h:h o:o { [l, o, h, o] }) &_:post_cmavo
							{ r }

LOhU :: String = _:Y* &_:cmavo r:(l:l o:o h:h u:u { [l, o, h, u] }) &_:post_cmavo
							{ r }

LU :: String = _:Y* &_:cmavo r:(l:l u:u { [l, u] }) &_:post_cmavo
							{ r }

LUhU :: String = _:Y* &_:cmavo r:(l:l u:u h:h u:u { [l, u, h, u] }) &_:post_cmavo
							{ r }

MAI :: String = _:Y* &_:cmavo r:
	( m:m o:o h:h o:o			{ [m, o, h, o] }
	/ m:m a:a i:i				{ [m, a, i] }
 ) &_:post_cmavo					{ r }

ME :: String = _:Y* &_:cmavo r:
	( m:m e:e				{ [m, e] }
	/ n:n u:u h:h a:a			{ [n, u, h, a] }
 ) &_:post_cmavo					{ r }

MEhU :: String = _:Y* &_:cmavo r:(m:m e:e h:h u:u { [m, e, h, u] }) &_:post_cmavo
							{ r }

MOI :: String = _:Y* &_:cmavo r:
	( m:m e:e h:h u:u			{ [m, e, h, u] }
	/ m:m e:e i:i				{ [m, e, i] }
	/ m:m o:o i:i				{ [m, o, i] }
	/ s:s i:i h:h e:e			{ [s, i, h, e] }
	/ c:c u:u h:h o:o			{ [c, u, h, o] }
	/ v:v a:a h:h e:e			{ [v, a, h, e] }
 ) &_:post_cmavo					{ r }

NA :: String = _:Y* &_:cmavo r:
	( j:j a:a h:h a:a			{ [j, a, h, a] }
	/ n:n a:a				{ [n, a] }
 ) &_:post_cmavo					{ r }

NAhE :: String = _:Y* &_:cmavo r:
	( f:f e:e h:h e:e			{ [f, e, h, e] }
	/ m:m o:o h:h i:i			{ [m, o, h, i] }
	/ t:t o:o h:h e:e			{ [t, o, h, e] }
	/ j:j e:e h:h a:a			{ [j, e, h, a] }
	/ n:n a:a h:h e:e			{ [n, a, h, e] }
	/ n:n o:o h:h e:e			{ [n, o, h, e] }
 ) &_:post_cmavo					{ r }

NIhE :: String = _:Y* &_:cmavo r:(n:n i:i h:h e:e { [n, i, h, e] }) &_:post_cmavo
							{ r }

NIhO :: String = _:Y* &_:cmavo r:
	( n:n i:i h:h o:o			{ [n, i, h, o] }
	/ n:n o:o h:h i:i			{ [n, o, h, i] }
 ) &_:post_cmavo					{ r }

NOI :: String = _:Y* &_:cmavo r:
	( v:v o:o i:i				{ [v, o, i] }
	/ n:n o:o i:i				{ [n, o, i] }
	/ p:p o:o i:i				{ [p, o, i] }
 ) &_:post_cmavo					{ r }

NU :: String = _:Y* &_:cmavo r:
	( n:n i:i				{ [n, i] }
	/ d:d u:u h:h u:u			{ [d, u, h, u] }
	/ s:s i:i h:h o:o			{ [s, i, h, o] }
	/ n:n u:u				{ [n, u] }
	/ l:l i:i h:h i:i			{ [l, i, h, i] }
	/ k:k a:a				{ [k, a] }
	/ j:j e:e i:i				{ [j, e, i] }
	/ s:s u:u h:h u:u			{ [s, u, h, u] }
	/ z:z u:u h:h o:o			{ [z, u, h, o] }
	/ m:m u:u h:h e:e			{ [m, u, h, e] }
	/ p:p u:u h:h u:u			{ [p, u, h, u] }
	/ z:z a:a h:h i:i			{ [z, a, h, i] }
 ) &_:post_cmavo					{ r }

PA :: String = _:Y* &_:cmavo r:
	( d:d a:a u:u				{ [d, a, u] }
	/ f:f e:e i:i				{ [f, e, i] }
	/ g:g a:a i:i				{ [g, a, i] }
	/ j:j a:a u:u				{ [j, a, u] }
	/ r:r e:e i:i				{ [r, e, i] }
	/ v:v a:a i:i				{ [v, a, i] }
	/ p:p i:i h:h e:e			{ [p, i, h, e] }
	/ p:p i:i				{ [p, i] }
	/ f:f i:i h:h u:u			{ [f, i, h, u] }
	/ z:z a:a h:h u:u			{ [z, a, h, u] }
	/ m:m e:e h:h i:i			{ [m, e, h, i] }
	/ n:n i:i h:h u:u			{ [n, i, h, u] }
	/ k:k i:i h:h o:o			{ [k, i, h, o] }
	/ c:c e:e h:h i:i			{ [c, e, h, i] }
	/ m:m a:a h:h u:u			{ [m, a, h, u] }
	/ r:r a:a h:h e:e			{ [r, a, h, e] }
	/ d:d a:a h:h a:a			{ [d, a, h, a] }
	/ s:s o:o h:h a:a			{ [s, o, h, a] }
	/ j:j i:i h:h i':i			{ [j, i, h, i'] }
	/ s:s u:u h:h o:o			{ [s, u, h, o] }
	/ s:s u:u h:h e:e			{ [s, u, h, e] }
	/ r:r o:o				{ [r, o] }
	/ r:r a:a u:u				{ [r, a, u] }
	/ s:s o:o h:h u:u			{ [s, o, h, u] }
	/ s:s o:o h:h i:i			{ [s, o, h, i] }
	/ s:s o:o h:h e:e			{ [s, o, h, e] }
	/ s:s o:o h:h o':o			{ [s, o, h, o'] }
	/ m:m o:o h:h a:a			{ [m, o, h, a] }
	/ d:d u:u h:h e:e			{ [d, u, h, e] }
	/ t:t e:e h:h o:o			{ [t, e, h, o] }
	/ x:x o:o				{ [x, o] }
	/ p:p a:a i:i				{ [p, a, i] }
	/ n:n o:o h:h o:o			{ [n, o, h, o] }
	/ n:n o:o				{ [n, o] }
	/ p:p a:a				{ [p, a] }
	/ r:r e:e				{ [r, e] }
	/ c:c i:i				{ [c, i] }
	/ v:v o:o				{ [v, o] }
	/ m:m u:u				{ [m, u] }
	/ x:x a:a				{ [x, a] }
	/ z:z e:e				{ [z, e] }
	/ b:b i:i				{ [b, i] }
	/ s:s o:o				{ [s, o] }
 ) &_:post_cmavo					{ r }

ROI :: String = _:Y* &_:cmavo r:
	( r:r e:e h:h u:u			{ [r, e, h, u] }
	/ r:r o:o i:i				{ [r, o, i] }
 ) &_:post_cmavo					{ r }

SE :: String = _:Y* &_:cmavo r:
	( s:s e:e				{ [s, e] }
	/ t:t e:e				{ [t, e] }
	/ v:v e:e				{ [v, e] }
	/ x:x e:e				{ [x, e] }
 ) &_:post_cmavo					{ r }

SEI :: String = _:Y* &_:cmavo r:(s:s e:e i:i { [s, e, i] }) &_:post_cmavo
							{ r }

SEhU :: String = _:Y* &_:cmavo r:(s:s e:e h:h u:u { [s, e, h, u] }) &_:post_cmavo
							{ r }

SI :: String = _:Y* &_:cmavo r:(s:s i:i { [s, i] }) &_:post_cmavo
							{ r }

SU :: String = _:Y* &_:cmavo r:(s:s u:u { [s, u] }) &_:post_cmavo
							{ r }

TEhU :: String = _:Y* &_:cmavo r:(t:t e:e h:h u:u { [t, e, h, u] }) &_:post_cmavo
							{ r }

TO :: String = _:Y* &_:cmavo r:(t:t o:o { [t, o] }) &_:post_cmavo
							{ r }

TOI :: String = _:Y* &_:cmavo r:(t:t o:o i:i { [t, o, i] }) &_:post_cmavo
							{ r }

TUhE :: String = _:Y* &_:cmavo r:(t:t u:u h:h e:e { [t, u, h, e] }) &_:post_cmavo
							{ r }

TUhU :: String = _:Y* &_:cmavo r:(t:t u:u h:h u':u { [t, u, h, u'] }) &_:post_cmavo
							{ r }

UI :: String = _:Y* &_:cmavo result:
	( r:r a:a h:h o:o			{ [r, a, h, o] }
	/ k:k e:e h:h i:i			{ [k, e, h, i] }
	/ g:g a:a h:h o:o			{ [g, a, h, o] }
	/ n:n a:a i:i				{ [n, a, i] }
	/ p:p e:e i:i				{ [p, e, i] }
	/ c:c a:a i:i				{ [c, a, i] }
	/ c:c u:u h:h i:i			{ [c, u, h, i] }
	/ s:s a:a i:i				{ [s, a, i] }
	/ r:r u:u h:h e:e			{ [r, u, h, e] }
	/ d:d a:a h:h o:o			{ [d, a, h, o] }
	/ f:f u:u h:h e:e			{ [f, u, h, e] }
	/ f:f u:u h:h o:o			{ [f, u, h, o] }
	/ i:i h:h a:a				{ [i, h, a] }
	/ i:i e:e				{ [i, e] }
	/ a:a h:h e:e				{ [a, h, e] }
	/ u:u h:h i:i				{ [u, h, i] }
	/ i:i h:h o:o				{ [i, h, o] }
	/ i:i h:h e:e				{ [i, h, e] }
	/ a:a h:h a':a				{ [a, h, a'] }
	/ i:i a:a				{ [i, a] }
	/ o:o h:h i:i				{ [o, h, i] }
	/ o:o h:h e:e				{ [o, h, e] }
	/ e:e h:h e':e				{ [e, h, e'] }
	/ o:o i:i				{ [o, i] }
	/ u:u o:o				{ [u, o] }
	/ e:e h:h i:i				{ [e, h, i] }
	/ u:u h:h o:o				{ [u, h, o] }
	/ a:a u:u				{ [a, u] }
	/ u:u a:a				{ [u, a] }
	/ a:a h:h i:i				{ [a, h, i] }
	/ i:i h:h u:u				{ [i, h, u] }
	/ i:i i':i				{ [i, i'] }
	/ u:u h:h a:a				{ [u, h, a] }
	/ u:u i:i				{ [u, i] }
	/ a:a h:h o:o				{ [a, h, o] }
	/ a:a i:i				{ [a, i] }
	/ a:a h:h u:u				{ [a, h, u] }
	/ i:i u:u				{ [i, u] }
	/ e:e i:i				{ [e, i] }
	/ o:o h:h o':o				{ [o, h, o'] }
	/ e:e h:h a:a				{ [e, h, a] }
	/ u:u u':u				{ [u, u'] }
	/ o:o h:h a:a				{ [o, h, a] }
	/ o:o h:h u:u				{ [o, h, u] }
	/ u:u h:h u':u				{ [u, h, u'] }
	/ e:e h:h o:o				{ [e, h, o] }
	/ i:i o:o				{ [i, o] }
	/ e:e h:h u:u				{ [e, h, u] }
	/ u:u e:e				{ [u, e] }
	/ i:i h:h i':i				{ [i, h, i'] }
	/ u:u h:h e:e				{ [u, h, e] }
	/ b:b a:a h:h a:a			{ [b, a, h, a] }
	/ j:j a:a h:h o:o			{ [j, a, h, o] }
	/ c:c a:a h:h e:e			{ [c, a, h, e] }
	/ s:s u:u h:h a:a			{ [s, u, h, a] }
	/ t:t i:i h:h e:e			{ [t, i, h, e] }
	/ k:k a:a h:h u:u			{ [k, a, h, u] }
	/ s:s e:e h:h o:o			{ [s, e, h, o] }
	/ z:z a:a h:h a:a			{ [z, a, h, a] }
	/ p:p e:e h:h i:i			{ [p, e, h, i] }
	/ r:r u:u h:h a:a			{ [r, u, h, a] }
	/ j:j u:u h:h a:a			{ [j, u, h, a] }
	/ t:t a:a h:h o:o			{ [t, a, h, o] }
	/ r:r a:a h:h u:u			{ [r, a, h, u] }
	/ l:l i:i h:h a:a			{ [l, i, h, a] }
	/ b:b a:a h:h u:u			{ [b, a, h, u] }
	/ m:m u:u h:h a:a			{ [m, u, h, a] }
	/ d:d o:o h:h a:a			{ [d, o, h, a] }
	/ t:t o:o h:h u:u			{ [t, o, h, u] }
	/ v:v a:a h:h i:i			{ [v, a, h, i] }
	/ p:p a:a h:h e:e			{ [p, a, h, e] }
	/ z:z u:u h:h u:u			{ [z, u, h, u] }
	/ s:s a:a h:h e:e			{ [s, a, h, e] }
	/ l:l a:a h:h a':a			{ [l, a, h, a'] }
	/ k:k e:e h:h u:u			{ [k, e, h, u] }
	/ s:s a:a h:h u:u			{ [s, a, h, u] }
	/ d:d a:a h:h i:i			{ [d, a, h, i] }
	/ j:j e:e h:h u:u			{ [j, e, h, u] }
	/ s:s a:a h:h a':a			{ [s, a, h, a'] }
	/ k:k a:a u:u				{ [k, a, u] }
	/ t:t a:a h:h u:u			{ [t, a, h, u] }
	/ n:n a:a h:h i:i			{ [n, a, h, i] }
	/ j:j o:o h:h a:a			{ [j, o, h, a] }
	/ b:b i:i h:h u:u			{ [b, i, h, u] }
	/ l:l i:i h:h o:o			{ [l, i, h, o] }
	/ p:p a:a u:u				{ [p, a, u] }
	/ m:m i:i h:h u:u			{ [m, i, h, u] }
	/ k:k u:u n:n i:i			{ [k, u, n, i] }
	/ j:j i:i h:h a:a			{ [j, i, h, a] }
	/ s:s i:i h:h a:a			{ [s, i, h, a] }
	/ p:p o:o h:h o':o			{ [p, o, h, o'] }
	/ p:p e:e h:h a:a			{ [p, e, h, a] }
	/ r:r o:o h:h i:i			{ [r, o, h, i] }
	/ r:r o:o h:h e:e			{ [r, o, h, e] }
	/ r:r o:o h:h o':o			{ [r, o, h, o'] }
	/ r:r o:o h:h u:u			{ [r, o, h, u] }
	/ r:r o:o h:h a:a			{ [r, o, h, a] }
	/ r:r e:e h:h e:e			{ [r, e, h, e] }
	/ l:l e:e h:h o:o			{ [l, e, h, o] }
	/ j:j u:u h:h o:o			{ [j, u, h, o] }
	/ f:f u:u h:h i:i			{ [f, u, h, i] }
	/ d:d a:a i:i				{ [d, a, i] }
	/ g:g a:a h:h i:i			{ [g, a, h, i] }
	/ z:z o:o h:h o:o			{ [z, o, h, o] }
	/ b:b e:e h:h u:u			{ [b, e, h, u] }
	/ r:r i:i h:h e:e			{ [r, i, h, e] }
	/ s:s e:e h:h i:i			{ [s, e, h, i] }
	/ s:s e:e h:h a:a			{ [s, e, h, a] }
	/ v:v u:u h:h e:e			{ [v, u, h, e] }
	/ k:k i:i h:h a:a			{ [k, i, h, a] }
	/ x:x u:u				{ [x, u] }
	/ g:g e:e h:h e':e			{ [g, e, h, e'] }
	/ b:b u:u h:h o:o			{ [b, u, h, o] }
 ) &_:post_cmavo { result }

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

-- ****** E. MORPHOLOGY ******

cmevla :: String
	= j:jbocme					{ j }
	/ z:zifcme					{ z }

jbocme :: String = &_:zifcme s:
	(o:onset n:nucleus c:coda? { o ++ n ++ maybeToList c })+ &_:space'
							{ concat s }

zifcme :: String = !_:h cs:
	( v:V					{ [v] }
	/ vv:VV					{ vv }
	/ y:y					{ [y] }
	/ i:I					{ [i] }
	/ h:h					{ [h] }
	/ c:C !_:space'				{ [c] }
 )* c:C &_:space'					{ concat cs ++ [c] }

cmavo :: String
	= !_:cmevla !_:CVCy_lujvo c:C? i:I? n:nucleus
		hns:(h:h n:nucleus { h : n })* &_:post_cmavo
							{ catMaybes [c, i] ++
								n ++ concat hns }

CVCy_lujvo :: ()
	= _:C _:V _:C _:y _:initial_rafsi*
		_:(_:final_rafsi / _:gismu / _:fuhivla / _:type_3_fuhivla)

post_cmavo :: () = _:space' / !_:nucleus _:cmavo / _:brivla

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
	= !_:cmevla c:classifier s:syllable+ &_:space'
							{ concat $ c : s }

fuhivla :: String
	= !_:cmevla !_:cmavo !_:rafsi_string !_:slinkuhi
		s0:syllable ss:syllable+ &_:space'	{ concat $ s0 : ss }

gismu :: String
	= fr:full_rafsi &_:space'			{ fr }

final_rafsi :: String
	= !_:cmevla sr: short_rafsi &_:space'	{ sr }

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
	/ _:space					{ "" }

non_space :: Char
	= !_:space c					{ c }

space' :: ()
	= _:space / !_

space :: ()
	= c:[c `elem` ".\t\n\r?!\x0020"]

|]
