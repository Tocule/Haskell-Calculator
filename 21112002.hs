data Conversion =OnlyInt Int Int|StringFirst String Int [(String,Int)]|StringSecond Int String [(String,Int)] deriving (Show,Eq)

data Op =Add Conversion|Sub Conversion|Mult Conversion|Div Conversion deriving (Show,Eq)

data Result = Value Int|Exception String deriving (Show,Eq)


calc op = case op of
	  Add(OnlyInt a b)->Value(a+b)
	  Add(StringFirst a b p)->myAdd1 a b p
	  Add(StringSecond a b p)->myAdd2 a b p
	  Sub(OnlyInt a b)->Value(a-b)
          Sub(StringFirst a b p)->mySub1 a b p
          Sub(StringSecond a b p)->mySub2 a b p
	  Mult(OnlyInt a b)->Value(a*b)
          Mult(StringFirst a b p)->myMult1 a b p
          Mult(StringSecond a b p)->myMult2 a b p
 	  Div(OnlyInt a b)->myDiv a b
          Div(StringFirst a b p)->myDiv1 a b p
          Div(StringSecond a b p)->myDiv2 a b p
     

myAdd1::String->Int->[(String,Int)]->Result
myAdd1 a b [] = Exception("No matching variable found")
myAdd1 a b (x:xs) |a==fst(x) = Value(snd(x)+b)
		  |otherwise = (myAdd1 a b xs)

myAdd2::Int->String->[(String,Int)]->Result
myAdd2 a b [] = Exception("No matching variable found")
myAdd2 a b (x:xs) |b==fst(x) = Value(a+snd(x))
                  |otherwise = (myAdd2 a b xs)

mySub1::String->Int->[(String,Int)]->Result
mySub1 a b [] = Exception("No matching variable found")
mySub1 a b (x:xs) |a==fst(x) = Value(snd(x)-b)
                  |otherwise = (mySub1 a b xs)

mySub2::Int->String->[(String,Int)]->Result
mySub2 a b [] = Exception("No matching variable found")
mySub2 a b (x:xs) |b==fst(x) = Value(a-snd(x))
                  |otherwise = (mySub2 a b xs)

myMult1::String->Int->[(String,Int)]->Result
myMult1 a b [] = Exception("No matching variable found")
myMult1 a b (x:xs) |a==fst(x) = Value(snd(x)*b)
                   |otherwise = (myMult1 a b xs)

myMult2::Int->String->[(String,Int)]->Result
myMult2 a b [] = Exception("No matching variable found")
myMult2 a b (x:xs) |b==fst(x) = Value(a*snd(x))
                   |otherwise = (myMult2 a b xs)



myDiv1::String->Int->[(String,Int)]->Result
myDiv1 a b [] = Exception("No matching variable found or Division by Zero")
myDiv1 a b (x:xs) |(a==fst(x)&&b/=0) = Value(div (snd(x)) (b))
                  |otherwise = (myDiv1 a b xs)

myDiv2::Int->String->[(String,Int)]->Result
myDiv2 a b [] = Exception("No matching variable found or Division by Zero")
myDiv2 a b (x:xs) |(b==fst(x)&&snd(x)/=0) = Value(div (a) (snd(x)))
                  |otherwise = (myDiv2 a b xs)

myDiv::Int->Int->Result
myDiv a b|b==0 = Exception("Error:Division by zero")
	 |otherwise = Value(div a b)

