--for Hugs 98
--import Prelude hiding (isDigit, isUpper, isLower, isAlpha :: Char -> Bool)

isDigit c = c >= '0' && c <= '9'
isUpper c = c >= 'A' && c <= 'Z'
isLower c = c >= 'a' && c <= 'z'
isAlpha c = isUpper c || isLower c

{-
	> isDigit '1'
	True
	> isDigit 'a'
	False

	> isUpper 'a'
	False
	> isUpper 'A'
	True
	> isUpper 'N'
	True

	> isLower 'N'
	False
	> isLower 'b'
	True

	> isAlpha 'c'
	True
	> isAlpha '1'
	False
-}