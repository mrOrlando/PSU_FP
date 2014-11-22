myNot :: Bool -> Bool
myNot True = False
myNot False = True

{-
	sample in REPL:
	:l bool
	> 3 == 56-53
	True
	> myNot (3 == 56-53)
	False
-}


--Hugs 98
--exOr :: (Bool, Bool) -> Bool
--exOr(x, y) = (x || y) && not (x && y)
--> exOr(True, False)
--True
--> exOr(True, True)
--False


-- exOr(True, False) -> True
-- exOr(False, True) -> True
-- exOr(_, _) -> False
exOr :: (Bool, Bool) => Bool
exOr x y = (x || y) && not (x && y)

{-
	:l bool
	> exOr True False
	True
	> exOr False True
	True
	> exOr False False
	False
	> exOr True True
	False
-}