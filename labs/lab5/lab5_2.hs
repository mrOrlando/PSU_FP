-- ========================================================================
--  Лабораторная работа №5
--  Пользовательские типы данных
--  Часть 2 (азартная)
-- ========================================================================

--карта:

--type Suit = ["hearts" | "spades" | "tambourine" | "clubs"]

--hearts - червы
--spades - пики
--tambourine - бубна
--clubs -треф

--11-туз
--12-валет
--13-дава
--14-король

data GameCard = GameCard{ suit  :: String -- масть
						, value :: Int    -- значение
						} deriving (Eq, Show)

{-
	> let card = GameCard{suit="hearts",value=6}
	> card
	GameCard {suit = "hearts", value = 6}
	> :t card
	card :: GameCard
-}

-- проверяет, что её аргумент является младшей картой
isMinor :: GameCard -> Bool
isMinor GameCard{value = value} = if value < 11 then True else False

{-
	> isMinor card
	True
-}


-- проверяет, что переданные карты одной масти
sameSuit :: [GameCard] -> Bool
sameSuit cards@(head:_tail) =
	let GameCard{suit=firstSuit} = head in
	let res = [1 | GameCard{suit=suit} <- cards, suit == firstSuit] in
	sum res == length cards

{-
	sameSuit [card,card]
	True
	let card2 = GameCard{suit="spades",value=6}
	> sameSuit [card,card2]
	False
-}


-- проверяет бьёт ли первая карта вторую
--11-туз
--12-валет
--13-дава
--14-король
beats :: GameCard -> GameCard -> Bool
beats GameCard{value=v1} GameCard{value=v2}
	| (v1 == 11) && (v2 /= 11) = True -- если первая карта туз, а вторая нет
	| (v2 == 11) = False -- если вторая карта туз
	| (v1 > v2) = True -- если первая карта больше второй
	| otherwise = False

{-
	> let card = GameCard{suit="hearts",value=6}
	> let card2 = GameCard{suit="spades",value=11}
	> beats card card2
	False
	> beats card2 card2
	False
	> beats card2 card
	True
	> let card = GameCard{suit="spades",value=14}
	> beats card2 card
	True
-}

-- проверяет бьёт ли первая карта вторую
-- учитывая козырную масть
--hearts - червы
--spades - пики
--tambourine - бубна
--clubs -треф
beats2 :: GameCard -> GameCard -> String -> Bool
beats2 GameCard{suit=s1, value=v1} GameCard{suit=s2, value=v2} trump
	| (t1 == True) && (t2 == False) = True -- если первая карта козырь, а вторая нет
	| (t1 == False) && (t2 == True) = False -- если первая карта простая, а вторая козырь

	| (t1 == True) && (v1 == 11) = True -- если первая карта козырный туз
	| (t2 == True) && (v2 == 11) = False -- если вторая карта козырный туз
	| (v1 > v2) && (t1 == True) && (t2 == True) = True -- если первая карта больше второй, обе козырные
	| (v1 < v2) && (t1 == True) && (t2 == True) = False -- если первая карта меньше второй, обе козырные

	| (v1 == 11) && (v2 /= 11) = True -- если первая карта туз, а вторая нет
	| (v2 == 11) = False -- если вторая карта туз
	| (v1 > v2) = True -- если первая карта больше второй
	| otherwise = False
	where
		t1 = (s1 == trump)
		t2 = (s2 == trump)

{-
	> let card = GameCard{suit="hearts",value=6}
	> let card2 = GameCard{suit="spades",value=11}
	> beats2 card card2 "hearts"
	True
	> beats2 card card2 "spades"
	False
	> beats2 card card2 "clubs"
-}