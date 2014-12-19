-- ========================================================================
--  Лабораторная работа №5
--  Пользовательские типы данных
--  Часть 2 (азартная)
-- ========================================================================

--карта:

--suit - масть
--hearts - червы
--spades - пики
--tambourine - бубна
--clubs -треф

--value - номинал
--11-туз
--12-валет
--13-дама
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
--13-дама
--14-король
beats :: GameCard -> GameCard -> Bool
beats GameCard{suit=s1, value=v1} GameCard{suit=s2, value=v2}
	| (v1 == 11) && (v2 /= 11) && (s1 == s2) = True -- если первая карта туз, а вторая нет, одной масти
	| (v2 == 11) && (s1 == s2) = False -- если вторая карта туз, одной масти
	| (v1 > v2) && (s1 == s2) = True -- если первая карта больше второй, одной масти
	| otherwise = False

{-
	> let card = GameCard{suit="hearts",value=6}
	> let card2 = GameCard{suit="spades",value=11}
	> beats card card2
	False
	> beats card2 card2
	False
	> beats card2 card
	False
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

	| (v1 == 11) && (v2 /= 11) && (s1 == s2) = True -- если первая карта туз, а вторая нет, одной масти
	| (v2 == 11) && (s1 == s2) = False -- если вторая карта туз, одной масти
	| (v1 > v2) && (s1 == s2) = True -- если первая карта больше второй, одной масти
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
	False
	> let card2 = GameCard{suit="hearts",value=11}
	> beats2 card card2 "hearts"
	False
	> beats2 card2 card "hearts"
	True
-}

--5) Функция beatsList, принимает в качестве аргументов список карт, карту и козырную масть
--возвращает список тех карт из первого арнумента, которые бьют указанную карту с учетом козырной масти.
beatsList :: [GameCard] -> GameCard -> String -> [GameCard]
beatsList cards comparСard trump = [card | card <- cards, beats2 card comparСard trump]

{-
	> let card = GameCard{suit="hearts",value=6}
	> let card2 = GameCard{suit="spades",value=11}
	> beatsList [card,card2] card "spades"
	[GameCard {suit = "spades", value = 11}]
	> beatsList [card,card2] card "hearts"
	[]
-}


--6) Функция, по заданному списку карт возвращающая список чисел, каждое из которых является
--возможной суммой очков указанных карт, рассчитанных по правилам игры в «двадцать одно»:
--младшие карты считаются по номиналу, валет, дама и король считаются за 10 очков, туз может
--рассматриваться и как 1 и как 11 очков. Функция должна вернуть все возможные варианты.
check21 :: [GameCard] -> [Int]
check21 cards =
	let values = [v | GameCard{value=v} <- cards] in --получает значение карт
	let newVal = map (\x -> if x > 11 then 10 else x) values in --преобразование вальтов, дам и королей
	if 11 `elem` newVal then --если в списке есть туз
		let newVal2 = map (\x -> if x == 11 then 1 else x) newVal in -- заменяет 11 на 1
		[sum newVal, sum newVal2] --считает обычную и новую суммы
	else --если в списке нет туза
		[sum newVal] --считает сумму

{-
	> let card = GameCard{suit="hearts",value=6}
	> let card2 = GameCard{suit="spades",value=11}
	> check21 [card,card2]
	[17,7]
	> let card3 = GameCard{suit="spades",value=14}
	> check21 [card,card2,card3]
	[27,17]
-}