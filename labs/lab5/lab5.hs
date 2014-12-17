-- ========================================================================
--  Лабораторная работа №5
--  Пользовательские типы данных
-- ========================================================================

--существующие продукты:
-- книги: название и автор
-- видеокассеты: название
-- компакт-диск: название, исполнитель и количество композиций
data Product =
	Book{
		title  :: String,
		author :: String
	}
	| Videotape{
		title :: String
	}
	| Cd{
		title        :: String,
		performer    :: String,
		count_tracks :: Int
	}
	deriving (Eq, Show)

{-
	> let cd = Cd "Plug Me In" "AC/DC" 5
	> let video = Videotape "Gladiator"
	> let book = Book "Learn You a Haskell for Great Good!" "Miran Lipovaca"
	> book
	Book {title = "Learn You a Haskell for Great Good!", author = "Miran Lipovaca"}
	> :t cd
	cd :: Product
	> :t video
	video :: Product
	> :t book
	book :: Product
-}


--возвращает название товара
getTitle :: Product -> String
getTitle product = title product

{-
	> getTitle cd
	"Plug Me In"
	> getTitle video
	"Gladiator"
	> getTitle book
	"Learn You a Haskell for Great Good!"
-}


--по списку товаров возвращает список их названий
getTitles :: [Product] -> [String]
getTitles products = [getTitle product | product <- products]

{-
	> getTitles [book,video,cd]
	["Learn You a Haskell for Great Good!","Gladiator","Plug Me In"]
-}


--по списку товаров возвращает список авторов книг
bookAuthors :: [Product] -> [String]
bookAuthors products = [author product | product <- products]

{-
	let book2 = Book "Programming Erlang!" "Joe Armstrong"
	> bookAuthors [book,book2]
	["Miran Lipovaca","Joe Armstrong"]
-}


--возвращает товар с заданным названием
--если их несколько - возвращает первый найденный
lookupTitle :: String -> [Product] -> Maybe Product
lookupTitle title products =
	case [product | product <- products, (getTitle product) == title] of
		[] -> Nothing
		[product] -> Just product
		prods -> Just (prods !! 0)

{-
	> lookupTitle "Plug Me In" [book,video,cd]
	Just (Cd {title = "Plug Me In", performer = "AC/DC", count_tracks = 5})
-}


--принимает в качестве параметров список названий
--и список товаров и для каждого названия извлекает из
--второго списка соответствующие товары
lookupTitles :: [String] -> [Product] -> [Product]
lookupTitles titles products = lookupTitles' titles products []
lookupTitles' [] products acc = reverse acc
lookupTitles' (title:rest) products acc =
	case lookupTitle title products of
		Just product ->
			lookupTitles' rest products (product:acc)
		_ ->
			lookupTitles' rest products acc

{-
	let cd2 = Cd "Fly on the Wall" "AC/DC" 10
	> lookupTitles ["Fly on the Wall", "Plug Me In"] [book,cd,video,cd2]
	[Cd {title = "Fly on the Wall", performer = "AC/DC", count_tracks = 10},
	 Cd {title = "Plug Me In", performer = "AC/DC", count_tracks = 5}]
-}