-- | Домашка 6. Необязательные челленджи.
module Level3 where

import MetaUtils (todo, todoImpl)


-- 3.1. Снова числа Чёрча — теперь с классами типов.
-- В Church находится натуральное число: шаг выполняется n раз.
-- Определите Show (точный формат "Church N") и Eq (сравнение значений n).
-- Напишите оба инстанса целиком.
-- toChurch/fromChurch даны, чтобы не повторять прошлые домашки.
-- Используйте Integer при декодировании, чтобы не переполнить Int.

newtype Church = Church (forall a. (a -> a) -> a -> a)

toChurch :: Integer -> Church
toChurch n
  | n < 0 = error "Church: negative input"
  | otherwise = Church $ \s z -> go n s z
  where
    go 0 _ z = z
    go k s z = s (go (k - 1) s z)

fromChurch :: Church -> Integer
fromChurch (Church n) = n (+ 1) 0

instance Show Church where
  show n = "Church " ++ show (fromChurch n)
instance Eq Church where
  n == m = fromChurch n == fromChurch m

-- 3.2. Арифметика и перечисление чисел Чёрча.
-- Реализуйте Enum и Num. pred 0 == 0, вычитание насыщается нулём,
-- abs сохраняет число, signum возвращает 0 или 1.
-- Напишите инстансы целиком, самостоятельно
-- выбрав минимально необходимый набор методов.
-- toEnum/fromInteger для отрицательных чисел должны сообщать ошибку.
-- fromEnum нужен только для чисел, помещающихся в Int.
-- Для succ, pred, +, * и - работайте с кодировкой (не декодируйте в Integer).
-- Это учебный Num: из-за отсутствия отрицательных чисел он НЕ удовлетворяет
-- всем законам Num. Не переносите его в код, ожидающий кольцо.
-- Напишите список [4,6,8,10,12,14] с помощью синтаксиса Enum и Num.


instance Enum Church where
  pred (Church n) = fst $ n (\(_, y) -> (y, succ y)) (toChurch 0, toChurch 0)
  toEnum   = toChurch . toInteger
  fromEnum = fromInteger . fromChurch

instance Num Church where
  (Church n) + (Church m) = Church $ \s z -> m s (n s z)
  (Church n) * m = n (+ m) (Church $ \_ z -> z)
  n - (Church m) = m pred n
  abs = id
  signum (Church n) = if (n (\_ -> False) True) then 0 else 1
  fromInteger = toChurch


from4To14 :: [Church]
from4To14 = enumFromThenTo (fromInteger 4) (fromInteger 6) (fromInteger 14)


-- 3.3. Явный словарь Functor.
-- f остаётся выбранным при создании словаря, а a и b выбираются заново при
-- каждом вызове myFmap. Поэтому forall стоит ВНУТРИ типа поля словаря.
-- RankNTypes уже включено в шаблоне. Закончите два словаря и case4.
-- case4 применяет один и тот же переданный словарь к двум разным типам.
-- Сначала проверьте case1–3, затем прочитайте тип go и реализуйте case4.

newtype MyFunctor f = MkMyFunctor
  { myFmap :: forall a b. (a -> b) -> f a -> f b }

myListFmap :: MyFunctor []
myListFmap = MkMyFunctor
  { myFmap = \f xs ->
    case xs of
      [] -> []
      (y:ys) -> f y : myFmap myListFmap f ys
  }

myMaybeFmap :: MyFunctor Maybe
myMaybeFmap = MkMyFunctor
  { myFmap = \f mb ->
    case mb of
      Nothing -> Nothing
      Just y  -> Just (f y)
  }

case1 :: [Int]
case1 = myFmap myListFmap (^ (2 :: Int)) [1 .. 10]

case2 :: [String]
case2 = myFmap myListFmap show ([1 .. 10] :: [Int])

case3 :: Maybe String
case3 = myFmap myMaybeFmap ('y' :) (Just "x")

case4 :: ([Int], [String])
case4 = go myListFmap
  where
    go :: MyFunctor [] -> ([Int], [String])
    go lf = (myFmap lf (^ (2 :: Int)) [1..10], myFmap lf ("y" ++) ["a"])