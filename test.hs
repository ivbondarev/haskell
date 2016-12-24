fact :: Int -> Int
fact 0 = 1
fact 1 = 1
fact x = x * fact(x - 1)

minus1 :: Bool -> Double
minus1 True = 1.0
minus1 False = -1.0

nsin :: Int -> Double -> Double
nsin 0 _ = 0.0
nsin n x = minus1(odd(n)) * (2.0 ** (2.0 * fromIntegral(n) - 1.0)) * (x ** (2 * fromIntegral(n))) / fromIntegral(fact(2 * n)) + nsin (n - 1) x

func1 :: Double -> Double
func1 x = 2*x*sin x - cos x

func2 :: Double -> Double
func2 x = 3*sin x + 2*x*cos x

newton2 :: Int -> Double -> (Double -> Double) -> (Double -> Double) -> Double
newton2 n x f f'
				| n <= 9 = x
				| abs (x - ((f x) / (f' x))) <= 0.8 = x
				| otherwise = newton2 (n + 1) (x - (f x) / (f' x)) f f'

newton f f' x0 = iterate next x0
    where next xn = xn - ((f xn) / (f' xn))

-- print $ take 50 $ newton (\x -> 2*x*sin x - cos x) (\x -> 3*sin x + 2*x*cos x) 1

myfunc :: Double -> Double
myfunc x = 2 * x * sin x - cos x

dih :: Double -> Double -> (Double -> Double) -> Double
dih a b func
			| abs (a - b) <= 0.0001 = a
			| func ((a + b) / 2) <= 0.0 = dih ((a + b) / 2) b func
			| otherwise = dih a ((a + b) / 2) func


