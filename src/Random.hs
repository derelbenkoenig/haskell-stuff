import System.Random

main = do
	gen <- newStdGen
	let ns = randoms gen :: [Int]
	print $ take 10 ns
