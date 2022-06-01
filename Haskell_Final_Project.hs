--CACHE:

data Item a = It Tag (Data a) Bool Int | NotPresent deriving (Show, Eq)
data Tag = T Int deriving (Show, Eq)
data Data a = D a deriving (Show, Eq)




--GENERAL:

convertBinToDec :: Integral a => a -> a

convertBinToDec bin= convertBinToDechelper bin 0
convertBinToDechelper 0 _=0
convertBinToDechelper bin acc= 
	(mod bin 10)*(2^acc)+convertBinToDechelper (div bin 10) (acc+1)
	
	
	

replaceIthItem :: t -> [t] -> Int -> [t]

replaceIthItem item l index= replaceIthItemhelper item l index 0
replaceIthItemhelper item [] index acc=error "Index Not Found"
replaceIthItemhelper item (x:xs) index acc=
	if acc/=index then x:(replaceIthItemhelper item xs index (acc+1))
	else item:xs




splitEvery :: Int -> [a] -> [[a]]

splitEvery n [] = []
splitEvery n l	=splitEveryHelper1 n l 0 []	
splitEveryHelper1 n (x:xs) acc l1  
		|(acc ==0) && ((length (x:xs)) == n) = [(x:xs)]
		|acc/=n = splitEveryHelper1  n (xs) (acc+1) (x:l1)
		|otherwise =[(reverse l1)]++(splitEveryHelper1 n (x:xs) 0 [])




logBase2 :: Floating a => a -> a

logBase2 1.0 =0.0
logBase2 num= 1.0+logBase2 (num/2.0) 




getNumBits :: (Integral a, RealFloat a1) =>a1 -> [Char] -> [c] -> a		

getNumBits _ "fullyAssoc" _ = 0
getNumBits numOfSets "setAssoc" _ = ceiling(logBase2 numOfSets)
getNumBits _ "directMap" cache = ceiling(logBase2 (fromIntegral (length cache)))




fillZeros :: [Char] -> Int -> [Char]

fillZeros str 0= str
fillZeros str n = fillZeroshelper str n 0
fillZeroshelper (x:xs) n acc=
	if acc/=n then "0"++(fillZeroshelper (x:xs) n (acc+1))
	else x:xs




--getDataFromCache

data Output a = Out (a, Int) | NoOutput deriving (Show, Eq)
getDataFromCache :: (Integral b, Eq a) => [Char] -> [Item a] -> [Char] -> b -> Output a



getDataFromCache stringAddress [] "directMap" bitsNum = NoOutput 
getDataFromCache stringAddress ((It (T t) (D d) validBit order): tail) "directMap" bitsNum 
		|(fst (convertAddress (read stringAddress) bitsNum "directMap") == t) && (validBit == True) = Out (d,0) 
		|otherwise = getDataFromCache stringAddress tail "directMap" bitsNum


getDataFromCache stringAddress [] "fullyAssoc" bitsNum = NoOutput 
getDataFromCache stringAddress cache "fullyAssoc" bitsNum = getDataFromCacheHelper stringAddress cache "fullyAssoc" bitsNum 0


getDataFromCache stringAddress [] "setAssoc" bitsNum = NoOutput
getDataFromCache stringAddress ((It (T t) (D d) validBit order): tail) "setAssoc" bitsNum = 
	getDataFromCacheHelper stringAddress  ((splitEvery (div (length ((It (T t) (D d) validBit order:tail))) (2^bitsNum)) ((It (T t) (D d) validBit order: tail))!!(convertBinToDec (snd (convertAddress (read stringAddress) bitsNum "setAssoc"))))) "setAssoc" bitsNum 0



--getDataFromCacheHelper

getDataFromCacheHelper stringAddress [] "fullyAssoc" bitsNum acc = NoOutput
getDataFromCacheHelper stringAddress (It (T t) (D d) validBit order: tail) "fullyAssoc" bitsNum acc
		|(fst (convertAddress (read stringAddress) bitsNum "fullyAssoc") == t) && (validBit == True) = Out (d,acc)
		| otherwise = getDataFromCacheHelper stringAddress tail "fullyAssoc" bitsNum (acc+1)
		
getDataFromCacheHelper stringAddress [] "setAssoc" bitsNum acc = NoOutput
getDataFromCacheHelper stringAddress (It (T t) (D d) validBit order: tail) "setAssoc" bitsNum acc
		| (fst (convertAddress (read stringAddress) bitsNum "setAssoc") == t) && (validBit == True) = Out (d,acc)
		| otherwise = getDataFromCacheHelper stringAddress tail "setAssoc" bitsNum (acc+1)
		


--convertAddress

convertAddress binAddress bitsNum "directMap" = (div binAddress (10^bitsNum), mod binAddress (10^bitsNum))


convertAddress binAddress 0 "fullyAssoc" = (binAddress, 0)


convertAddress binAddress bitsNum "setAssoc" = (div binAddress  (10 ^ bitsNum), mod binAddress (10 ^ bitsNum))






--replaceInCache


replaceInCache tag idx memory ((It (T t) (D d) f i):xs) "directMap" bitsNum= (loopOnMemory memory (convertBinToDec (read(intTostr tag idx))) "directMap", replaceIthItem (It (T (convertBinToDec t)) (D (loopOnMemory memory (convertBinToDec (read(intTostr tag idx))) "directMap")) True 0) ((It (T t) (D d) f i):xs) (getMaxItemIth ((It (T t) (D d) f i):xs) 0 1))



replaceInCache tag idx memory oldCache "fullyAssoc" bitsNum |allTrue oldCache == False  =(finddatainMemoryfully (convertBinToDec tag) memory 0  , replacefully tag (finddatainMemoryfully (convertBinToDec tag) memory 0) (incrementoldcache oldCache))
														    |otherwise=	(finddatainMemoryfully (convertBinToDec tag) memory 0 ,replaceIthItem (It (T tag) (D(finddatainMemoryfully (convertBinToDec tag) memory 0)) True 0 ) (incrementoldcache oldCache) (findHigherOrderIndx (findHigherOrder oldCache) oldCache 0) )--case if every data item in the old cache is  valid

{-replaceInCache tag idx memory (It (T t) (D d) validBit order: tail) "setAssoc" bitsNum = (d, concat (replaceIthItem (replaceInCacheHelper tag idx memory (It (T t) (D d) validBit order: tail) "setAssoc" bitsNum) (It (T t) (D d) validBit order: tail) ((convertBinToDec (snd (convertAddress (read (intTostr tag idx)) bitsNum "setAssoc"))))))

replaceInCacheHelper tag idx memory (It (T t) (D d) validBit order : tail) "setAssoc" bitsNum =
	replaceInCache tag idx memory ((splitEvery (div (length ((It (T t) (D d) validBit order:tail))) (2^bitsNum)) ((It (T t) (D d) validBit order): tail))!!(convertBinToDec (snd (convertAddress (read (intTostr tag idx)) bitsNum "setAssoc")))) "fullyAssoc" bitsNum 
-}
--------

intTostr x y= (show x) ++ (show y)

loopOnMemory memory z "directMap" = 
	loopOnMemoryHelper memory z "directMap" 0
loopOnMemoryHelper [] _ "directMap" _ = []
loopOnMemoryHelper (x:xs) z "directMap" acc 
	| acc<z = loopOnMemoryHelper xs z "directMap" (acc+1)
	| otherwise = x

getMaxItemIth [x] acc1 acc2 = acc1
getMaxItemIth ((It (T t1) (D d1) f1 i1):(It (T t2) (D d2) f2 i2):xs) acc1 acc2= 
		if (i1>=i2 && f1== False && f2==False) then getMaxItemIth ((It (T (convertBinToDec t1)) (D d1) f1 i1):xs) (acc1) (acc2+1)
		else getMaxItemIth ((It (T (convertBinToDec t2)) (D d2) f2 i2):xs) (acc2) (acc2+1)

---------

allTrue []= True
allTrue (x:xs) |(getValidBit x)==True  =allTrue xs
               |otherwise=False
 
findHigherOrder::[(Item a)]->Item a
findHigherOrder (x:[]) =x
findHigherOrder (x1:xs)|getOrder x1 >= getOrder (findHigherOrder xs )=x1
                       |otherwise=findHigherOrder xs

findHigherOrderIndx x[] _ =error "NotFOUND"							   
findHigherOrderIndx	 x (x1:xs) acc |x==x1	=acc
				                   |otherwise=findHigherOrderIndx x xs (acc+1)

finddatainMemoryfully taginDec (x:xs) acc  |taginDec/=acc =finddatainMemoryfully taginDec xs (acc+1)
										   |otherwise =x
										   
incrementoldcache []=[]
incrementoldcache (x:xs)=incrementorder x :incrementoldcache xs								   
										   
makeanItem tag	data1 bool order =(It (T tag)  (D data1 ) bool order	)

replacefully _ _ []=[]								   

replacefully tag data1 (x:xs) |getValidBit x==False =(makeanItem tag data1 True 0:xs)
                              |otherwise =x:replacefully tag data1 xs
							  
getValidBit (It tag data1 bool order)=bool
getOrder    (It tag data1 bool order)=order 
incrementorder (It tag data1 bool order) |bool==True =(It tag data1 bool (order+1))
                                         |otherwise =(It tag data1 bool order)
										 
						
------

{-getData stringAddress cache memory cacheType bitsNum
| x == NoOutput = replaceInCache tag index memory cache cacheType bitsNum
| otherwise = (getX x, cache)
where
x = getDataFromCache stringAddress cache cacheType bitsNum
address = read stringAddress:: Int
(tag, index) = convertAddress address bitsNum cacheType
getX (Out (d, _)) = d -}

{-runProgram [] cache _ _ _ = ([], cache)
runProgram (addr: xs) cache memory cacheType numOfSets =
((d:prevData), finalCache)
where
bitsNum = round(logBase2 numOfSets)
(d, updatedCache) = getData addr cache memory cacheType bitsNum
(prevData, finalCache) = runProgram xs updatedCache memory cacheType numOfSets -}