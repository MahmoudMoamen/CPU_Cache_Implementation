convertBinToDec(Bin,Dec):-
        convertBinToDecHelper(Bin,Dec,0).
convertBinToDecHelper(0,0,_).
convertBinToDecHelper(Bin,Dec,Acc):-
        Bin > 0,                                                     
        Z is 2**Acc,
        X is Bin mod 10,
		Z1 is Z*X,
		AccNew is Acc+1,
		BinNew is Bin//10,
		convertBinToDecHelper(BinNew,Dec1,AccNew),        
		Dec is Z1+Dec1.


replaceIthItem(Item,L,I,Result):-
        replaceIthItemHelper(Item,L,I,Result,0).
replaceIthItemHelper(Item,[H|T],I,[H|T1],Acc):-
        Acc \= I,
		Acc1 is Acc+1,
		replaceIthItemHelper(Item,T,I,T1,Acc1).
replaceIthItemHelper(Item,[_|T],I,[Item|T],I).


splitEvery(N,L,R):-
        splitEveryHelper(N,L,R,1).
splitEveryHelper(_,[],[],_).
splitEveryHelper(N,[H|T],[[H|T1]|T2],Acc):-
        N \= Acc,
		Acc1 is Acc+1,
		splitEveryHelper(N,T,[T1|T2],Acc1).
splitEveryHelper(N,[H|T],[[H|[]]|T2],N):-
        splitEveryHelper(N,T,T2,1).


logBase2(1,0).
logBase2(Num,Res):-
		logBase2Helper(Num,Res,0).
logBase2Helper(1,Res,Res).
logBase2Helper(Num,Res,C):-
		Num>1,
		Num1 is Num/2,
		C1 is C+1,
		logBase2Helper(Num1,Res,C1).


fillZeros(X,0,X).
fillZeros(X,Y,Z):-
        Y>0,
		string_concat('0',X,Z1),
		Y1 is Y-1,
		fillZeros(Z1,Y1,Z).
		
		
getNumBits(_,fullyAssoc,_,0).
getNumBits(NumOfSets,setAssoc,_,BitsNum):-
        logBase2(NumOfSets,BitsNum).
getNumBits(_,directMap,Cache,BitsNum):-
		length(Cache,L),
		logBase2(L,BitsNum).



%DIRECT_MAPPING:

getDataFromCache(StringAddress,[H|_],Data,0,directMap,BitsNum):-
		atom_number(StringAddress,Address),
		convertAddress(Address,BitsNum,Tag,_,directMap),
		H = item(tag(TagCacheString),data(D),ValidBit,_),
		atom_number(TagCacheString,TagCache),
		TagCache = Tag,
		ValidBit = 1,
		Data = D.
getDataFromCache(StringAddress,[H|T],Data,0,directMap,BitsNum):-
		atom_number(StringAddress,Address),
		convertAddress(Address,BitsNum,Tag,_,directMap),
		H = item(tag(TagCacheString),data(_),ValidBit,_),
		atom_number(TagCacheString,TagCache),
		TagCache = Tag,
		ValidBit = 0,
		getDataFromCache(StringAddress,T,Data,0,directMap,BitsNum).
getDataFromCache(StringAddress,[H|T],Data,0,directMap,BitsNum):-
		atom_number(StringAddress,Address),
		convertAddress(Address,BitsNum,Tag,_,directMap),
		H = item(tag(TagCacheString),data(_),ValidBit,_),
		atom_number(TagCacheString,TagCache),
		TagCache \= Tag,
		ValidBit = 1,
		getDataFromCache(StringAddress,T,Data,0,directMap,BitsNum).
getDataFromCache(StringAddress,[H|T],Data,0,directMap,BitsNum):-
		atom_number(StringAddress,Address),
		convertAddress(Address,BitsNum,Tag,_,directMap),
		H = item(tag(TagCacheString),data(_),ValidBit,_),
		atom_number(TagCacheString,TagCache),
		TagCache \= Tag,
		ValidBit = 0,
		getDataFromCache(StringAddress,T,Data,0,directMap,BitsNum).			



convertAddress(Bin,0,Tag,Idx,directMap):-
		Tag = Bin,
		Idx = 0.
convertAddress(Bin,BitsNum,Tag,Idx,directMap):-
		convertAddressHelper(Bin,BitsNum,Tag,Idx,directMap,0,'').
convertAddressHelper(Bin,BitsNum,Tag,Idx,directMap,Acc,EmptyString):-
		Acc = BitsNum,
		Tag = Bin,
		atom_number(EmptyString,Idx).
convertAddressHelper(Bin,BitsNum,Tag,Idx,directMap,Acc,EmptyString):-
        Acc < BitsNum,
		X is Bin mod 10,
		BinNew is Bin//10,
		Acc1 is Acc +1,
		atom_number(XString,X),
		string_concat(XString,EmptyString,EmptyString1),
		convertAddressHelper(BinNew,BitsNum,Tag,Idx,directMap,Acc1,EmptyString1).



replaceInCache(Tag,Idx,Mem,OldCache,NewCache,ItemData,directMap,_):-
		atom_number(OutputTagNumber,Tag),
		atom_number(OutputIdxNumber,Idx),
		string_concat(OutputTagNumber,OutputIdxNumber,OutputAddressString),
		atom_number(OutputAddressString,OutputAddressNumber),
		convertBinToDec(OutputAddressNumber,OutputDecimalNumber),
		loopOnMemory(Mem,OutputDecimalNumber,DataFromMem,directMap),
		ItemData=DataFromMem,
		convertBinToDec(Idx,IdxInDecimal),
		loopOnCache(OldCache,IdxInDecimal,ItemData,NewCache,directMap,Tag).
	
loopOnMemory(Mem,OutputDecimalNumber,DataFromMem,directMap):-
		loopOnMemoryHelper(Mem,OutputDecimalNumber,DataFromMem,directMap,0).
loopOnMemoryHelper([H|_],OutputDecimalNumber,H,directMap,OutputDecimalNumber).
loopOnMemoryHelper([_|T],OutputDecimalNumber,DataFromMem,directMap,Acc):-
		Acc<OutputDecimalNumber,
		Acc1 is Acc+1,
		loopOnMemoryHelper(T,OutputDecimalNumber,DataFromMem,directMap,Acc1).

loopOnCache(OldCache,IdxInDecimal,ItemData,NewCache,directMap,Tag):-
		loopOnCacheHelper(OldCache,IdxInDecimal,ItemData,NewCache,directMap,Tag,0).
loopOnCacheHelper([H|T],IdxInDecimal,ItemData,[H1|T],directMap,Tag,IdxInDecimal):-
		atom_number(StringTag,Tag),
		H=item(tag(X),_,_,_),
		atom_length(X,XLength),
		atom_length(Tag,TagLength),
		NoOfZeros is XLength-TagLength,
		fillZeros(StringTag,NoOfZeros,NewerTag),
		H1=item(tag(NewerTag),data(ItemData),1,0).
loopOnCacheHelper([H|T],IdxInDecimal,ItemData,[H|T1],directMap,Tag,Acc):-
		Acc<IdxInDecimal,
		Acc1 is Acc+1,
		loopOnCacheHelper(T,IdxInDecimal,ItemData,T1,directMap,Tag,Acc1).



			
%FULLY_ASSOC:
		
getDataFromCache(StringAddress,[H|T],Data,HopsNum,fullyAssoc,_):-
		getDataFromCacheHelper(StringAddress,[H|T],Data,HopsNum,fullyAssoc,_,0).
getDataFromCacheHelper(StringAddress,[H|_],Data,HopsNum,fullyAssoc,_,Acc):-
		H = item(tag(Tag),data(D),ValidBit,_),
		StringAddress = Tag,
		ValidBit = 1,
		Data = D,
		HopsNum = Acc.
getDataFromCacheHelper(StringAddress,[H|T],Data,HopsNum,fullyAssoc,_,Acc):-
        H = item(tag(Tag),data(_),ValidBit,_),
		StringAddress = Tag,
		ValidBit = 0,
		Acc1 is Acc+1,
		getDataFromCacheHelper(StringAddress,T,Data,HopsNum,fullyAssoc,_,Acc1).
getDataFromCacheHelper(StringAddress,[H|T],Data,HopsNum,fullyAssoc,_,Acc):-
        H = item(tag(Tag),data(_),ValidBit,_),
		StringAddress \= Tag,
		ValidBit = 1,
		Acc1 is Acc+1,
		getDataFromCacheHelper(StringAddress,T,Data,HopsNum,fullyAssoc,_,Acc1).
getDataFromCacheHelper(StringAddress,[H|T],Data,HopsNum,fullyAssoc,_,Acc):-
        H = item(tag(Tag),data(_),ValidBit,_),
		StringAddress \= Tag,
		ValidBit = 0,
		Acc1 is Acc+1,
		getDataFromCacheHelper(StringAddress,T,Data,HopsNum,fullyAssoc,_,Acc1).



convertAddress(Bin,_,Tag,_,fullyAssoc):- 
		Tag = Bin.



replaceInCache(Tag,_,Mem,OldCache,NewCache,ItemData,fullyAssoc,_):-
		increment(OldCache,L),
		convertBinToDec(Tag,OutputDecimalNumber),
		loopOnMemory(Mem,OutputDecimalNumber,DataFromMem,fullyAssoc),
		ItemData=DataFromMem,
		loopOnCache(L,_,ItemData,NewCache,Tag,fullyAssoc).		
replaceInCache(Tag,_,Mem,[H|T],NewCache,ItemData,fullyAssoc,_):-
		increment([H|T],L),
		convertBinToDec(Tag,OutputDecimalNumber),
		loopOnMemory(Mem,OutputDecimalNumber,DataFromMem,fullyAssoc),
		ItemData=DataFromMem,
		H=item(tag(X),_,_,_),
		atom_number(StringTag,Tag),
		atom_length(X,XLength),
		atom_length(Tag,TagLength),
		NoOfZeros is XLength-TagLength,
		fillZeros(StringTag,NoOfZeros,NewerTag),
		Item1= item(tag(NewerTag),data(ItemData),1,0),
		getmax(L,MostMax),
		indexOf(L,MostMax,I),
		replaceIthItem(Item1,L,I,NewCache).


loopOnMemory(Mem,OutputDecimalNumber,DataFromMem,fullyAssoc):-
		loopOnMemoryHelper(Mem,OutputDecimalNumber,DataFromMem,0,fullyAssoc).
loopOnMemoryHelper([H|_],OutputDecimalNumber,H,OutputDecimalNumber,fullyAssoc).
loopOnMemoryHelper([_|T],OutputDecimalNumber,DataFromMem,Acc,fullyAssoc):-
		Acc<OutputDecimalNumber,
		Acc1 is Acc+1,
		loopOnMemoryHelper(T,OutputDecimalNumber,DataFromMem,Acc1,fullyAssoc).

loopOnCache([H|T],_,ItemData,[H1|T],Tag,fullyAssoc):-
		atom_number(StringTag,Tag),
		H=item(tag(X),_,0,_),
		atom_length(X,XLength),
		atom_length(Tag,TagLength),
		NoOfZeros is XLength-TagLength,
		fillZeros(StringTag,NoOfZeros,NewerTag),
		H1=item(tag(NewerTag),data(ItemData),1,0).
loopOnCache([H|T],_,ItemData,[H2|T2],Tag,fullyAssoc):-
		length([H|T],L),
		L\=0,
		H = item(_,_,1,_),
		loopOnCache(T,_,ItemData,[H2|T2],Tag,fullyAssoc).
		
max_order(item(_,_,1,Max),item(_,_,1,Max1),Item):- 
		Max1>Max,
		Item=item(_,_,1,Max1).	
max_order(item(_,_,1,Max),item(_,_,1,Max1),Item):- 
		Max>Max1,
		Item=item(_,_,1,Max).		
		
getmax([H],H).		
getmax([H|[H1|T]],MostMax):-
		max_order(H,H1,Max),
		getmax([Max|T],MostMax).
	
indexOf([Element|_], Element, 0).
indexOf([_|Tail], Element, Index):-
	indexOf(Tail, Element, Index1),
	Index is Index1+1 .		

increment([H|T],L):-
	incrementHelper([H|T],L,0).
incrementHelper([],[],_).
incrementHelper([H|T],[H|T1],Acc):-
	H = item(_,_,0,_),
	Acc1 is Acc+1,
	incrementHelper(T,T1,Acc1).
incrementHelper([H|T],[H1|T1],Acc):-
	H=item(M,N,1,Order),
	Order1 is Order +1,
	H1=item(M,N,1,Order1),
	Acc1 is Acc+1,
	incrementHelper(T,T1,Acc1).



%SETT_ASSOC:

getDataFromCache(StringAddress,List,Data,HopsNum,setAssoc,SetNum):-
		atom_number(StringAddress,Address),
		convertAddress(Address,SetNum,_,Idx,setAssoc),
		length(List,L),
		S is L//SetNum,
		splitEvery(S,List,Sets),
		convertBinToDec(Idx,IdxDec),
		findSet(IdxDec,Sets,NewList),
		getDataFromCacheHelp(StringAddress,NewList,Data,HopsNum,setAssoc,SetNum).

findSet(Idx,OldCacheSets,Set):-
		findSetHelper(Idx,OldCacheSets,Set,0).
findSetHelper(Idx,[H|T],H,Idx).
findSetHelper(Idx,[H|T],Set,Acc):-
		Acc<Idx,
		Acc1 is Acc+1,
		findSetHelper(Idx,T,Set,Acc1).
		
		
getDataFromCacheHelp(StringAddress,[H|T],Data,HopsNum,setAssoc,SetNum):-
		getDataFromCacheHelper(StringAddress,[H|T],Data,HopsNum,setAssoc,SetNum,0).
getDataFromCacheHelper(StringAddress,[H|_],Data,HopsNum,setAssoc,SetNum,Acc):-
		atom_number(StringAddress,Address),
		convertAddress(Address,SetNum,Tag,_,setAssoc),
		H = item(tag(TagCacheString),data(D),ValidBit,_),
		atom_number(TagCacheString,TagCache),
		TagCache = Tag,
		ValidBit = 1,
		Data = D,
		HopsNum = Acc.
getDataFromCacheHelper(StringAddress,[H|T],Data,HopsNum,setAssoc,SetNum,Acc):-
		atom_number(StringAddress,Address),
		convertAddress(Address,SetNum,Tag,_,setAssoc),
		H = item(tag(TagCacheString),data(_),ValidBit,_),
		atom_number(TagCacheString,TagCache),
		TagCache = Tag,
		ValidBit = 0,
		Acc1 is Acc +1,
		getDataFromCacheHelper(StringAddress,T,Data,HopsNum,setAssoc,SetNum,Acc1).
getDataFromCacheHelper(StringAddress,[H|T],Data,HopsNum,setAssoc,setNum,Acc):-
		atom_number(StringAddress,Address),
		convertAddress(Address,SetNum,Tag,_,setAssoc),
		H = item(tag(TagCacheString),data(_),ValidBit,_),
		atom_number(TagCacheString,TagCache),
		TagCache \= Tag,
		ValidBit = 1,
		Acc1 is Acc +1,
		getDataFromCacheHelper(StringAddress,T,Data,HopsNum,setAssoc,SetNum,Acc1).
getDataFromCacheHelper(StringAddress,[H|T],Data,HopsNum,setAssoc,SetNum,Acc):-
		atom_number(StringAddress,Address),
		convertAddress(Address,SetNum,Tag,_,setAssoc),
		H = item(tag(TagCacheString),data(_),ValidBit,_),
		atom_number(TagCacheString,TagCache),
		TagCache \= Tag,
		ValidBit = 0,
		Acc1 is Acc +1,
		getDataFromCacheHelper(StringAddress,T,Data,HopsNum,setAssoc,SetNum,Acc1).

		



convertAddress(Bin,1,Tag,Idx,setAssoc):-
		Tag = Bin,
		Idx = 0.
convertAddress(Bin,SetNum,Tag,Idx,setAssoc):-
		convertAddressHelper(Bin,SetNum,Tag,Idx,setAssoc,0,'').
convertAddressHelper(Bin,SetNum,Tag,Idx,setAssoc,Acc,EmptyString):-
		logBase2(SetNum,SetNumber),
		Acc = SetNumber,
		Tag = Bin,
		atom_number(EmptyString,Idx).
convertAddressHelper(Bin,SetNum,Tag,Idx,setAssoc,Acc,EmptyString):-
		logBase2(SetNum,SetNumber),
        Acc < SetNumber,
		X is Bin mod 10,
		BinNew is Bin//10,
		Acc1 is Acc +1,
		atom_number(XString,X),
		string_concat(XString,EmptyString,EmptyString1),
		convertAddressHelper(BinNew,SetNum,Tag,Idx,setAssoc,Acc1,EmptyString1).
		
		

replaceInCache2(Tag,_,Mem,OldCache,NewCache,ItemData,fullyAssoc,_):-
		convertBinToDec(Tag,OutputDecimalNumber),
		loopOnMemory(Mem,OutputDecimalNumber,DataFromMem,fullyAssoc),
		ItemData=DataFromMem,
		loopOnCache(OldCache,_,ItemData,NewCache,Tag).
		
replaceInCache2(Tag,_,Mem,[H|T],NewCache,ItemData,fullyAssoc,_):-
		convertBinToDec(Tag,OutputDecimalNumber),
		loopOnMemory(Mem,OutputDecimalNumber,DataFromMem,fullyAssoc),
		ItemData=DataFromMem,
		H=item(tag(X),_,_,_),
		atom_number(StringTag,Tag),
		atom_length(X,XLength),
		atom_length(Tag,TagLength),
		NoOfZeros is XLength-TagLength,
		fillZeros(StringTag,NoOfZeros,NewerTag),
		Item1= item(tag(NewerTag),data(ItemData),1,0),
		getmax([H|T],MostMax),
		indexOf([H|T],MostMax,I),
		replaceIthItem(Item1,[H|T],I,NewCache).

replaceInCache(Tag,Idx,Mem,OldCache,NewCache,ItemData,setAssoc,SetsNum):-
		atom_number(OutputTagNumber,Tag),
		atom_number(OutputIdxNumber,Idx),
		string_concat(OutputTagNumber,OutputIdxNumber,OutputAddressString),
		atom_number(OutputAddressString,OutputAddressNumber),
		convertBinToDec(OutputAddressNumber,OutputDecimalNumber),
		loopOnMemory(Mem,OutputDecimalNumber,DataFromMem,directMap),
		ItemData=DataFromMem,
		length(OldCache,L),
		S1 is L//SetsNum,
		splitEvery(S1,OldCache,OldCacheSets),
		convertBinToDec(Idx,IdxInDecimal),
		findSet(IdxInDecimal,OldCacheSets,Set),
		increment(Set,SetI),
		replaceInCache2(0,0,[ItemData],SetI,SetNew,ItemData,fullyAssoc,_),
		replaceIthItem(SetNew,OldCacheSets,IdxInDecimal,NewCacheSets),
		splitEvery(S1,NewCache,NewCacheSets).

%Implemented_Predicates:

getData(StringAddress,OldCache,Mem,NewCache,Data,HopsNum,Type,BitsNum,hit):-
		getDataFromCache(StringAddress,OldCache,Data,HopsNum,Type,BitsNum),
		NewCache = OldCache.
getData(StringAddress,OldCache,Mem,NewCache,Data,HopsNum,Type,BitsNum,miss):-
		\+getDataFromCache(StringAddress,OldCache,Data,HopsNum,Type,BitsNum),
		atom_number(StringAddress,Address),
		convertAddress(Address,BitsNum,Tag,Idx,Type),
		replaceInCache(Tag,Idx,Mem,OldCache,NewCache,Data,Type,BitsNum).


runProgram([],OldCache,_,OldCache,[],[],Type,_).
runProgram([Address|AdressList],OldCache,Mem,FinalCache,[Data|OutputDataList],[Status|StatusList],Type,NumOfSets):-
		getNumBits(NumOfSets,Type,OldCache,BitsNum),
		(Type = setAssoc, Num = NumOfSets; Type \= setAssoc, Num = BitsNum),
		getData(Address,OldCache,Mem,NewCache,Data,HopsNum,Type,Num,Status),
		runProgram(AdressList,NewCache,Mem,FinalCache,OutputDataList,StatusList,Type,NumOfSets).