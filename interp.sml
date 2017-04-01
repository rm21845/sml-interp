fun hw3(inFile: string, outFile: string) =
let
    (* Instream and outstream *)
    val inStream = TextIO.openIn inFile
    val outStream = TextIO.openOut outFile 
    val line = TextIO.inputLine inStream
    val keywords = ["push","pop",":false:",":true",":error:","add","sub","mul","div","rem",
		   "neg","swap","quit"]
    val stack = []

    val pushVal = []

    (*Pop top of stack or return error if stack is empty *)
    fun pop(y: (int * string) list) = 
      if null y
      then (3,":error:")::y
      else if  #1 (hd y) = 19 
      then  (3, ":error:")::y
      else tl y

    (*Runs through the list and moves all elements up to and including the nearest let statement, effectively ending an enviroment*)	      
    fun endEnv(stack: (int * string) list) =
      if #1 (hd stack) = 19
      then tl stack
      else endEnv(pop(stack))

    (*Determines if all graphical characters in a char list are digits(0-9) Returns true if so; False otherwise *)
    fun isDigit(c: char list) = 
      if null c
      then  true
      else if Char.isGraph(hd c) andalso not(Char.isDigit(hd c))
      then false
      else isDigit(tl c)

    fun charToStr(c: char list) =
      if null c
      then ""
      else if hd c = #"\n"
      then ""
      else if hd c = #"\""
      then ""
      else Char.toString(hd c) ^ charToStr(tl c)


    (* Swaps top two elements in stack. Takes the first two elements in the stack, reverses their order, then concats the reverse with *)
    (* the stacked returned after dropping the first two elements(same as the ones first taken) from the list in the orginal stack *)
    fun swap(s : (int * string) list) =
      if null s orelse null (tl s)
      then  (3, ":error:")::s
      else if  #1 (hd s) = 19 orelse  #1  (hd (tl s)) = 19 orelse  #1 (hd s) = 20 orelse  #1  (hd (tl s)) = 20 
      then  (3, ":error:")::s
      else rev(List.take(s,2)) @ List.drop(s,2)


    fun fixUnit(a : char list, original : string,s : int ) = 
      if null a orelse null (tl a)
      then original
      else if s = 1 andalso hd  a = #":"
      then fixUnit(tl a,original, 2)
      else if s = 2 andalso hd  a = #"u"
      then fixUnit(tl a,original, 3)
      else if s = 3 andalso hd  a = #"n"
      then fixUnit(tl a,original, 4)
      else if s = 4 andalso hd  a = #"i"
      then fixUnit(tl a,original, 5)                           (*1 is push*)
      else if s = 5 andalso hd  a = #"t"
      then fixUnit(tl a,original, 6)
      else if s = 6 andalso hd a = #":"
      then ":unit:"
      else original
    

    (*Why this language represents negative with a tilde is beyond me *)
     fun ripTilde(c : char list) =
      if null c
      then ""
      else if hd c = #"~"
      then  Char.toString(#"-") ^ ripTilde(tl c)
      else Char.toString(hd c) ^ ripTilde(tl c)

     fun tokenizeValue(toTokenize: string) =      
       String.tokens( fn x=> x = #",")toTokenize 


     (*This function is called on after we have discovered that a name has a *)
     fun getBindValue(value: (int* string), stack: (int * string) list) =
       if null stack
       then (~1 , "empty") 
       else  if #1 (hd stack) = 21
       then
	   if #2 (hd stack) = ":unit:"
	   then getBindValue(value, tl stack)
	   else if  (#2 (value)) =  (hd( List.drop((tokenizeValue(#2(hd stack))),1)))
	   then ( (getOpt(Int.fromString((hd( List.drop((tokenizeValue(#2(hd stack))),2)))), 0)) ,  (hd( List.drop((tokenizeValue(#2(hd stack))),3))))
	   else  getBindValue(value, tl stack)
       else getBindValue(value, tl stack)

     (*Checks to see if a value is both bindable and a name. That is, if the top value on the stack is a name that has been previously bond to a value  *)
     (* This is used be isBindable to see if a name given by as a potential  value can be bound to a new name*)
     (*It is also used by arithmetic and boolean operations to see if a name being tried as an operand is a valid type(*int or bool*) for the given operation*)
     fun isBindableName(stack: (int * string) list) =
       if  #1 (hd stack) = 13 andalso  #1 (getBindValue(hd stack, stack), ~2) <> (~1, "empty")
       then true
       else false

     (*Check to see if a value can be binded*)
     fun isBindable(stack: (int * string) list) =
       if  #1 (hd stack) = 4 orelse  #1  (hd stack) = 5 orelse  #1 (hd stack) = 1 orelse  #1  (hd stack) = 12 orelse  #1  (hd  stack) = 21
       then true
       else if isBindableName(stack)
       then true
       else false


     (*binds a value to a name*)
     fun binding(stack: (int * string) list) =
       if null stack orelse null (tl stack)
       then  (3, ":error:")::stack
       else if isBindable(stack) andalso (#1 (hd (tl stack))) = 13
       then 
	   if #1 (hd (stack)) = 13
	   then (21, ":unit:" ^  "," ^  #2 (hd (tl stack)) ^  "," ^ Int.toString( (#1 (getBindValue((hd (stack)), stack)))) ^ 
		      "," ^ #2 (getBindValue((hd (stack)), stack)))::(List.drop(stack,2))
	   else (21, ":unit:" ^  "," ^  #2 (hd (tl stack)) ^  "," ^ Int.toString( #1 (hd (stack))) ^  "," ^  #2 (hd (stack)))::(List.drop(stack,2))
       else  (3, ":error:")::stack



    (*Returns true if an element is boolean type by seeing if it is of either type true or false*)
    (* This is a helper function for boolean operations *)
     fun isElementBool(stack: (int * string) list) =
       if #1 (hd stack) = 4 orelse  #1 (hd stack) = 5
       then true
       else if isBindableName(stack) andalso ( (#1 (getBindValue((hd stack), stack))) = 4 orelse (#1 (getBindValue((hd stack), stack))) = 5)
       then true
       else false

    fun getBool(toBool: int * string, stack) =
      if (#1(toBool)) = 5
      then ":true:"
      else if (#1(toBool)) =  4
      then ":false:"
      else (#2 (getBindValue((#1(toBool),#2( toBool)), stack)))
      

     fun anding(opType : int, stack: (int * string) list) =
       if null stack orelse null (tl stack)
       then  (3, ":error:")::stack
       else if isElementBool(stack) = false orelse isElementBool(tl stack) = false
       then (3, ":error:")::stack
       else if ( getBool((hd( stack)),stack) = ":true:") andalso( getBool((hd (tl stack)),(tl stack)) = ":true:")
       then (5,":true:")::(tl (tl stack))
       else (4,":false:")::(tl (tl stack))

     fun oring(opType : int, stack: (int * string) list) =
       if null stack orelse null (tl stack)
       then  (3, ":error:")::stack
       else if isElementBool(stack) = false orelse isElementBool(tl stack) = false
       then (3, ":error:")::stack
       else if ( getBool((hd( stack)),stack) = ":true:") orelse( getBool((hd (tl stack)),(tl stack)) = ":true:")
       then (5, ":true:")::(tl (tl stack))
       else (4, ":false:")::(tl (tl stack))


     fun noting(opType : int, stack: (int * string) list) =
       if null stack
       then  (3, ":error:")::stack
       else if isElementBool(stack) = false
       then (3, ":error:")::stack
       else if   getBool((hd stack),stack) = ":true:"
       then (4, ":false:")::(tl stack)
       else (5, ":true:")::(tl stack)

    fun iff(s : (int * string) list) =
      if null s orelse null (tl s) orelse null (tl(tl s))
      then  (3, ":error:")::s
      else if isElementBool(List.drop(s,2))
      then
	  if getBool(hd( List.drop(s,2)), List.drop(s,2)) = ":true:"
	  then hd (s)::List.drop(s,3)
	  else hd (tl s)::List.drop(s,3)
      else (3, ":error:")::s


    (* Used by arithmetic operation functions to determine if elements within a stack are integers *)
    (* by checking to see if its code type is that of a push value which implicitly indicates an integer type *)
    fun isElementDigit(stack: (int * string) list) =
      if #1 (hd stack) = 1
      then true
      else if isBindableName(stack) andalso (#1 (getBindValue((hd stack), stack))) = 1
      then true
      else false

    fun getDigit(toInt: string, stack) =
      if Int.fromString(toInt) <> NONE
      then (getOpt(Int.fromString(toInt), 0))
      else getOpt(Int.fromString(#2 (getBindValue((13, toInt), stack))), 0)
      

    (* I'm sure I will forget what that else condition is doing so it is *)
    (* Taking the two top two elements pushed to the stacks that are in string format *)
    (* Converting them both int option -> int, performing the given integer operation according to the code passed in, then converting the result back into a string *)
    (* It then cons onto the stack with the two operands removed. If there are not two elements in the stack or they are both not integers, an :error: message will be *)
    (* consed to the stack passed and this new list will be returned *) 
    fun binaryArithmetic(opType : int, stack: (int * string) list) =
      case opType of
      6 => (if null stack orelse null (tl stack)
	    then  (3, ":error:")::stack
	    else if isElementDigit(stack) = false orelse isElementDigit(tl stack) = false
	    then (3, ":error:")::stack
	    else(1, Int.toString(getDigit((#2  (hd (tl stack))),stack)  + getDigit((#2  (hd stack)),stack)))::(tl (tl stack)))  (* add  + getOpt( Int.fromString(#2 (hd tl stack)), 0) *)
       | 7  => (if null stack orelse null (tl stack)
		then  (3, ":error:")::stack
		else if isElementDigit(stack) = false orelse isElementDigit(tl stack) = false
		then (3, ":error:")::stack
		else(1, Int.toString(getDigit((#2  (hd (tl stack))),stack)  - getDigit((#2  (hd stack)),stack)))::(tl (tl stack)))   (* sub  + getOpt( Int.fromString(#2 (hd tl stack)), 0) *)
       |8  =>  (if null stack orelse null (tl stack)
		then  (3, ":error:")::stack
		else if isElementDigit(stack) = false orelse isElementDigit(tl stack) = false
		then (3, ":error:")::stack
		else(1, Int.toString(getDigit((#2  (hd (tl stack))),stack)  * getDigit((#2  (hd stack)),stack)))::(tl (tl stack)))  (* mul  + getOpt( Int.fromString(#2 (hd tl stack)), 0) *)
       |9  => (if null stack orelse null (tl stack)
	       then  (3, ":error:")::stack
	       else if isElementDigit(stack) = false orelse isElementDigit(tl stack) = false
	       then (3, ":error:")::stack
	       else(1, Int.toString(getDigit((#2  (hd (tl stack))),stack)  div getDigit((#2  (hd stack)),stack)))::(tl (tl stack)))  (* div  + getOpt( Int.fromString(#2 (hd tl stack)), 0) *)
       | 10 =>  (if null stack orelse null (tl stack)
		 then  (3, ":error:")::stack
		 else if isElementDigit(stack) = false orelse isElementDigit(tl stack) = false
		 then (3, ":error:")::stack
		 else(1, Int.toString(getDigit((#2  (hd (tl stack))),stack)  mod getDigit((#2  (hd stack)),stack)))::(tl (tl stack)))   (* mod  + getOpt( Int.fromString(#2 (hd tl stack)), 0) *)
       | 11 =>  (if null stack
		 then  (3, ":error:")::stack
		 else if isElementDigit(stack) = false
		 then (3, ":error:")::stack
		 else (1, Int.toString(Int.toInt(~1) * getDigit((#2  (hd stack)),stack)))::(tl stack))  (* neg  + getOpt( Int.fromString(#2 (hd tl stack)), 0) *)
       | 17 =>  (if null stack orelse null (tl stack)
		 then  (3, ":error:")::stack
		 else if isElementDigit(stack) = false orelse isElementDigit(tl stack) = false
		 then (3, ":error:")::stack
		 else
		     if  getDigit((#2  (hd (tl stack))),stack)  = getDigit((#2  (hd stack)),stack)
		     then (5, ":true:")::(tl (tl stack))
		     else  (4, ":false:")::(tl (tl stack)))
       | 18 =>  (if null stack orelse null (tl stack)
		 then  (3, ":error:")::stack
		 else if isElementDigit(stack) = false orelse isElementDigit(tl stack) = false
		 then (3, ":error:")::stack
		 else
		     if getDigit((#2  (hd (tl stack))),stack)  < getDigit((#2  (hd stack)),stack)
		     then (5, ":true:")::(tl (tl stack))
		     else  (4, ":false:")::(tl (tl stack)))
       | _ =>  ((89, "fuck")::[])

    (*FSM tokenizer. If a valid lexeme is found, it is stored in a pair (lexeme's token code, value)*)
    fun fsm([], s) = (0, "")
      | fsm(a::b, s) =  (if s = 1 andalso a = #"p"
			 then fsm(b, 2)
			 else if s = 2 andalso a = #"u"
			 then fsm(b, 3)
			 else if s = 3 andalso a = #"s"
			 then fsm(b, 4)
			 else if s = 4 andalso a = #"h"
			 then fsm(b, 45)                           (*1 is push*)
			 else if s = 2 andalso a = #"o"
			 then fsm(b, 6)
			 else if s = 6 andalso a = #"p"
			 then (2,"")                               (*2 is pop*)
			 else if s = 1 andalso a = #":"
			 then fsm(b, 8)
			 else if s = 8 andalso a = #"e"
			 then fsm(b, 9)
			 else if s = 9 andalso a = #"r"
			 then fsm(b, 15)
			 else if s = 15 andalso a = #"r"
			 then fsm(b, 16)
			 else if s = 16 andalso a = #"o"
			 then fsm(b, 17)
			 else if s = 17 andalso a = #"r"
			 then fsm(b, 18)
			 else if s = 18 andalso a = #":"
			 then  (3,"")                              (* 3 is :error: *)
			 else if s = 8 andalso a = #"f"
			 then fsm(b, 10)
			 else if s = 10 andalso a = #"a"
			 then fsm(b, 11)
			 else if s = 11 andalso a = #"l"
			 then fsm(b, 12)
			 else if s = 12 andalso a = #"s"
			 then fsm(b,13)
			 else if s = 13 andalso a = #"e"
			 then fsm(b, 14)
			 else if s = 14 andalso a = #":"
			 then  (4,"")                                 (* 4 is :false:*)
			 else if s = 8 andalso a = #"t"
			 then fsm(b, 20)
			 else if s = 20 andalso a = #"r"
			 then fsm(b, 21)
			 else if s = 21 andalso a = #"u"
			 then fsm(b, 22)
			 else if s = 22 andalso a = #"e"
			 then fsm(b,23)
			 else if s = 23  andalso a = #":"
			 then  (5,"")                              (* 5 is :true:*)
			 else if s = 1 andalso a = #"a"
			 then fsm(b, 24)
			 else if s = 24 andalso a = #"d"
			 then fsm(b, 25)
			 else if s = 25 andalso a = #"d"
			 then  (6,"")                            (*6 is add*)
			 else if s = 1 andalso a = #"s"
			 then fsm(b, 27)
			 else if s = 27 andalso a = #"u"
			 then fsm(b, 28)
			 else if s = 28 andalso a = #"b"
			 then  (7,"")                          (*7 is sub*)
			 else if s = 1 andalso a = #"m"
			 then fsm(b, 30)
			 else if s = 30 andalso a = #"u"
			 then fsm(b, 31)
			 else if s = 31 andalso a = #"l"
			 then  (8,"")                           (*8 is mul*)
			 else if s = 1 andalso a = #"d"
			 then fsm(b, 33)
			 else if s = 33 andalso a = #"i"
			 then fsm(b,34)                           
			 else if s = 34 andalso a = #"v"
			 then   (9,"")                           (* 9 is divide *)
			 else if s = 1 andalso a = #"r"
			 then fsm(b, 36)
			 else if s = 36 andalso a = #"e"
			 then fsm(b,37)                           
			 else if s = 37 andalso a = #"m"
			 then   (10,"")                            (* 10 is remainder*)   
			 else if s = 1 andalso a = #"n"
			 then fsm(b, 39)
			 else if s = 39 andalso a = #"e"
			 then fsm(b,40)                           
			 else if s = 40 andalso a = #"g"
			 then   (11,"")                           (* 11 is negation *)
			 else if s = 27 andalso a = #"w"
			 then fsm(b, 43)                           
			 else if s = 43 andalso a = #"a"
			 then  fsm(b, 44)
			 else if s = 44 andalso a = #"p"
			 then (12," ")       
			 else if s = 45 andalso Char.isSpace(a) (* if still a whitespace, ignore*)
			 then   fsm(b,45) 
			 else if s = 45 andalso a = #"\""
			 then  (12,charToStr(b)) (* represents a quote statement with code 12*)
			 else if s = 45 andalso Char.isAlpha(a)
			 then   (13,Char.toString(a)^ charToStr(b)) (*13 is of name type *)
			 else if s = 45 andalso a = #"-"
			 then if isDigit(b)  
			      then  (1,Char.toString(a)^ charToStr(b))
			      else (3, ":error")        
			 else if s = 45
			 then if isDigit(b)
			      then   (1, charToStr(a::b))
			      else (3, ":error:")                (* 1 is digit push type*)
			 else if s = 24 andalso a = #"n"
			 then fsm(b, 46)
			 else if s = 46 andalso a = #"d"
			 then (14,"")                            (*14 is and*)
			 else  if s = 1 andalso a = #"o"
			 then fsm(b, 47)
			 else  if s = 47 andalso a = #"r"
			 then (15, "")                           (*15 is or*)
			 else  if s = 39 andalso a = #"o"
			 then fsm(b, 48)
			 else  if s = 48 andalso a = #"t"
			 then (16, "")                          (*16 is not*)
			 else  if s = 1 andalso a = #"e"
			 then fsm(b, 49)
			 else  if s = 49 andalso a = #"q"
			 then fsm(b, 50)
			 else  if s = 50 andalso a = #"u"
			 then fsm(b, 51)
			 else  if s = 51 andalso a = #"a"
			 then fsm(b, 52)
			 else  if s = 52 andalso a = #"l"
			 then (17, "")                             (*17 is equal*)
			 else  if s = 1 andalso a = #"l"
			 then fsm(b, 53)
			 else  if s = 53 andalso a = #"e"
			 then fsm(b, 54)
			 else  if s = 54 andalso a = #"s"
			 then fsm(b, 55)
			 else  if s = 55 andalso a = #"s"
			 then (18, "")                             (*18 is lessThan*)(*note that we only track up to less*)
			 else  if s = 54 andalso a = #"t"
			 then (19, "let")                          (*19 is let*)
			 else  if s = 49 andalso a = #"n"
			 then fsm(b, 56)
			 else  if s = 56 andalso a = #"d"
			 then (20, "end")                         (*20 is end*)
			 else if s = 1 andalso a = #"b"
			 then fsm(b, 57)
			 else if s = 57 andalso a = #"i"
			 then fsm(b, 58)                        
			 else if s = 58 andalso a = #"n"
			 then fsm(b,59 )
			 else if s = 59 andalso a = #"d"
			 then (22, ":unit:")              (*bind is 22 *) (*unit is 21*)
			 else if s = 1 andalso a = #"i"
			 then fsm(b, 60)
			 else if s = 60 andalso a = #"f"
			 then (23, "")                    (*23 is if*)                   
			 else  (99,"") )


    (*pops and pushes things from the stack. May also perform small minor semantic operations*)
    fun operation(x: (int * string) list, y: (int * string) list) =(if null x
								    then y
								    else if #1 (hd x) = 1 orelse  #1 (hd x) = 12 orelse  #1 (hd x) = 13          (*if push*)          
								    then operation(tl x,(#1(hd x),#2(hd x))::y)
								    else if #1 (hd x) = 2            (*if pop*)
								    then operation(tl x,pop(y))
								    else if #1 (hd x) = 3
								    then operation(tl x,(#1(hd x),":error:")::y)
								    else if #1 (hd x) = 4
								    then operation(tl x,(#1(hd x),":false:")::y)
								    else if #1 (hd x) = 5
								    then operation(tl x,(#1(hd x),":true:")::y)	
								    else if #1 (hd x) = 6  	
								    then  operation(tl x,binaryArithmetic(6, y))
								    else if #1 (hd x) = 7  	
								    then  operation(tl x,binaryArithmetic(7, y))
								    else if #1 (hd x) = 8  	
								    then  operation(tl x,binaryArithmetic(8, y))
								    else if #1 (hd x) = 9  	
								    then  operation(tl x,binaryArithmetic(9, y))
								    else if #1 (hd x) = 10  	
								    then  operation(tl x,binaryArithmetic(10, y))
								    else if #1 (hd x) = 11 
								    then  operation(tl x,binaryArithmetic(11, y))
								    else if #1 (hd x) = 12   	
								    then  operation(tl x,swap(y))
								    else if #1 (hd x) = 14 
								    then  operation(tl x,anding(14, y))
								    else if #1 (hd x) = 15 
								    then  operation(tl x, oring(15, y))
								    else if #1 (hd x) = 16 
								    then  operation(tl x, noting(16, y))
								    else if #1 (hd x) = 17 
								    then  operation(tl x,binaryArithmetic(17, y))
								    else if #1 (hd x) = 18
								    then  operation(tl x,binaryArithmetic(18, y))
								    else if #1 (hd x) = 19
								    then operation(tl x,(#1(hd x),"let")::y)
								    else if #1 (hd x) = 20
								    then operation(tl x,(hd(y))::endEnv(y))
								    else if #1 (hd x) = 22
								    then operation(tl x, binding(y))
								    else if #1 (hd x) = 23
								    then operation(tl x, iff(y))
								    else y ) (*add rest of the commands later*)

    fun helper(line : string option) =                                                                             
      case line of                                                                                                 
 	  NONE =>      []                                               
       |  SOME(c) =>  (fsm(explode(getOpt(line," ")),1):: helper(TextIO.inputLine inStream))

      
(*remove the recursive call here to the inputLine inStream. Instead, have a function temp load input into a queue list, and put that in the "in" body then move output back there*)
    fun prtStack(s :(int * string) list) =                                                                          
      if null s                                                                                                    
      then (TextIO.closeIn inStream; TextIO.closeOut outStream)                                                    
      else (TextIO.output(outStream, ripTilde( explode(fixUnit(explode(#2(hd s)),(#2 ( hd s)),1)))^ "\n");                    
 		      prtStack(tl s))                                                                    

(*TextIO.output(TextIO.stdOut, ls)*)
in
prtStack(operation(helper(line),stack))	
end





