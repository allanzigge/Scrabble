// Insert your updated Eval.fs file here from Assignment 7. All modules must be internal.

module internal Eval
    open StateMonad
    (* Code for testing *)

    let hello = ('H',4)::('E',1)::('L',1)::('L',1)::('O',1)::[]
    let state = mkState [("x", 5); ("y", 42)] hello ["_pos_"; "_result_"]
    let emptyState = mkState [] [] []
    
    let add a b = 
        a >>= (fun x ->
            b >>= (fun y ->
                ret (x + y)))

    let sub a b = 
        a >>= (fun x ->
            b >>= (fun y ->
                ret (x - y)))   

    let mul a b = 
        a >>= (fun x ->
            b >>= (fun y ->
                ret (x * y)))  
    let div a b = 
        a >>= (fun x ->
            b >>= (fun y ->
                if y <> 0 then ret (x / y) else fail DivisionByZero)) 

    let modu a b = 
        a >>= (fun x ->
            b >>= (fun y ->
                if y <> 0 then ret (x / y) else fail DivisionByZero))   

    let isVowels c =
      let vowels = set ['A';'E';'I';'O';'U']
      c |> System.Char.ToUpper |> vowels.Contains

    type aExp =
        | N of int
        | V of string
        | WL
        | PV of aExp
        | Add of aExp * aExp
        | Sub of aExp * aExp
        | Mul of aExp * aExp
        | Div of aExp * aExp
        | Mod of aExp * aExp
        | CharToInt of cExp

    and cExp =
       | C  of char  (* Character value *)
       | CV of aExp  (* Character lookup at word index *)
       | ToUpper of cExp
       | ToLower of cExp
       | IntToChar of aExp

    type bExp =             
       | TT                   (* true *)
       | FF                   (* false *)

       | AEq of aExp * aExp   (* numeric equality *)
       | ALt of aExp * aExp   (* numeric less than *)

       | Not of bExp          (* boolean not *)
       | Conj of bExp * bExp  (* boolean conjunction *)

       | IsVowel of cExp      (* check for vowel *)
       | IsLetter of cExp     (* check for letter *)
       | IsDigit of cExp      (* check for digit *)

    let (.+.) a b = Add (a, b)
    let (.-.) a b = Sub (a, b)
    let (.*.) a b = Mul (a, b)
    let (./.) a b = Div (a, b)
    let (.%.) a b = Mod (a, b)

    let (~~) b = Not b
    let (.&&.) b1 b2 = Conj (b1, b2)
    let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2)       (* boolean disjunction *)
    let (.->.) b1 b2 = (~~b1) .||. b2           (* boolean implication *) 
       
    let (.=.) a b = AEq (a, b)   
    let (.<.) a b = ALt (a, b)   
    let (.<>.) a b = ~~(a .=. b)
    let (.<=.) a b = a .<. b .||. ~~(a .<>. b)
    let (.>=.) a b = ~~(a .<. b)                (* numeric greater than or equal to *)
    let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)    

    let rec arithEval a : SM<int> = 
        match a with
        | N n ->ret n
        | V v -> lookup v 
        | WL ->  wordLength
        | PV p -> arithEval p >>= pointValue
        | Add (n, m) -> add (arithEval n) (arithEval m)
        | Sub (n, m) -> sub (arithEval n) (arithEval m)
        | Mul (n,m) -> mul (arithEval n) (arithEval m)
        | Div (n,m) -> div (arithEval n) (arithEval m)
        | Mod (n,m) -> modu (arithEval n) (arithEval m)
        | CharToInt c ->  // Handle CharToInt case
            c |> charEval >>= (fun x ->
                ret (int x))
         
    and charEval a : SM<char> = 
       match a with
       | C c -> ret c
       | ToUpper c ->  charEval c >>= (fun char -> ret (System.Char.ToUpper char))
       | ToLower c -> charEval c >>= (fun char -> ret (System.Char.ToLower char))
       | CV cv ->  arithEval cv >>= characterValue 
       | IntToChar aExp ->
            aExp |> arithEval >>= (fun intValue ->
                ret (char intValue))

    let rec boolEval b : SM<bool> = 
        match b with
        | TT -> ret true
        | FF -> ret false
        | AEq (a1,a2) -> 
            arithEval a1 >>= (fun x ->
            arithEval a2 >>= (fun y ->
                ret(x=y)))
        | ALt (a1,a2) -> 
            arithEval a1 >>= (fun x ->
            arithEval a2 >>= (fun y ->
                ret(x>y)))
        | Not b -> boolEval b >>= (fun x->
                            ret (not x))
        |Conj (a1,a2) -> 
            boolEval a1 >>= (fun x ->
            boolEval a2 >>= (fun y ->
                ret(x && y)))
        |IsVowel c -> 
            charEval c >>=(fun x ->
                ret(isVowels x))
        |IsLetter c -> 
            charEval c >>=(fun x ->
                ret(System.Char.IsLetter x))
        |IsDigit c -> 
            charEval c >>=(fun x ->
                ret(System.Char.IsDigit x))
                          


    type stmnt =                  (* statements *)
    | Declare of string           (* variable declaration *)
    | Ass of string * aExp        (* variable assignment *)
    | Skip                        (* nop *)
    | Seq of stmnt * stmnt        (* sequential composition *)
    | ITE of bExp * stmnt * stmnt (* if-then-else statement *)
    | While of bExp * stmnt       (* while statement *)

    let rec stmntEval stmnt : SM<unit> = failwith "Not implemented"

(* Part 3 (Optional) *)

    type StateBuilder() =

        member this.Bind(f, x)    = f >>= x
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Delay(f)      = f ()
        member this.Combine(a, b) = a >>= (fun _ -> b)
        
    let prog = new StateBuilder()

    let arithEval2 a = failwith "Not implemented"
    let charEval2 c = failwith "Not implemented"
    let rec boolEval2 b = failwith "Not implemented"

    let stmntEval2 stm = failwith "Not implemented"

(* Part 4 (Optional) *) 

    let stmntToSquareFun stm = failwith "Not implemented"

    let stmntToBoardFun stm m = failwith "Not implemented"

    type squareStmnt = Map<int, stmnt>
    let stmntsToSquare stms = failwith "Not implemented"

    let mkBoard c x boardStmnt ids = failwith "Not implemented"