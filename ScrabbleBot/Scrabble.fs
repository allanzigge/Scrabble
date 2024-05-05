namespace TileTitan

open ScrabbleUtil
open ScrabbleUtil.ServerCommunication

open System.IO

open ScrabbleUtil.DebugPrint

// The RegEx module is only used to parse human input. It is not used for the final product.

module RegEx =
    open System.Text.RegularExpressions

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let parseMove ts =
        let pattern = @"([-]?[0-9]+[ ])([-]?[0-9]+[ ])([0-9]+)([A-Z]{1})([0-9]+)[ ]?" 
        Regex.Matches(ts, pattern) |>
        Seq.cast<Match> |> 
        Seq.map 
            (fun t -> 
                match t.Value with
                | Regex pattern [x; y; id; c; p] ->
                    ((x |> int, y |> int), (id |> uint32, (c |> char, p |> int)))
                | _ -> failwith "Failed (should never happen)") |>
        Seq.toList

 module Print =

    let printHand pieces hand =
        hand |>
        MultiSet.fold (fun _ x i -> forcePrint (sprintf "%d -> (%A, %d)\n" x (Map.find x pieces) i)) ()

module State = 
    // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, your player numer, your board, and your dictionary,
    // but it could, potentially, keep track of other useful
    // information, such as number of players, player turn, etc.

    type state = {
        board         : Parser.board
        dict          : Dictionary.Dict
        playerNumber  : uint32
        hand          : MultiSet.MultiSet<uint32>
        playedWords   : list<(list<((int * int) * (uint32 * (char * int)))> * string)>
        playerTurn    : uint32
        numPlayers : uint32
        coordMap: Map<(int*int),uint32>
    }

    let mkState b d pn h pw pt np cm= {board = b; dict = d;  playerNumber = pn; hand = h; playedWords = pw; playerTurn = pt ; numPlayers = np; coordMap = cm}

    let board st         = st.board
    let dict st          = st.dict
    let playerNumber st  = st.playerNumber
    let hand st          = st.hand

    let numPlayers st = st.numPlayers
    let playedWords = []

    let coordMap = Map.empty
    let playerTurn st            = st.playerTurn

module Scrabble =
    open System.Threading

    let playGame cstream (pieces: Map<uint32, tile>) (st : State.state) =

        let rec aux (st : State.state) =

            if (State.playerTurn st) = (State.playerNumber st) then
                debugPrint("-.-.-.-.-.-. My turn, i am player number" + string (State.playerNumber st) + " .-.-.-.-.-.-\n")
                Print.printHand (pieces: Map<uint32,tile>) (State.hand st)

                debugPrint("-.-.-.-.-.-. Player number"+ string (State.playerNumber st) + "has coodmap:" + string (st.coordMap) + " .-.-.-.-.-.-\n")

                // remove the force print when you move on from manual input (or when you have learnt the format)
                // forcePrint "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"
                // let input =  System.Console.ReadLine()
                let lstOfTiles = MultiSet.toList (State.hand st)
                // let lstOfChars : char list = 
                //     List.map (fun tile -> 
                //     let set = (Map.find tile pieces)
                //     match set with 
                //     | [(char, _)] -> char
                //     | _ -> ' '
                //     ) lstOfTiles
               

                let idToChar id = 
                    match Map.find id pieces with 
                    | tile -> fst tile.MinimumElement

                let coordGenerator coord direction =
                        match direction with
                        | "right" -> ((fst coord)+1 ,snd coord)
                        | "left" -> ((fst coord)-1 ,snd coord)
                        | "down" -> (fst coord,(snd coord)+1)
                        | "up" -> (fst coord,(snd coord)-1)

                let flipDir dir = 
                    match dir with
                    | "right" -> "down"
                    | "down" -> "right"
                
                let oppositeDir dir = 
                        match dir with
                        | "right" -> "left"
                        | "down" -> "up"
                        

                let idToCharTouple (id: uint32) = 
                    match Map.find id pieces with 
                    | tile -> tile.MinimumElement
                
                let rec rmElementFromList lst id  =
                    match lst with
                    | x :: lst' when x = id -> lst'
                    | x :: lst' -> x :: rmElementFromList lst' id
                    | _ -> lst


                let isValidCharPlacement (coord:(int * int)) (char:uint32) (dir:string)=

                    let rec list1 nextCoord  = 
                        let nextChar = Map.tryFind (coordGenerator nextCoord dir) st.coordMap
                        match nextChar with
                        | None -> [] //empty
                        | Some (x) -> [x] @ list1 (coordGenerator nextCoord dir)
                    
                    let rec list2 nextCoord = 
                        let nextChar = Map.tryFind (coordGenerator nextCoord (oppositeDir dir)) st.coordMap
                        match nextChar with
                        | None -> [] //empty
                        | Some (x) -> list2 (coordGenerator nextCoord (oppositeDir dir)) @ [x]
                    
                    let finalList  =
                        (list2 coord)@[char]@(list1 coord)
                    
                    if (finalList.Length = 1) then
                        true
                    else 
                        let word = (List.map idToChar finalList).ToString()
                        ScrabbleUtil.Dictionary.lookup word st.dict
                    

                let isNextTileOccupied (dir:string) (coord:(int*int)) =
                    
                    let nextTile = coordGenerator coord dir
                    if(st.coordMap.ContainsKey nextTile) then
                        (true,st.coordMap[nextTile])
                    else
                        (false, 100u)
                
                let lookInOppositeDirection (dir:string)  (coord:(int*int)) dict =
                    let opDir = oppositeDir (flipDir dir)
                    //The mkListOfIds keeps going in the opposite direction and adds any id's it meets
                    // to a list (creating the word the starting char possibly is a part of)
                    let rec mkListOfIds(dir1:string) (coord2:(int*int))=
                        debugPrint ("coordooordcorood " + string (coord2))
                        debugPrint (dir1)
                        let nextTileChar = isNextTileOccupied dir1 coord2
                        match nextTileChar with
                        | (true,char) -> 
                            let tempReturn =  (mkListOfIds dir1 (coordGenerator coord2 dir1))
                            (fst tempReturn), ((snd tempReturn) @ [char])
                        | (false,_) -> (coord2, [])
                    let listOfIds = (mkListOfIds opDir coord)
                    debugPrint ("ididididididididid" + string (listOfIds))
                    //Folding over the list of id's and stepping through their dictionaries should
                    //give the start dict, now influences by possible other chars which it's already 
                    //are making a word with
                    let returnDictionary = 
                        List.fold (fun acc id ->
                            let acc =
                                match (ScrabbleUtil.Dictionary.step (idToChar id) acc) with
                                | Some(true, dict) -> dict
                                | Some(false, dict) -> dict
                                | _ -> acc
                            acc
                        ) dict (snd listOfIds)
                    (fst listOfIds), returnDictionary



                    
                let isWordInPlay startCoord thisCoord :bool =
                    //løb playedWord igennem
                    //fold - kig i hvert ord check om startCoord = første coord i ordet 
                    // andet fold, check om vi rammer thisCoord
                    List.fold(fun (acc: bool) (word:list<(int * int) * (uint32 * (char * int))> * string) -> 
                        if acc then 
                            acc
                        else
                            match startCoord with 
                            | x when x = (fst ((fst word)[0])) -> 
                                List.fold(fun acc letter ->
                                    if acc then 
                                        acc
                                    else 
                                        match fst letter with 
                                        |c when c = thisCoord -> true
                                        | _ -> acc
                                ) false (fst word) 
                            |_ -> acc
                    )  false st.playedWords  



                let findWordFromChar dict id hand dir coord=
                    let klamTuple = (lookInOppositeDirection dir coord dict)
                    let startCoord = fst klamTuple
                    let fstDict =ScrabbleUtil.Dictionary.step (idToChar id) (snd klamTuple)
                    match fstDict with 
                    | Some (_,dict') -> 
                        let rec aux lst dict coord : uint32 list=
                            List.fold (fun (acc) (id1) ->
                                if acc.IsEmpty then
                                    let nextTileChar = isNextTileOccupied (flipDir dir) coord
                                    
                                    if(fst nextTileChar) then
                                        let nextDict = ScrabbleUtil.Dictionary.step (idToChar (snd nextTileChar)) dict
                                        match nextDict with
                                        | Some(x) ->
                                            if(isValidCharPlacement (coordGenerator coord (flipDir dir)) (snd nextTileChar) dir) then
                                                match x with
                                                | (true, dict'') ->
                                                    if isWordInPlay startCoord coord then   
                                                        let help = aux (lst) dict'' (coordGenerator coord (flipDir dir))//Checks if the recursive rabbithole yielded any word
                                                        match help with 
                                                        | [] -> acc
                                                        | _ -> acc @ 100u :: help //Set the "id" to 100u, such that the moveGenerator can avoid using it, since it is already on the board
                                                    else
                                                        100u :: acc
                                                | (false, dict'') -> 
                                                    let help = aux (lst) dict'' (coordGenerator coord (flipDir dir))//Checks if the recursive rabbithole yielded any word
                                                    match help with 
                                                    | [] -> acc
                                                    | _ -> acc @ 100u :: help //Set the "id" to 100u, such that the moveGenerator can avoid using it, since it is already on the board
                                            else
                                                acc
                                        | _ -> []
                                    else
                                        let nextDict = ScrabbleUtil.Dictionary.step (idToChar id1) dict
                                        match nextDict with
                                        | Some(x) ->
                                            if(isValidCharPlacement (coordGenerator coord (flipDir dir)) id1 dir) then
                                                match x with
                                                | (true,_) -> id1 :: acc
                                                | (false, dict'') -> 
                                                    let help = aux (rmElementFromList lst id1) dict'' (coordGenerator coord (flipDir dir))//Checks if the recursive rabbithole yielded any word
                                                    match help with 
                                                    | [] -> acc
                                                    | _ -> acc @ id1 :: help
                                            else
                                                acc
                                        | _ -> []
                                else
                                    acc
                            ) [] lst
                        let word = aux hand dict' coord
                        if word.IsEmpty then
                            []
                        else
                            id :: word
                    | _ -> []


                let rec MoveGenerator (word:(uint32 list*(int*int))) (dir: string): list<(int * int) * (uint32 * (char * int))> =
                    match  (fst word) with
                    | [] -> []
                    | x :: xs when x = 100u -> MoveGenerator (xs,(coordGenerator (snd  word) dir)) dir //If it hits a 100u, it is a char which is already on the board, and we don't want to play that
                    | x :: xs -> [((snd  word),(x , (idToCharTouple x) ))] @ MoveGenerator (xs,(coordGenerator (snd  word) dir)) dir

                let MakeMove (word:((uint32 list*(int*int)) * string)) : list<(int * int) * (uint32 * (char * int))> =
                    let dir =
                        match snd word with
                        | "right" -> "down"
                        | "down" -> "right" 

                    let result = MoveGenerator (fst word) dir

                    if st.playedWords.IsEmpty then 
                        result
                    else
                        match result with
                        | x :: xs -> xs
                        | [] -> []


                let MakeWord (lst : uint32 list) dict: ((uint32 list*(int*int)) * string) =             
                    if st.playedWords.IsEmpty then
                        let rec aux1 lst dict : uint32 list=
                            List.fold (fun (acc) (id) ->
                                if acc.IsEmpty then
                                    let temp = ScrabbleUtil.Dictionary.step (idToChar id) dict
                                    match temp with
                                    | Some (true,_) -> id :: acc
                                    | Some (false, dict') -> 
                                        let help = aux1 (rmElementFromList lst id) dict'
                                        match help with 
                                        | [] -> acc
                                        | _ -> id :: acc @ help
                                    | _ -> []
                                else
                                    acc
                            ) [] lst
                        (aux1 lst dict,(0,0)),"right"
                    else 
                        let aux2 (pw: list<list<((int * int) * (uint32 * (char * int)))> * string>) (hand: uint32 list) dict : list<((uint32 list*(int*int)) * string)>=
                            List.fold (fun (acc:list<((uint32 list*(int*int)) * string)>) (w:list<((int * int) * (uint32 * (char * int)))> * string ) ->
                                //folds over the letters of the a played word, to give a start letter to our new word
                                let returnedWord = (List.fold (fun (acc1:((uint32 list * (int*int))*string)) letter ->
                                    if (fst (fst acc1)).IsEmpty then
                                        let wordFromChar = findWordFromChar dict (fst (snd letter)) hand (snd w) (fst letter)
                                        if wordFromChar.IsEmpty then
                                            debugPrint "Empty sgowegoweigjsoegij"
                                            (((fst (fst acc1)) @ findWordFromChar dict (fst (snd letter)) hand (flipDir(snd w)) (fst letter),(fst letter)),(flipDir (snd w))) //returns the builded word with its start coord
                                        else 
                                            debugPrint ("Not empty -------- " + string wordFromChar)
                                            (((fst (fst acc1)) @ wordFromChar,(fst letter)), (snd w)) //returns the builded word with its start coord
                                    else
                                        acc1
                                ) (([],(0,0)), "") (fst w))
                                acc @ [((fst (fst returnedWord), snd (fst returnedWord)),snd returnedWord)] //return builded word with direction
                                
                            ) [(([],(0,0)),"")] pw 
                        let listOfPossibleWords =  aux2 (st.playedWords) lst dict
                        let longestWord = 
                            List.fold(fun (acc:((uint32 list*(int*int)) * string)) (word: ((uint32 list*(int*int)) * string)) ->
                            match word with
                            | x when (fst (fst x)).Length > (fst (fst acc)).Length -> 
                                word
                            |_ -> acc               
                            ) (([],(0,0)),"") listOfPossibleWords
                        longestWord
                
                        

                let move = MakeMove (MakeWord lstOfTiles (State.dict st))
                   
                
                if move.IsEmpty then 
                    send cstream SMPass
                else
                    debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
                    debugPrint (sprintf "Player %d -> MADE THIS WORDMOVE:\n%A\n" (State.playerNumber st) move) 
                    send cstream (SMPlay move)

            
                debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.

            let msg = recv cstream

            let directionParser (word: list<((int * int) * (uint32 * (char * int)))>) : list<((int * int) * (uint32 * (char * int)))> * string =
                    if word.Length = 1 then
                        word,"right"
                    else
                        let coord1 = (fst word[0])
                        let coord2 = (fst word[1])
                        match coord2 with
                        | (x,y) when x > (fst coord1) -> word , "right"
                        | (x,y) when y > (snd coord1) -> word , "down"
                        | _ -> word,"right"

            let updateHand (rm:list<coord * (uint32 * (char * int))>) (add: list<uint32 * uint32>) :  MultiSet.MultiSet<uint32> =
                let afterRemove = List.fold (fun acc x -> MultiSet.removeSingle (fst (snd x)) acc) st.hand rm
                List.fold (fun acc (x, k) -> MultiSet.add x k acc) afterRemove add

            let updateCoordMap (w:list<(int * int) * (uint32 * (char * int))>) =
                List.fold (fun (acc:Map<int * int,uint32>) (x, (k,_)) -> Map.add x k acc) st.coordMap w

            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                debugPrint(sprintf "-.-.-.-.-.-.-.- ms: " + string (ms) + "-.-.-.-.-.-.-.-")
                let st' = State.mkState (State.board st) (State.dict st) (State.playerNumber st) (updateHand ms newPieces) (st.playedWords @ [directionParser ms]) ((State.playerNumber st % State.numPlayers st) + 1u) (State.numPlayers st) (updateCoordMap ms)
                // This state needs to be updated
                aux st'
            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                let st' = State.mkState (State.board st) (State.dict st) (State.playerNumber st) (State.hand st) (st.playedWords @ [directionParser ms]) ((pid % State.numPlayers st) + 1u) (State.numPlayers st) (updateCoordMap ms)
                // This state needs to be updated
                aux st'
            | RCM (CMPlayFailed (pid, ms)) ->
                (* Failed play. Update your state *)
                let st' = State.mkState (State.board st) (State.dict st) (State.playerNumber st) (State.hand st) (st.playedWords) ((pid % State.numPlayers st) + 1u) (State.numPlayers st) (st.coordMap)
                // This state needs to be updated
                aux st'
            | RCM (CMPassed (pid)) ->
                (* Somebody passed. Update your state *)
                let st' = State.mkState (State.board st) (State.dict st) (State.playerNumber st) (State.hand st) (st.playedWords) ((pid % State.numPlayers st) + 1u) (State.numPlayers st) (st.coordMap)
                // This state needs to be updated
                aux st'
            | RCM (CMGameOver _) -> ()
            | RCM a -> failwith (sprintf "not implmented: %A" a)
            | RGPE err -> printfn "Gameplay Error:\n%A" err; aux st



        aux st

    let startGame 
            (boardP : boardProg) 
            (dictf : bool -> Dictionary.Dict) 
            (numPlayers : uint32) 
            (playerNumber : uint32) 
            (playerTurn  : uint32) 
            (hand : (uint32 * uint32) list)
            (tiles : Map<uint32, tile>)
            (timeout : uint32 option) 
            (cstream : Stream) =
        debugPrint 
            (sprintf "Starting game!
                      number of players = %d
                      player id = %d
                      player turn = %d
                      hand =  %A
                      timeout = %A\n\n" numPlayers playerNumber playerTurn hand timeout)

        //let dict = dictf true // Uncomment if using a gaddag for your dictionary
        let dict = dictf false // Uncomment if using a trie for your dictionary
        let board = Parser.mkBoard boardP
                  
        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand

        fun () -> playGame cstream tiles (State.mkState board dict playerNumber handSet [] playerTurn numPlayers Map.empty)
        