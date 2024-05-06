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
        timeout: uint32 option
        mutable forfeitPlayers: list<uint32>
    }

    let mkState b d pn h pw pt np cm t fp= {board = b; dict = d;  playerNumber = pn; hand = h; playedWords = pw; playerTurn = pt ; numPlayers = np; coordMap = cm; timeout = t; forfeitPlayers =fp}

    let board st         = st.board
    let dict st          = st.dict
    let playerNumber st  = st.playerNumber
    let hand st          = st.hand

    let numPlayers st = st.numPlayers
    let playedWords = []

    let forfeitPlayers = []

    let coordMap = Map.empty
    let playerTurn st            = st.playerTurn

    let timeout st = st.timeout

module Scrabble =
    open System.Threading

    let playGame cstream (pieces: Map<uint32, tile>) (st : State.state) =

        let rec aux (st : State.state) =
            let mutable ct = new System.Threading.CancellationTokenSource()
            let mutable longestWordSoFar = (([],(0,0)), "")

            let idToCharTouple (id: uint32) = 
                                match Map.find id pieces with 
                                | tile -> tile.MinimumElement

            let coordGenerator coord direction =
                                    match direction with
                                    | "right" -> ((fst coord)+1 ,snd coord)
                                    | "left" -> ((fst coord)-1 ,snd coord)
                                    | "down" -> (fst coord,(snd coord)+1)
                                    | "up" -> (fst coord,(snd coord)-1)

            let rec MoveGenerator (word:(uint32 list*(int*int))) (dir: string): list<(int * int) * (uint32 * (char * int))> =
                                match  (fst word) with
                                | [] -> []
                                | x :: xs when x = 100u -> MoveGenerator (xs,(coordGenerator (snd  word) dir)) dir //If it hits a 100u, it is a char which is already on the board, and we don't want to play that
                                | x :: xs -> [((snd  word),(x , (idToCharTouple x) ))] @ MoveGenerator (xs,(coordGenerator (snd  word) dir)) dir

            let MakeMove (word:((uint32 list*(int*int)) * string)) : list<(int * int) * (uint32 * (char * int))> =
                let dir = snd word

                let result = MoveGenerator (fst word) dir

                if st.playedWords.IsEmpty then 
                    result
                else
                    match result with
                    | x :: xs -> xs
                    | [] -> []

            match st.timeout with
            | Some(x) -> 
                ct <- new System.Threading.CancellationTokenSource(int (x-800u))
            
            Async.Start(
                async {
                    let! ct = Async.CancellationToken
                    use! c = Async.OnCancel(
                        fun () -> 
                            debugPrint "Timeout occurred eriuhgieurhgpiu"
                            let move = MakeMove (longestWordSoFar)
                            if move.IsEmpty then 
                                send cstream SMPass
                            else
                                debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
                                debugPrint (sprintf "Player %d -> MADE THIS MOVE:\n%A\n" (State.playerNumber st) move) 
                                send cstream (SMPlay move))
                    do!
                
                        async {
                            if (State.playerTurn st) = (State.playerNumber st) then
                                // If you would like to test our forfeit functionality, uncomment the next 5 lines. 
                                // let r = System.Random()
                                // let dumNums = r.Next(1, 25)
                                // if dumNums = 1 then
                                //     send cstream (SMForfeit)
                                // else
                                    
                                    
                                    debugPrint("-.-.-.-.-.-. My turn, i am player number" + string (State.playerNumber st) + " .-.-.-.-.-.-\n")
                                    Print.printHand (pieces: Map<uint32,tile>) (State.hand st)

                                    let lstOfTiles = MultiSet.toList (State.hand st)                                

                                    let idToChar id = 
                                        match Map.find id pieces with 
                                        | tile -> fst tile.MinimumElement

                                    let flipDir dir = 
                                        match dir with
                                        | "right" -> "down"
                                        | "down" -> "right"
                                    
                                    let oppositeDir dir = 
                                            match dir with
                                            | "right" -> "left"
                                            | "down" -> "up"
                                            
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
                                            let nextTileChar = isNextTileOccupied dir1 coord2
                                            match nextTileChar with
                                            | (true,char) -> 
                                                let tempReturn =  (mkListOfIds dir1 (coordGenerator coord2 dir1))
                                                (fst tempReturn), ((snd tempReturn) @ [char])
                                            | (false,_) -> (coord2, [])
                                        let listOfIds = (mkListOfIds opDir coord)
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
                                        if st.coordMap.ContainsKey(startCoord) && st.coordMap.ContainsKey(thisCoord) then
                                            true
                                        else
                                            false

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
                                                                        if isWordInPlay startCoord (coordGenerator coord (flipDir dir)) then
                                                                            // debugPrint("in trueee")   
                                                                            let help = aux (lst) dict'' (coordGenerator coord (flipDir dir))//Checks if the recursive rabbithole yielded any word
                                                                            match help with 
                                                                            | [] -> acc
                                                                            | _ -> acc @ 100u :: help //Set the "id" to 100u, such that the moveGenerator can avoid using it, since it is already on the board
                                                                        else
                                                                            // debugPrint("in faaaaalse")
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
                                                                    | (true,_) -> 
                                                                        if (fst (isNextTileOccupied (flipDir dir) (coordGenerator coord (flipDir dir)))) then
                                                                            acc
                                                                        else
                                                                            id1 :: acc
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
                                        

                                    let MakeWord (lst1 : uint32 list) dict: ((uint32 list*(int*int)) * string) = 
                                        let stopWatch = System.Diagnostics.Stopwatch.StartNew()           
                                        if st.playedWords.IsEmpty then   
                                            List.iter(fun id->
                                                let foundWord = (findWordFromChar dict id (rmElementFromList lst1 id) "right" (0,0), (0,0)), "down" 
                                                if (fst (fst foundWord)).Length > (fst (fst longestWordSoFar)).Length then
                                                    longestWordSoFar <- foundWord
                                            ) lst1
                                        else 
                                                List.iter(fun w->
                                                    List.iter(fun letter ->
                                                        let foundWordDir =      (findWordFromChar dict (fst (snd letter)) lst1 (snd w) (fst letter),(fst letter)), (flipDir (snd w))
                                                        let foundWordOppDir =   (findWordFromChar dict (fst (snd letter)) lst1 (flipDir(snd w)) (fst letter),(fst letter)), (snd w)
                                                        if (fst (fst foundWordDir)).Length > (fst (fst longestWordSoFar)).Length then
                                                            longestWordSoFar <- foundWordDir
                                                        elif (fst (fst foundWordOppDir)).Length > (fst (fst longestWordSoFar)).Length then
                                                            longestWordSoFar <- foundWordOppDir
                                                    ) (fst w)
                                                ) st.playedWords
                                        stopWatch.Stop()
                                        printfn "StopWatch %f" stopWatch.Elapsed.TotalMilliseconds
                                        longestWordSoFar
                                    
                                    let move = MakeMove (MakeWord lstOfTiles (State.dict st))
                                    
                                    // Your doThing function made asynchronous
                                    let doThingAsync : Async<unit> =
                                        async {                                 
                                                let move = MakeMove (MakeWord lstOfTiles (State.dict st))
                                                if move.IsEmpty then 
                                                    send cstream SMPass
                                                else
                                                    debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
                                                    debugPrint (sprintf "Player %d -> MADE THIS MOVE:\n%A\n" (State.playerNumber st) move) 
                                                    send cstream (SMPlay move)
                                            
                                        }
                                    Async.Start(doThingAsync,ct)
                            }
                },ct.Token)

            let msg = recv cstream

            let rec NextPlayerFinder pid =                
                let possiblePlayer = ((pid % State.numPlayers st) + 1u)
                if List.contains(possiblePlayer) st.forfeitPlayers then
                    NextPlayerFinder (possiblePlayer)
                else
                    possiblePlayer

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
                debugPrint("Player: " + string st.playerNumber + " in CMPLAYSUCCESS")
                // printfn("Hand before: %A") st.hand
                // printfn("Hand after: %A") (updateHand ms newPieces)
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                let st' = State.mkState (State.board st) (State.dict st) (State.playerNumber st) (updateHand ms newPieces) (st.playedWords @ [directionParser ms]) (NextPlayerFinder (State.playerNumber st)) (State.numPlayers st) (updateCoordMap ms) (State.timeout st) (st.forfeitPlayers)
                // printfn("efter save %A ") (st'.hand)
                // This state needs to be updated
                aux st'
            | RCM (CMPlayed (pid, ms, points)) ->
                debugPrint("Player: " + string st.playerNumber + " in CMPLAYED")
                (* Successful play by other player. Update your state *)
                let st' = State.mkState (State.board st) (State.dict st) (State.playerNumber st) (State.hand st) (st.playedWords @ [directionParser ms]) (NextPlayerFinder(pid)) (State.numPlayers st) (updateCoordMap ms) (State.timeout st)(st.forfeitPlayers)
                // This state needs to be updated
                aux st'
            | RCM (CMPlayFailed (pid, ms)) ->
                (* Failed play. Update your state *)
                let st' = State.mkState (State.board st) (State.dict st) (State.playerNumber st) (State.hand st) (st.playedWords) (NextPlayerFinder(pid)) (State.numPlayers st) (st.coordMap) (State.timeout st)(st.forfeitPlayers)
                // This state needs to be updated
                aux st'
            | RCM (CMPassed (pid)) ->
                (* Somebody passed. Update your state *)
                let st' = State.mkState (State.board st) (State.dict st) (State.playerNumber st) (State.hand st) (st.playedWords) (NextPlayerFinder(pid)) (State.numPlayers st) (st.coordMap) (State.timeout st)(st.forfeitPlayers)
                // This state needs to be updated
                aux st'
            | RCM (CMTimeout (pid)) ->
                let st' = State.mkState (State.board st) (State.dict st) (State.playerNumber st) (State.hand st) (st.playedWords) (NextPlayerFinder(pid)) (State.numPlayers st) (st.coordMap) (State.timeout st)(st.forfeitPlayers)
                // This state needs to be updated
                aux st'
            | RCM (CMForfeit (pid)) ->
                st.forfeitPlayers <- st.forfeitPlayers @ [pid] 
                let st' = State.mkState (State.board st) (State.dict st) (State.playerNumber st) (State.hand st) (st.playedWords) (NextPlayerFinder(pid)) (State.numPlayers st) (st.coordMap) (State.timeout st)(st.forfeitPlayers)
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

        fun () -> playGame cstream tiles (State.mkState board dict playerNumber handSet [] playerTurn numPlayers Map.empty timeout [])
        