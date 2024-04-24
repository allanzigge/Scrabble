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
        playedWords   : list<list<(coord * (uint32 * (char * int)))>>
    }

    let mkState b d pn h = {board = b; dict = d;  playerNumber = pn; hand = h; playedWords = [] }

    let board st         = st.board
    let dict st          = st.dict
    let playerNumber st  = st.playerNumber
    let hand st          = st.hand

module Scrabble =
    open System.Threading

    let playGame cstream (pieces: Map<uint32, tile>) (st : State.state) =

        let rec aux (st : State.state) =
            Print.printHand (pieces: Map<uint32,tile>) (State.hand st)

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

            let idToCharTouple (id: uint32) = 
                match Map.find id pieces with 
                | tile -> tile.MinimumElement
            
            let rec rmElementFromList lst id  =
                match lst with
                | x :: lst' when x = id -> lst'
                | x :: lst' -> x :: rmElementFromList lst' id
                | _ -> lst
            
            let coordGenerator coord direction =
                    match direction with
                    | "right" -> ((fst coord)+1 ,snd coord)
                    | "down" -> (fst coord,(snd coord)+1)

            let rec MoveGenerator (word:uint32 list) (coord: int * int) (dir:string): list<(int * int) * (uint32 * (char * int))> =
                    match word with
                    | [] -> []
                    | x :: xs -> [(coord,(x , (idToCharTouple x) ))] @ MoveGenerator xs (coordGenerator coord dir) dir

        
            let MakeWord (lst : uint32 list) dict:uint32 list=             
                // if st.playedWords.IsEmpty then
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
                    aux1 lst dict
                // else []
            
            
            

            
            
                     

            let move = MoveGenerator (MakeWord lstOfTiles (State.dict st)) (0,0) "right" 

           
            debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
            send cstream (SMPlay move)

            let msg = recv cstream
            debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.

            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                let st' = {st with playedWords = st.playedWords @ [ms]} // This state needs to be updated
                aux st'
            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                let st' = st // This state needs to be updated
                aux st'
            | RCM (CMPlayFailed (pid, ms)) ->
                (* Failed play. Update your state *)
                let st' = st // This state needs to be updated
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

        fun () -> playGame cstream tiles (State.mkState board dict playerNumber handSet)
        