module Dictionary


    type Dict = 
        | Leaf of bool
        | Node of bool * System.Collections.Generic.Dictionary<char, Dict>
    
    type tmpDict = System.Collections.Generic.Dictionary<char, Dict>

    let empty () = Leaf false // Empty trie

    let rec insert (word : string) =
        function 
        | Leaf _ when word.Length = 0 -> Leaf true
        | Node (_, dict) when word.Length = 0 -> Node (true, dict) 

        | Leaf b -> // insert onto leaf
            let tmp = tmpDict()
            let chr = word[0]
            tmp.[chr] <- insert word.[1..] (empty())
            Node (b, tmp)

        | Node (b, dict) -> 
            let chr = word[0]
            match dict.TryGetValue(chr) with
            | (true, value)  ->
                dict.[chr] <- insert word.[1..] value
                Node(b,dict)
            | (false, _) ->
                dict.[chr] <- insert word.[1..] (empty())
                Node(b,dict)
    
    let rec lookup (word:string) = 
        function
        | Leaf b when word.Length = 0 -> b
        | Leaf _ -> false

        | Node (b,_) when word.Length = 0 -> b

        | Node (_,dict) -> 
            match dict.TryGetValue(word.[0]) with
            | (true, value) -> lookup word.[1..] value
            | (false, _) -> false

    let step (c:char) =
        function 
        | Node (_, dict) ->
            match dict.TryGetValue(c) with //Try get the given char value
            |(true, value) -> // If true its a complete word
                match value with
                | Leaf b -> Some(b, value)
                | Node (b, _) -> Some(b, value)
            |(false, _) -> None 
        | Leaf _ -> None
