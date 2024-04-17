// Insert your MultiSet.fs file here. All modules must be internal

module internal MultiSet

    type MultiSet<'a when 'a : comparison> = R of Map<'a, uint32> 

    let empty = R Map.empty

    let isEmpty (R m) = Map.isEmpty m
    
    let size (R m) : uint32 = Map.fold (fun acc _ count -> count + acc) 0u m

    let contains (search : 'a) (R m : MultiSet<'a>) : bool =
        Map.containsKey search m 

    let numItems (item : 'a) (R m : MultiSet<'a>) : uint32 = 
       m.TryFind(item) |> Option.defaultValue 0u

    let add (a:'a)(n: uint32)(R m: MultiSet<'a>) : MultiSet<'a> = 
        let num : uint32 = numItems a (R m)
        R(Map.add  a (num+n) m)

    let addSingle (item : 'a) (s : MultiSet<'a>) : MultiSet<'a> = 
        add (item) (1u) (s)

    let remove (a : 'a) (n : uint32) (R(s)) : MultiSet<'a> = 
        if (n >= (numItems a (R(s))))
        then R (Map.remove a s)
        else R (Map.add a ((numItems a (R(s)))-n) s)

    let removeSingle (item : 'a) (R m : MultiSet<'a>) : MultiSet<'a> = 
        remove (item) (1u) (R m)        
    
    let fold (f : 'b -> 'a -> uint32 -> 'b) (x : 'b) (R(s)) = 
        Map.fold (f) x s

    let foldBack (f : 'a -> uint32 -> 'b -> 'b) (R(s)) (x : 'b) = 
        Map.foldBack (f) s x
    
    let ofList (_ : 'a list) : MultiSet<'a> = failwith "not implemented"
    let rec toList (m : MultiSet<'a>) : 'a list = 
        fold (fun acc (char : 'a) value -> 
        if value > 1u then 
            let tmp = m
            removeSingle char tmp |> toList |> (fun lst -> lst @ (char :: acc))
        else char :: acc) [] m


    let map (_ : 'a -> 'b) (_ : MultiSet<'a>) : MultiSet<'b> = failwith "not implemented"

    let union (_ : MultiSet<'a>) (_ : MultiSet<'a>) : MultiSet<'a> = failwith "not implemented"
    let sum (_ : MultiSet<'a>) (_ : MultiSet<'a>) : MultiSet<'a> = failwith "not implemented"
    
    let subtract (_ : MultiSet<'a>) (_ : MultiSet<'a>) : MultiSet<'a> = failwith "not implemented"
    
    let intersection (_ : MultiSet<'a>) (_ : MultiSet<'a>) : MultiSet<'a> = failwith "not implemented"
       
    