    module BetterDictionary

    type Dictionary

    val empty : Unit -> Dictionary 

    val insert : string -> Dictionary -> Dictionary 

    val lookup : string -> Dictionary -> bool

    val step : char -> Dictionary -> (bool * Dictionary) option