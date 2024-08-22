module Main exposing (Category(..), Operator(..), Type(..), add, div, mult, rem, sub)


type Type
    = Integer
    | Float


type Operator
    = Operator { label : String, signature : String, category : Category, params : List Type, results : List Type }


type Category
    = Math
    | Flow


add =
    Operator { label = "i+", signature = "", category = Math, params = [ Integer, Integer ], results = [ Integer ] }


sub =
    Operator { label = "i-", signature = "", category = Math, params = [ Integer, Integer ], results = [ Integer ] }


mult =
    Operator { label = "i*", signature = "", category = Math, params = [ Integer, Integer ], results = [ Integer ] }


div =
    Operator { label = "i/", signature = "", category = Math, params = [ Integer, Integer ], results = [ Integer ] }


rem =
    Operator { label = "i%", signature = "", category = Math, params = [ Integer, Integer ], results = [ Integer ] }
