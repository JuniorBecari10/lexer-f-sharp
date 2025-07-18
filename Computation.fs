module Computation

type ResultBuilder() =
    member _.Bind(m: Result<'a, 'e>, f: 'a -> Result<'b, 'e>) : Result<'b, 'e> =
        Result.bind f m

    member _.Return(x: 'a) : Result<'a, 'e> =
        Ok x

    member _.ReturnFrom(m: Result<'a, 'e>) : Result<'a, 'e> =
        m

    member _.Zero() : Result<unit, 'e> =
        Ok ()

let result = ResultBuilder()
