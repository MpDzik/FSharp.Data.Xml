namespace FSharp.Data.Xml

open System

module internal Argument =

    let validateNotNull value paramName =
        if value = null then raise (ArgumentNullException(paramName))

    let validateNotNullOrEmpty value (paramName : string) =
        if String.IsNullOrEmpty(value)
        then raise (ArgumentException("The value cannot be null or empty.", paramName))

    let validateNotNullOrWhitespace value (paramName : string) =
        if String.IsNullOrWhiteSpace(value)
        then raise (ArgumentException("The value cannot be null or whitespace.", paramName))

    let validate condition (paramName : string) (message : string) =
        if not(condition) then raise (ArgumentException(message, paramName))