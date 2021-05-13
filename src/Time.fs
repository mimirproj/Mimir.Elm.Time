namespace Elm.Time

[<Struct>]
type Posix = 
    private
    | Posix of int64

[<RequireQualifiedAccess>]
module Time = 
    open System 

    let now() = 
        DateTimeOffset.Now.ToUnixTimeMilliseconds()
        |> Posix

    let posixToMillis (Posix millis) = 
        millis

    let millisToPosix millis = 
        Posix millis

    let toDateTimeOffset(Posix t) = 
        DateTimeOffset.FromUnixTimeMilliseconds(t)