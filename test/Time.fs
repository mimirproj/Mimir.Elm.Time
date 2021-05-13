module Tests.Time

open Expecto
open Prelude
open Elm.Time

[<Tests>]
let tests =
    let simpleTests =
        describe "Simple Stuff"
            [ test "roundtrip" <| fun _ -> 
                let ms = System.DateTimeOffset.Now.ToUnixTimeMilliseconds()
                let t = Time.millisToPosix ms
                let ms1 = Time.posixToMillis t

                Expect.equal ms ms1
              
            ]

    describe "Time" [
        simpleTests
    ]