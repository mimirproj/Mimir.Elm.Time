module Tests.Time

open Expecto
open Prelude
open Elm.Time

[<Tests>]
let tests =
    let simpleTests =
        describe "Simple Stuff"
            [ 
                test "roundtrip" <| fun _ -> 
                    let ms = int (System.DateTimeOffset.Now.ToUnixTimeMilliseconds())
                    let t = Time.millisToPosix ms
                    let ms1 = Time.posixToMillis t

                    Expect.equal ms ms1


                test "toYear" <| fun _ -> 
                    Expect.equal (Time.toYear Time.utc (Time.millisToPosix 0)) 1970

                test "toMonth" <| fun _ -> 
                    Expect.equal (Time.toMonth Time.utc (Time.millisToPosix 0)) Jan

                test "toDay" <| fun _ -> 
                    Expect.equal (Time.toDay Time.utc (Time.millisToPosix 0)) 1

                test "toWeekday" <| fun _ -> 
                    Expect.equal (Time.toWeekday Time.utc (Time.millisToPosix 0)) Thu

                test "toHour" <| fun _ -> 
                    Expect.equal (Time.toHour Time.utc (Time.millisToPosix 0)) 0

                test "toMinute" <| fun _ -> 
                    Expect.equal (Time.toMinute Time.utc (Time.millisToPosix 0)) 0

                test "toSecond 0" <| fun _ -> 
                    Expect.equal (Time.toSecond Time.utc (Time.millisToPosix 0)) 0

                test "toSecond 1" <| fun _ -> 
                    Expect.equal (Time.toSecond Time.utc (Time.millisToPosix 1234)) 1

                test "toSecond 5" <| fun _ -> 
                    Expect.equal (Time.toSecond Time.utc (Time.millisToPosix 5678)) 5

                test "toMillisecond 0" <| fun _ -> 
                    Expect.equal (Time.toMillis Time.utc (Time.millisToPosix 0)) 0

                test "toMillisecond 234" <| fun _ -> 
                    Expect.equal (Time.toMillis Time.utc (Time.millisToPosix 1234)) 234

                test "toMillisecond 678" <| fun _ -> 
                    Expect.equal (Time.toMillis Time.utc (Time.millisToPosix 5678)) 678
              
            ]

    describe "Time" [
        simpleTests
    ]