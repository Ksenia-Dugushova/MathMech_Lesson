namespace MathMech_lessons.Tests

open Expecto
open MathMech_lessons.Main

module SayTests =
    [<Tests>]
    let tests =
        testList "samples" [
            testCase "1 test for exponentiation in a simple way"
            <| fun _ ->
                let actualResult = f1 2 10
                Expect.equal actualResult 1024 "The answer isn't correct"

            testCase "2 test for exponentiation in a simple way"
            <| fun _ ->
                let actualResult = f1 0 5
                Expect.equal actualResult 0 "The answer isn't correct"

            testCase "3 test for exponentiation in a simple way"
            <| fun _ ->
                let actualResult = f1 1 5
                Expect.equal actualResult 1 "The answer isn't correct"

            testCase "4 test for exponentiation in a simple way"
            <| fun _ ->
                let actualResult = f1 -2 5
                Expect.equal actualResult -32 "The answer isn't correct"


            testCase "1 test for exponentiation in a fast way"
            <| fun _ ->
                let actualResult = f2 2 10
                Expect.equal actualResult 1024 "The answer isn't correct"

            testCase "2 test for exponentiation in a fast way"
            <| fun _ ->
                let actualResult = f2 0 5
                Expect.equal actualResult 0 "The answer isn't correct"

            testCase "3 test for exponentiation in a fast way"
            <| fun _ ->
                let actualResult = f2 1 5
                Expect.equal actualResult 1 "The answer isn't correct"

            testCase "4 test for exponentiation in a fast way"
            <| fun _ ->
                let actualResult = f2 -2 5
                Expect.equal actualResult -32 "The answer isn't correct"


            testCase "1 test for difference"
            <| fun _ ->
                let actualResult =
                    f3 [|
                        1
                        2
                        3
                    |]

                Expect.equal actualResult 2 "The answer isn't correct"

            testCase "2 test for difference"
            <| fun _ ->
                let actualResult =
                    f3 [|
                        0
                        0
                        0
                    |]

                Expect.equal actualResult 0 "The answer isn't correct"

            testCase "3 test for difference"
            <| fun _ ->
                let actualResult =
                    f3 [|
                        -1
                        2
                        -13
                        9
                    |]

                Expect.equal actualResult 22 "The answer isn't correct"


            testCase "1 test for array"
            <| fun _ ->
                let actualResult = f4 1 5
                Expect.equal actualResult [| 3 |] "The answer isn't correct"

            testCase "2 test for array"
            <| fun _ ->
                let actualResult = f4 5 13

                Expect.equal
                    actualResult
                    [|
                        7
                        9
                        11
                    |]
                    "The answer isn't correct"

            testCase "3 test for array"
            <| fun _ ->
                let actualResult = f4 2 14

                Expect.equal
                    actualResult
                    [|
                        3
                        5
                        7
                        9
                        11
                        13
                    |]
                    "The answer isn't correct"
            testCase "4 test for array"
            <| fun _ ->
                let actualResult = f4 -5 3

                Expect.equal
                    actualResult
                    [|
                        -3
                        -1
                        1
                    |]
                    "The answer isn't correct"

        ]
