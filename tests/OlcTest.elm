module OlcTest exposing (suite)

import Expect
import Olc
import Result.Extra
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "The Olc module"
        [ describe "Olc.encode"
            [ test "returns specified digits OlcString" <|
                \_ ->
                    let
                        expected =
                            List.map
                                Result.Extra.singleton
                                [ "8Q7XMXQP+QMFM"
                                , "8Q7XMXQP+QM"
                                , "8Q7XMXQP+"
                                , "8Q7XMX00+"
                                , "62H20000+"

                                -- , "CFX30000+"
                                ]

                        testData =
                            [ ( ( 35.6894444, 139.9866667 ), 12 )
                            , ( ( 35.6894444, 139.9866667 ), 10 )
                            , ( ( 35.6894444, 139.9866667 ), 8 )
                            , ( ( 35.6894444, 139.9866667 ), 6 )
                            , ( ( 1, 180 ), 4 )

                            -- , ( ( 90, 1 ), 4 )
                            ]
                    in
                    testData
                        |> List.map
                            (\( coordinate, codeLength ) ->
                                Olc.encode codeLength coordinate
                                    |> Result.map Olc.toString
                            )
                        |> Expect.equalLists expected
            , test "generates OlcString using Clipped Latitude and Normalized Longitude and return it" <|
                \_ ->
                    let
                        expected =
                            List.map
                                Result.Extra.singleton
                                [ "8Q7XMXQP+QMFM"
                                , "62H20000+"

                                -- , "CFX30000+"
                                ]

                        testData =
                            [ ( ( 35.6894444, 499.9866667 ), 12 )
                            , ( ( 1, 540 ), 4 )

                            -- , ( 92, 1 )
                            ]
                    in
                    testData
                        |> List.map
                            (\( coordinate, codeLength ) ->
                                Olc.encode codeLength coordinate
                                    |> Result.map Olc.toString
                            )
                        |> Expect.equalLists expected
            , test "returns error when code length lower than 10 and odd" <|
                \_ ->
                    Olc.encode 1 ( 35.6894444, 499.9866667 )
                        |> Expect.err
            ]
        ]
