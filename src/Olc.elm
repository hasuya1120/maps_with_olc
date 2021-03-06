module Olc exposing
    ( OlcString
    , RawCoordinate
    , RawLatitude
    , RawLongitude
    , encode
    , toString
    )

{-| This module encodes and decodes Olc.


# Types

@docs OlcString
@docs RawCoordinate
@docs RawLatitude
@docs RawLongitude


# Functions

@docs encode
@docs toString

-}

import List.Extra
import Maybe.Extra
import Result.Extra
import String exposing (join)



-- TYPES


type OlcDigits
    = OlcDigits Char


{-| A string representing Olc
-}
type OlcString
    = OlcString String


{-| Latitude of WGS84.
-}
type alias RawLatitude =
    Float


type alias ClippedLatitude =
    Float


type alias PositiveLatitude =
    Int


{-| Longitude of WGS84.
-}
type alias RawLongitude =
    Float


type alias NormalizedLongitude =
    Float


type alias PositiveLongitude =
    Int


{-| Latitude and longitude tuples to convert to Olc.
-}
type alias RawCoordinate =
    ( RawLatitude, RawLongitude )


type alias PositiveCoordinate =
    ( PositiveLatitude, PositiveLongitude )



-- constants


paddingCharacter : Char
paddingCharacter =
    '0'


formatSeparator : String
formatSeparator =
    "+"


separatePosition : Int
separatePosition =
    8


olcDigits : List OlcDigits
olcDigits =
    [ '2', '3', '4', '5', '6', '7', '8', '9', 'C', 'F', 'G', 'H', 'J', 'M', 'P', 'Q', 'R', 'V', 'W', 'X' ]
        |> List.map OlcDigits


maxLatitude : Float
maxLatitude =
    90


maxLongitude : Float
maxLongitude =
    180


pairCodeLength : Int
pairCodeLength =
    10


gridCodeLength : Int
gridCodeLength =
    5


gridRows : Int
gridRows =
    5


gridColumns : Int
gridColumns =
    4


fromIntegerToDigit : Int -> Maybe OlcDigits
fromIntegerToDigit value =
    List.Extra.getAt value olcDigits


fromDigitsToOlcString : Int -> List (Maybe OlcDigits) -> OlcString
fromDigitsToOlcString codeLength maybeOlcDigits =
    maybeOlcDigits
        |> Maybe.Extra.combine
        |> Maybe.withDefault []
        |> List.map (\(OlcDigits str) -> str)
        |> String.fromList
        |> String.slice 0 codeLength
        |> OlcString
        |> insertFormatSeparator


insertFormatSeparator : OlcString -> OlcString
insertFormatSeparator (OlcString string) =
    let
        beforeSeparatorSubString =
            string |> String.slice 0 separatePosition |> String.padRight separatePosition paddingCharacter

        afterSeparaterSubString =
            String.slice separatePosition (String.length string) string
    in
    [ beforeSeparatorSubString, afterSeparaterSubString ]
        |> String.join formatSeparator
        |> OlcString


{-| to String from OlcString.

    toString OlcString "8FVC9G8F+6W" == "8FVC9G8F+6W"

-}
toString : OlcString -> String
toString (OlcString string) =
    string


{-| Encodes latitude and longitude to Olc. Specify codeLength as the first argument and latitude and longitude tuples as the second argument, it will be encoded in olc.

    encode 10 ( 47.365562, 8.524813 ) == Ok (OlcString "8FVC9G8F+6W")

If the code length is 10 digits or less, the valid code length is an even digit(2, 4, 6, 8, 10).
See [Olc Document](https://github.com/google/open-location-code/blob/master/docs/specification.md) for more details.

-}
encode : Int -> RawCoordinate -> Result String OlcString
encode codeLength rawCoordinate =
    {--TODO: Allow latitude == 90 to be converted correctly--}
    if (codeLength >= pairCodeLength) || (modBy 2 codeLength == 0) then
        rawCoordinate
            |> convertToPositiveCoordinate
            |> calculateOlcDigits codeLength
            |> fromDigitsToOlcString codeLength
            |> Result.Extra.singleton

    else
        Err "Code length should be even when code lengh lower than or equal to 10."


convertToPositiveCoordinate : RawCoordinate -> PositiveCoordinate
convertToPositiveCoordinate rawCoordinate =
    rawCoordinate
        |> Tuple.mapBoth clipLatitude normalizeLongitude
        |> Tuple.mapBoth convertToPositiveLatitude convertToPositiveLongitude


clipLatitude : RawLatitude -> ClippedLatitude
clipLatitude latitude =
    clamp -maxLatitude maxLatitude latitude


convertToPositiveLatitude : ClippedLatitude -> PositiveLongitude
convertToPositiveLatitude latitude =
    (latitude + maxLatitude) * 8000 * (gridRows ^ gridCodeLength |> toFloat) |> floor



{--FIXME: type mismatch--}


normalizeLongitude : RawLongitude -> NormalizedLongitude
normalizeLongitude longitude =
    if longitude >= maxLongitude then
        normalizeLongitude (longitude - 360)

    else if longitude < -maxLongitude then
        normalizeLongitude (longitude + 360)

    else
        longitude


convertToPositiveLongitude : NormalizedLongitude -> PositiveLongitude
convertToPositiveLongitude longitude =
    (longitude + maxLongitude) * 8000 * (gridColumns ^ gridCodeLength |> toFloat) |> floor


calculateOlcDigits : Int -> PositiveCoordinate -> List (Maybe OlcDigits)
calculateOlcDigits codeLength positiveCoordinate =
    let
        least5Digits =
            if pairCodeLength < codeLength then
                calculateLeastSignificant5digits positiveCoordinate []

            else
                []

        -- Shifting digits for least siginificant 5 digit calculation, so return to original
        newCoordinate =
            positiveCoordinate
                |> Tuple.mapFirst (\lat -> lat // (gridRows ^ gridCodeLength))
                |> Tuple.mapSecond (\lng -> lng // (gridColumns ^ gridCodeLength))

        most10digits =
            calculateMostSignificant10Digits newCoordinate []
    in
    List.append most10digits least5Digits


calculateMostSignificant10Digits : PositiveCoordinate -> List (Maybe OlcDigits) -> List (Maybe OlcDigits)
calculateMostSignificant10Digits positiveCoordinate acc =
    if List.length acc < pairCodeLength then
        let
            ( latitudeDigit, longitudeDigit ) =
                positiveCoordinate
                    |> Tuple.mapFirst (modBy20 >> fromIntegerToDigit)
                    |> Tuple.mapSecond (modBy20 >> fromIntegerToDigit)

            newAcc =
                List.append [ latitudeDigit, longitudeDigit ] acc

            newCoordinate =
                positiveCoordinate
                    |> Tuple.mapBoth divideBy20 divideBy20
        in
        calculateMostSignificant10Digits newCoordinate newAcc

    else
        acc


calculateLeastSignificant5digits : PositiveCoordinate -> List (Maybe OlcDigits) -> List (Maybe OlcDigits)
calculateLeastSignificant5digits positiveCoordinate acc =
    if List.length acc < gridCodeLength then
        let
            {--TODO: The official implementation has the following formula, but the specification seems to be latitudeDigit * longitudeDigit.
            Check why the formula is --}
            digit =
                positiveCoordinate
                    |> Tuple.mapBoth modByGridRows modByGridColumns
                    |> (\( latDigit, lngDigit ) -> (latDigit * 4) + lngDigit)
                    |> fromIntegerToDigit

            newAcc =
                digit :: acc

            newCoordinate =
                Tuple.mapBoth divideByGridRows divideByGridColumns positiveCoordinate
        in
        calculateLeastSignificant5digits newCoordinate newAcc

    else
        acc



-- Utils


modBy20 : Int -> Int
modBy20 =
    modBy 20


modByGridRows : Int -> Int
modByGridRows =
    modBy gridRows


modByGridColumns : Int -> Int
modByGridColumns =
    modBy gridColumns


divideBy20 : Int -> Int
divideBy20 value =
    value // 20


divideByGridRows : Int -> Int
divideByGridRows value =
    value // gridRows


divideByGridColumns : Int -> Int
divideByGridColumns value =
    value // gridColumns
