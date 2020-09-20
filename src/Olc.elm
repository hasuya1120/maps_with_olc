module Olc exposing (..)

import Maybe.Extra
import String


type OlcDigits
    = OlcDigits String


type OlcString
    = OlcString String


type alias RawLatitude =
    Float


type alias RawLongitude =
    Float


type alias RawCoordinate =
    ( RawLatitude, RawLongitude )


fromDecimalToDigit : Int -> Maybe OlcDigits
fromDecimalToDigit value =
    case value of
        0 ->
            OlcDigits "2" |> Just

        1 ->
            OlcDigits "3" |> Just

        2 ->
            OlcDigits "4" |> Just

        3 ->
            OlcDigits "5" |> Just

        4 ->
            OlcDigits "6" |> Just

        5 ->
            OlcDigits "7" |> Just

        6 ->
            OlcDigits "8" |> Just

        7 ->
            OlcDigits "9" |> Just

        8 ->
            OlcDigits "C" |> Just

        9 ->
            OlcDigits "F" |> Just

        10 ->
            OlcDigits "G" |> Just

        11 ->
            OlcDigits "H" |> Just

        12 ->
            OlcDigits "J" |> Just

        13 ->
            OlcDigits "M" |> Just

        14 ->
            OlcDigits "P" |> Just

        15 ->
            OlcDigits "Q" |> Just

        16 ->
            OlcDigits "R" |> Just

        17 ->
            OlcDigits "V" |> Just

        18 ->
            OlcDigits "W" |> Just

        19 ->
            OlcDigits "X" |> Just

        _ ->
            Nothing


fromDigitsToMaybeOlcString : List (Maybe OlcDigits) -> Maybe OlcString
fromDigitsToMaybeOlcString maybeOlcDigits =
    maybeOlcDigits
        |> Maybe.Extra.combine
        |> Maybe.andThen fromDigitsToOlcString


fromDigitsToOlcString : List OlcDigits -> Maybe OlcString
fromDigitsToOlcString olcDigits =
    olcDigits
        |> List.map (\(OlcDigits str) -> str)
        |> String.concat
        |> OlcString
        |> Just


type FormatSeparator
    = FormatSeparator String


formatSeparator : FormatSeparator
formatSeparator =
    FormatSeparator "+"


type PaddingCharacter
    = PaddingCharacter String


paddingCharacter : PaddingCharacter
paddingCharacter =
    PaddingCharacter "0"


toOlcMostSignificant10Digits : RawCoordinate -> Maybe OlcString
toOlcMostSignificant10Digits rawCoordinate =
    rawCoordinate
        |> convertToPositiveCoordinate
        |> convertCoordinateToOlcDigits []
        |> fromDigitsToMaybeOlcString


type alias PositiveCoordinate =
    ( PositiveLatitude, PositiveLongitude )


convertToPositiveCoordinate : RawCoordinate -> PositiveCoordinate
convertToPositiveCoordinate rawCoodinate =
    rawCoodinate
        |> Tuple.mapBoth normalizeLongitude clipLatitude
        |> Tuple.mapBoth convertToPositiveLongitude convertToPositiveLatitude


type alias NormalizedLongitude =
    Float


normalizeLongitude : RawLongitude -> NormalizedLongitude
normalizeLongitude longitude =
    if longitude >= 180 then
        normalizeLongitude (longitude - 360)

    else if longitude < -180 then
        normalizeLongitude (longitude + 360)

    else
        longitude


type alias PositiveLongitude =
    Int


convertToPositiveLongitude : NormalizedLongitude -> PositiveLongitude
convertToPositiveLongitude longitude =
    (longitude + 180) * 8000 |> floor


type alias ClippedLatitude =
    Float


clipLatitude : RawLatitude -> ClippedLatitude
clipLatitude latitude =
    clamp -90 90 latitude


type alias PositiveLatitude =
    Int


convertToPositiveLatitude : ClippedLatitude -> PositiveLongitude
convertToPositiveLatitude latitude =
    (latitude + 90) * 8000 |> floor


convertCoordinateToOlcDigits : List (Maybe OlcDigits) -> PositiveCoordinate -> List (Maybe OlcDigits)
convertCoordinateToOlcDigits acc ( longitude, latitude ) =
    if List.length acc < 10 then
        let
            longitudeDigit =
                longitude |> modBy20 |> fromDecimalToDigit

            latitudeDigit =
                latitude |> modBy20 |> fromDecimalToDigit

            newAcc =
                List.append [ latitudeDigit, longitudeDigit ] acc

            newLongitude =
                divideBy20 longitude

            newLatitude =
                divideBy20 latitude
        in
        convertCoordinateToOlcDigits newAcc ( newLongitude, newLatitude )

    else
        acc


modBy20 : Int -> Int
modBy20 =
    modBy 20


divideBy20 : Int -> Int
divideBy20 value =
    value // 20
