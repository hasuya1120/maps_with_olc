module Olc exposing (OlcDigits, OlcString, getOlcString)

import List.Extra
import Maybe.Extra
import String



-- TYPES


type OlcDigits
    = OlcDigits Char


type OlcString
    = OlcString String


type alias RawLatitude =
    Float


type alias ClippedLatitude =
    Float


type alias PositiveLatitude =
    Int


type alias RawLongitude =
    Float


type alias NormalizedLongitude =
    Float


type alias PositiveLongitude =
    Int


type alias RawCoordinate =
    ( RawLatitude, RawLongitude )


type alias PositiveCoordinate =
    ( PositiveLatitude, PositiveLongitude )


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


insertFormatSeparator : OlcString -> OlcString
insertFormatSeparator (OlcString string) =
    let
        beforeSeparatorSubString =
            String.slice 0 separatePosition string |> String.padRight separatePosition paddingCharacter

        afterSeparaterSubString =
            String.slice separatePosition (String.length string) string
    in
    [ beforeSeparatorSubString, afterSeparaterSubString ]
        |> String.join formatSeparator
        |> OlcString



{--TODO: Write documentation about generate OlcString --}


getOlcString : Int -> RawCoordinate -> OlcString
getOlcString codeLength rawCoordinate =
    rawCoordinate
        |> Tuple.mapBoth clipLatitude normalizeLongitude
        |> calculateMostSignificant10Digits codeLength


calculateMostSignificant10Digits : Int -> ( ClippedLatitude, NormalizedLongitude ) -> OlcString
calculateMostSignificant10Digits codeLength postiveCoordinate =
    postiveCoordinate
        |> Tuple.mapBoth convertToPositiveLatitude convertToPositiveLongitude
        |> convertCoordinateToOlcDigits []
        |> fromDigitsToOlcString codeLength
        |> insertFormatSeparator


convertCoordinateToOlcDigits : List (Maybe OlcDigits) -> PositiveCoordinate -> List (Maybe OlcDigits)
convertCoordinateToOlcDigits acc ( latitude, longitude ) =
    if List.length acc < 10 then
        let
            longitudeDigit =
                longitude |> modBy20 |> fromIntegerToDigit

            latitudeDigit =
                latitude |> modBy20 |> fromIntegerToDigit

            newAcc =
                List.append [ latitudeDigit, longitudeDigit ] acc

            newLongitude =
                divideBy20 longitude

            newLatitude =
                divideBy20 latitude
        in
        convertCoordinateToOlcDigits newAcc ( newLatitude, newLongitude )

    else
        acc


gridRows : Int
gridRows =
    5


gridColumns : Int
gridColumns =
    4



{--TODO: implement a function to calculate least significant 5 digits--}


calculateLeastSignificant5digits : Int -> ( ClippedLatitude, NormalizedLongitude ) -> OlcString
calculateLeastSignificant5digits codeLength coordinate =
    if codeLength <= 11 then
        OlcString ""

    else
        OlcString ""



-- Utils


normalizeLongitude : RawLongitude -> NormalizedLongitude
normalizeLongitude longitude =
    if longitude >= 180 then
        normalizeLongitude (longitude - 360)

    else if longitude < -180 then
        normalizeLongitude (longitude + 360)

    else
        longitude


convertToPositiveLongitude : NormalizedLongitude -> PositiveLongitude
convertToPositiveLongitude longitude =
    (longitude + 180) * 8000 |> floor


clipLatitude : RawLatitude -> ClippedLatitude
clipLatitude latitude =
    clamp -90 90 latitude


convertToPositiveLatitude : ClippedLatitude -> PositiveLongitude
convertToPositiveLatitude latitude =
    (latitude + 90) * 8000 |> floor


modBy20 : Int -> Int
modBy20 =
    modBy 20


divideBy20 : Int -> Int
divideBy20 value =
    value // 20
