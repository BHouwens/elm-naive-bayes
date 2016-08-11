module NaiveBayes where


import Array
import List.Extra exposing (elemIndex, transpose, intercalate)


-- MODEL

type alias Model = 
    { classes : List String
    , data : List DataEntry 
    }

type alias DataEntry = 
    { name : String 
    , values : List Float
    }


newDataEntry : String -> List Float -> DataEntry
newDataEntry name values =
    { name = name
    , values = values
    }

-- test model

initialModel : Model
initialModel =
    { classes = ["yes", "no"]
    , data = 
        [ (newDataEntry "sunny" [3, 2])
        , (newDataEntry "overcast" [4, 0])
        , (newDataEntry "rainy" [2, 3])
        ]
    }



-- MAIN

{-| Gets the full denominator for isolated probability calculations 
-}
getFullDenominator : List DataEntry -> Float
getFullDenominator data =
    let
        allValues =
            List.map (\n -> n.values) data
        intercalation =
            intercalate [] allValues
    in
        List.sum intercalation


{-| Gets all the data values for a specific class in training data
-}
getSummedValuesForB : String -> Model -> Float
getSummedValuesForB b model =
    let
        index =
            elemIndex b model.classes
        allValues =
            List.map (\n -> n.values) model.data
        arrayOfTransposition =
            Array.fromList (transpose allValues)
    in
        case index of
            Nothing ->
                0
            Just x ->
                case Array.get x arrayOfTransposition of
                    Nothing ->
                        0
                    Just y ->
                        List.sum y



{-| Returns the summed values for a given A
-}
getSummedValuesForA : String -> List DataEntry -> Float
getSummedValuesForA a data =
    let 
        result =
            List.head (List.filter (\n -> n.name == a) data)
    in
        case result of
            Nothing ->
                0
            Just x ->
                List.sum x.values


{-| Returns the value from the data at the given index
-}
getValueAtIndex : String -> Int -> List DataEntry -> Float
getValueAtIndex a index data =
    let
        result =
            List.head (List.filter (\n -> n.name == a) data)
    in
        case result of
            Nothing ->
                0
            Just x ->
                case Array.get index (Array.fromList x.values) of
                    Nothing ->
                        0
                    Just y ->
                        y


{-| Finds the probability of A in the classic Naive Bayes algorithm
-} 
probabilityOfA : String -> List DataEntry -> Float
probabilityOfA a data =
    let 
        denominator =
            getFullDenominator data
        sum =
            getSummedValuesForA a data
    in
        sum / denominator


{-| Finds the probability of B in the classic Naive Bayes algorithm
-}
probabilityOfB : String -> Model -> Float
probabilityOfB b model =
    let
        denominator =
            getFullDenominator model.data
        sum =
            getSummedValuesForB b model
    in
        sum / denominator


{-| Finds the probability of A given B in the classic Naive Bayes algorithm
-}
probabilityOfAGivenB : String -> String -> Model -> Float
probabilityOfAGivenB a b model =
    let
        index = 
            elemIndex b model.classes
        summedB =
            getSummedValuesForB b model
        numerator =
            case index of
                Nothing ->
                    0
                Just x -> 
                    getValueAtIndex a x model.data
    in
        numerator / summedB


{-| Returns the probability of the data value A given a class B
-}
posterior : String -> String -> Model -> Float
posterior a b model =
    let
        finalAs =
            probabilityOfA a model.data
        finalBs =
            probabilityOfB b model
        aGivenB =
            probabilityOfAGivenB a b model
    in
        (aGivenB * finalBs) / finalAs