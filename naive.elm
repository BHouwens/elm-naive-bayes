module NaiveBayes where

import Array
import Graphics.Element exposing (show)
import List.Extra exposing (elemIndex, transpose, intercalate)


-- MODEL

type alias Model = 
    { classes : List String
    , data : List DataEntry 
    }

type alias DataEntry = 
    { name : String 
    , values : List Int
    }


newDataEntry : String -> (List Int) -> DataEntry
newDataEntry name values =
    { name = name
    , values = values
    }


initialModel : Model
initialModel =
    { classes = ["test1", "test2"]
    , data = 
        [ (newDataEntry "john" [1, 2])
        , (newDataEntry "suzie" [3, 4])
        ]
    }



-- UPDATE

setData' : List String -> List DataEntry -> Model -> Model
setData' classes data model =
    { model 
    | classes = classes
    , data = data
    }

setData : List String -> List DataEntry -> Model
setData classes data = setData' classes data initialModel



-- MAIN

{-| Gets the full denominator for isolated probability calculations 
-}
getFullDenominator : List DataEntry -> Int
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
getSummedValuesForB : String -> Model -> Int
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
getSummedValuesForA : String -> List DataEntry -> Int
getSummedValuesForA a data =
    let filtered =
            List.head (List.filter (\n -> n.name == a) data)
    in
        case filtered of
            Nothing ->
                0
            Just x ->
                List.sum x.values



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
        toFloat sum / toFloat denominator


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
        toFloat sum / toFloat denominator


{-| Finds the probability of A given B in the classic Naive Bayes algorithm
-}
probabilityOfAGivenB : String -> String -> Model -> Float
probabilityOfAGivenB a b model =
    let
        index = 
            elemIndex b model.classes
        summedA =
            getSummedValuesForA a model.data
        summedB =
            getSummedValuesForB b model
    in
        toFloat summedA / toFloat summedB


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


main : Graphics.Element.Element
main = 
    show (toString (posterior "suzie" "test1" initialModel))