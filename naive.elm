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
getClassValues : String -> Model -> Maybe (List Int)
getClassValues a model =
    let
        index =
            elemIndex a model.classes
        allValues =
            List.map (\n -> n.values) model.data
        arrayOfTransposition =
            Array.fromList (transpose allValues)
    in
        case index of
            Nothing ->
                Nothing
            Just x ->
                Array.get x arrayOfTransposition


{-| Finds the probability of A in the classic Naive Bayes algorithm
-} 
probabilityOfA : String -> Model -> Float
probabilityOfA a model =
    let 
        data =
            model.data
        denominator =
            getFullDenominator data
        filtered =
            List.head (List.filter (\n -> n.name == a) data)
        sum =
            case filtered of
                Nothing ->
                    0
                Just x ->
                    List.sum x.values
    in
        toFloat sum / toFloat denominator


{-| Finds the probability of B in the classic Naive Bayes algorithm
-}
probabilityOfB : String -> Model -> Float
probabilityOfB a model =
    let
        data =
            model.data
        denominator =
            getFullDenominator data
        values =
            getClassValues a model
        sum =
            case values of
                Nothing ->
                    0
                Just x ->
                    List.sum x
    in
        toFloat sum / toFloat denominator
        

main : Graphics.Element.Element
main = 
    show (toString (probabilityOfB "test2" initialModel))

