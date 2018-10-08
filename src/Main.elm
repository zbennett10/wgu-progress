module Main exposing (..)

import Http
import Html exposing ( Html, text, div, h1, img, ul, li, hr )
import Bootstrap.ListGroup as ListGroup
import Bootstrap.Grid as Grid
import Bootstrap.Progress as Progress
import Json.Decode as Decode exposing ( field )

type alias Course = { name: String,  status: String, degreeType: String }


---- MODEL ----

type alias Model =
    { courses : List Course }

init : ( Model, Cmd Msg )
init =
    ( { courses = [ Course "test" "test" "test" ] }, fetchCourses )



---- UPDATE ----


type Msg
    = CoursesReceived ( Result Http.Error ( List Course ) )

-- Fetches initial WGU courses 
fetchCourses : Cmd Msg
fetchCourses =
    Http.send CoursesReceived ( Http.get "/courses.json" courseListDecoder )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CoursesReceived result ->
            case result of
                Ok newCourses -> 
                    ( { model | courses = newCourses }
                    , Cmd.none 
                    )        

                Err _ ->
                    ( model
                    , Cmd.none
                    )



---- VIEW ----

view : Model -> Html Msg
view model =
    Grid.container []
        [ Grid.row []
            [ Grid.col []
                [ h1 [] [ text ( "Total - " ++ ( toString ( calcCourseCompletionPercentage model.courses ) ) ++ "%" ) ] 
                , Progress.progress [ Progress.value ( calcCourseCompletionPercentage model.courses ) ] 
                ]
            ]
        , hr [] []
        , Grid.row []
            [ Grid.col []
                [ h1 [] [ text ( "Bachelors - " ++ ( toString ( calcBachelorPercentComplete model.courses ) ) ++ "%" ) ]
                , Progress.progress [ Progress.value ( calcBachelorPercentComplete model.courses ), Progress.success ] 
                ]
            , Grid.col []
                [ h1 [] [ text ( "Masters - " ++ ( toString ( calcMastersPercentComplete model.courses ) ) ++ "%" ) ] 
                , Progress.progress [ Progress.value ( calcMastersPercentComplete model.courses ) ] 
                ]
            ]
        , hr [] []
        , Grid.row []
            [ Grid.col []
                [ renderCoursesByType model.courses "Bachelor" ]
            , Grid.col []
                [ renderCoursesByType model.courses "Master" ]
            ]
        ]

---- PROGRAM ----

main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }

-- HTML Rendering Helpers --

renderCoursesByType: List Course -> String -> Html msg
renderCoursesByType courses courseType =
    filterCoursesByDegreeType courses courseType
        |> renderCourseListItems
        |> ListGroup.ul

renderCourseListItems: List Course -> List (ListGroup.Item msg)
renderCourseListItems courses =
    List.map assignCourseListItemClassesByStatus courses

assignCourseListItemClassesByStatus: Course -> (ListGroup.Item msg)
assignCourseListItemClassesByStatus course =
    if course.status == "Past" then
        ListGroup.li [ ListGroup.success ] [ text course.name ]
    else if course.status == "Present" then
        ListGroup.li [ ListGroup.active ] [ text course.name ]
    else
        ListGroup.li [ ListGroup.disabled ] [ text course.name ]

renderBachelorCourses: List Course -> Html msg
renderBachelorCourses bachCourses =
    List.filter ( \c -> c.degreeType == "Bachelor") bachCourses
        |> List.map ( \c -> ListGroup.li [] [ text c.name ] )
        |> ListGroup.ul


-- Utility Helpers --

calcCourseCompletionPercentage: List Course -> Float
calcCourseCompletionPercentage courses =
    ( toFloat ( List.length ( getFinishedCourses courses ) ) ) / toFloat ( List.length courses ) * 100

calcBachelorPercentComplete: List Course -> Float
calcBachelorPercentComplete courses =
    let
        bachelorCourseCount = 
            List.length ( filterCoursesByDegreeType courses "Bachelor" )

        finishedBachelorCourseCount =
            List.length ( getFinishedBachelorCourses courses )
    in
    ( ( toFloat finishedBachelorCourseCount ) / ( toFloat bachelorCourseCount ) ) * 100

calcMastersPercentComplete: List Course -> Float
calcMastersPercentComplete courses =
    let
        mastersCourseCount = 
            List.length ( filterCoursesByDegreeType courses "Master" )

        finishedMastersCourseCount =
            List.length ( getFinishedMastersCourses courses )
    in
    ( ( toFloat finishedMastersCourseCount ) / ( toFloat mastersCourseCount ) ) * 100



filterCoursesByDegreeType: List Course -> String -> List Course
filterCoursesByDegreeType courses courseType =
    List.filter (\c -> c.degreeType == courseType ) courses

getFinishedCourses: List Course -> List Course
getFinishedCourses courses =
    List.filter (\c -> c.status == "Past" ) courses

getCurrentCourses: List Course -> List Course
getCurrentCourses courses =
    List.filter (\c -> c.status == "Present" ) courses

getFutureCourses: List Course -> List Course
getFutureCourses courses =
    List.filter (\c -> c.status == "Future" ) courses

getFinishedBachelorCourses: List Course -> List Course
getFinishedBachelorCourses courses =
    List.filter (\c -> c.status == "Past" && c.degreeType == "Bachelor" ) courses

getFutureBachelorCourses: List Course -> List Course
getFutureBachelorCourses courses =
    List.filter (\c -> c.status == "Future" && c.degreeType == "Bachelor" ) courses

getFinishedMastersCourses: List Course -> List Course
getFinishedMastersCourses courses =
    List.filter (\c -> c.status == "Past" && c.degreeType == "Master" ) courses

getFutureMasterCourses: List Course -> List Course
getFutureMasterCourses courses =
    List.filter (\c -> c.status == "Future" && c.degreeType == "Master" ) courses

-- Decodes a Course
courseDecoder: Decode.Decoder Course
courseDecoder =
    Decode.map3 Course 
        ( field "name" Decode.string )
        ( field "status" Decode.string )
        ( field "degreeType" Decode.string ) 

-- Decodes a list of Courses
courseListDecoder: Decode.Decoder ( List Course )
courseListDecoder =
    Decode.list courseDecoder
