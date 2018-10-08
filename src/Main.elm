module Main exposing (..)

import Http
import Html exposing ( Html, text, div, h1, img, ul, li )
import Bootstrap.ListGroup as ListGroup
import Bootstrap.Grid as Grid
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

getFinishedMasterCourses: List Course -> List Course
getFinishedMasterCourses courses =
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
