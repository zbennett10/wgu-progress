module Main exposing (..)

import Http
import Html exposing ( Html, text, div, h1, img, ul, li )
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
    div []
        [ renderTestCourseList model.courses ]



---- PROGRAM ----

main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }

-- For testing purposes
renderTestCourseList: List Course -> Html msg
renderTestCourseList courses = 
    ul [] (List.map (\c -> li [] [ text c.name ]) courses)

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
