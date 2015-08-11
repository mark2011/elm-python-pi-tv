import Char
import Color
import Date exposing (Date, fromTime, day, dayOfWeek, hour, minute, second, month, year)
import Graphics.Element exposing
    (container, down, fittedImage, flow, image, leftAligned, middle, midLeft, topLeft)
import Html exposing (fromElement, Html)
import Http
import Json.Decode as Json
import Keyboard
import List exposing (map)
import Signal exposing (Address, mergeMany)
import String
import Task exposing (andThen, onError, succeed, Task)
import Text exposing (fromString)
import Time
import Window

type alias PhotosList = List String
type alias Model =
  { debug: Bool
   ,date: Date
   ,windowWidth: Int
   ,windowHeight: Int
   ,photoName: Maybe String
   ,photos: PhotosList
   ,errorLog: List Http.Error
  }


type Action
  = Clock Date
  | KeyPressed Char
  | SetWindowSize (Int, Int)
  | SetPhotosList PhotosList
  | Errored Http.Error


following : a -> List a -> List a
following a list = case list of
  head :: tail -> if head == a then tail else following a tail
  [] -> []


maybeNextEntry : Maybe a -> List a -> Maybe a
maybeNextEntry maybeEntry entries = case entries of
  [] -> Nothing
  first :: tail -> case maybeEntry of
    Nothing -> Just first
    Just entry -> case following entry entries of
      [] -> Just first
      head :: _ -> Just head


main = start
  { model =
    { debug = False
     ,date = fromTime 0
     ,windowWidth = 1024
     ,windowHeight = 768
     ,photoName = Nothing
     ,photos = []
     ,errorLog = []
    }
   , view = view
   , update = update
  }

isClockTime : Date -> Bool
isClockTime date = minute date `rem` 5 == 0


isPhotoTime : Date -> Bool
isPhotoTime date = second date `rem` 15 == 0


view _ model =
  let
    show fontSize =
      fromString
      >> Text.color Color.white
      >> Text.typeface ["monospace"]
      >> Text.height fontSize
      >> leftAligned
    c1 = container model.windowWidth model.windowHeight
    listWidth list =
      let
        listWidth' n list = case list of
          head :: tail -> listWidth' (max n (String.length head)) tail
          [] -> n
      in
        listWidth' 1 list
    ratio x y = 0.85 * toFloat x / toFloat y |> floor |> toFloat
    fontSize1 list = ratio model.windowHeight (min (List.length list) 10)
    fontSize2 list = 1.85 * (ratio model.windowWidth (listWidth list))
    fontSize list = min (fontSize1 list) (fontSize2 list)
    fixBlankLine line = if line == "" then " " else line
    drawText list = list |> map fixBlankLine |> map (fontSize list |> show) >> flow down >> c1 topLeft
    dayNumber = day model.date
    dayName = case dayOfWeek model.date of
      Date.Mon -> "Monday"
      Date.Tue -> "Tuesday"
      Date.Wed -> "Wednesday"
      Date.Thu -> "Thursday"
      Date.Fri -> "Friday"
      Date.Sat -> "Saturday"
      Date.Sun -> "Sunday"
    monthName = case month model.date of
      Date.Jan -> "January"
      Date.Feb -> "February"
      Date.Mar -> "March"
      Date.Apr -> "April"
      Date.May -> "May"
      Date.Jun -> "June"
      Date.Jul -> "July"
      Date.Aug -> "August"
      Date.Sep -> "September"
      Date.Oct -> "October"
      Date.Nov -> "November"
      Date.Dec -> "December"
    daySuffix = if
      | dayNumber == 1 || dayNumber == 21 || dayNumber == 31 -> "st"
      | dayNumber == 2 || dayNumber == 22 -> "st"
      | dayNumber == 3 || dayNumber == 23 -> "rd"
      | otherwise -> "th"
    hour12 = if
      | hour model.date == 0 -> 12
      | hour model.date <= 12 -> hour model.date
      | otherwise -> hour model.date - 12
    amPm = if hour model.date <= 11 then "AM" else "PM"
    minute00 =
      if minute model.date < 10 then
        "0" ++ toString (minute model.date)
      else
        toString (minute model.date)
    clockText = drawText <|
      ["Today is " ++ dayName,
       monthName ++ " " ++ toString dayNumber ++ daySuffix,
       toString (year model.date),
       "",
       "",
       "The time is " ++ toString hour12 ++ ":" ++ minute00 ++ " " ++ amPm]
    screen = if
      | model.debug ->
          drawText
            (map toString model.errorLog)
      | isClockTime model.date ->
          clockText
      | otherwise ->
          case model.photoName of
            Nothing ->
              drawText
                ["",
                 "",
                 "  Place photos in",
                 "    photos folder",
                 "",
                 ""]
            Just path -> 
              fittedImage model.windowWidth model.windowHeight path |> c1 middle
    in
      screen |> Graphics.Element.color Color.black |> fromElement


update action model = case action of
  Errored error ->
    { model | errorLog <- error :: model.errorLog }
  SetPhotosList photos ->
    { model | photos <- photos }
  Clock date ->
    if isPhotoTime model.date then
      { model | date <- date, photoName <- maybeNextEntry model.photoName model.photos }
    else
      { model | date <- date }
  KeyPressed 'd' ->
    { model | debug <- not model.debug }
  KeyPressed _ ->
    model
  SetWindowSize windowSize ->
    { model | windowWidth <- fst windowSize, windowHeight <- snd windowSize }


type alias App =
  { model: Model
   ,view: Address Action -> Model -> Html
   ,update: Action -> Model -> Model
  }


viewMailbox =
  Signal.mailbox Nothing


viewMailboxAddress =
  Signal.forwardTo viewMailbox.address Just


pollingResults = Signal.mailbox Nothing
sendPolling actionConstructor =
  actionConstructor >> Just >> (Signal.send pollingResults.address)
sendPhotos =
  sendPolling SetPhotosList
sendError =
  sendPolling Errored


port pollingRequests: Signal (Task ()())
port pollingRequests =
  Time.every (10 * Time.second) |> Signal.map (\_ ->
    Http.get (Json.list Json.string) "http://localhost:3000/photos"
    `andThen` sendPhotos
    `onError` sendError)


start: App -> Signal Html
start app =
  let
    asActionSignal f =
      Signal.map (f >> Just)

    windowSize =
      Window.dimensions |> asActionSignal SetWindowSize

    clock =
      Time.every Time.second |> asActionSignal (fromTime >> Clock)

    keys =
      Keyboard.presses |> asActionSignal (Char.fromCode >> KeyPressed)

    model =
      Signal.foldp
        (\(Just action) model -> app.update action model)
        app.model
        (mergeMany [viewMailbox.signal, clock, keys, windowSize, pollingResults.signal])
  in
    Signal.map (app.view viewMailboxAddress) model
