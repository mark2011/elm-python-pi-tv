import Char
import Color
import Date exposing (Date, fromTime, second, minute)
import Graphics.Element exposing
  (container, down, empty, fittedImage, flow, image, layers, leftAligned,
    middle, midLeft, opacity, topLeft)
import Html
import Http
import Json.Decode as J exposing ((:=))
import Json.Encode as K
import Keyboard
import List exposing (map)
import Module exposing (appendIf, nextEntry, timeSinceMinute)
import Signal exposing ((<~), (~))
import String
import Task exposing (andThen, onError, succeed, Task)
import Text
import Time
import Window

(?) maybe default = Maybe.withDefault default maybe
infixr 9 ?

type alias KeyedList key itemBase = List {itemBase | key: key}
type alias KeyedChannel key itemBase = {selectedItemKey: Maybe key, items: KeyedList key itemBase}
type alias PhotoBaseItem = {modTime: Float}
type alias LightBaseItem = {name: String, on: Bool, reachable: Bool}
type ChannelKey
  = Guide
  | Photos
  | Lights
type Direction
  = Forward
  | Backward
  | Rewind
  | Stay
type Action
  = RequestDone
  | SetConfig {hueToken: String}
  | Clock Date
  | KeyboardPressed Char
  | SetPhotoItems (KeyedList String PhotoBaseItem)
  | SetLightItems (KeyedList String LightBaseItem)
  | Error String
type Request
  = NoRequest
  | LogMessage String
  | GetConfig
  | GetPhotoItems
  | PressKeyboard String
  | GetLightItems String
  | BlinkLight String String
fadeTime = 10.0 * Time.second
isEvery k f date = (f date) `rem` k == 0
isPollTime = isEvery 10 second
isShowClockTime = isEvery 5 minute
isAdvancePhotoTime date = (isEvery 15 second date) && not (isShowClockTime date)

view _ model (width, height) =
  let
    fullScreen = container width height
    ratio x y = 0.85 * toFloat x / toFloat y |> floor |> toFloat
    fontSize1 list = ratio height <| List.length list
    fontSize2 list = (*) 1.85 <| ratio width <| List.foldl max 1 <| map String.length list
    fontSize list = fontSize1 list `min` fontSize2 list
    drawLines = drawLines2 << map (\line -> (line, identity))
    drawLines2 lines =
      let drawLine2 (line, f) =
        (if line == "" then " " else line)
        |> Text.fromString
        |> Text.typeface ["monospace"]
        |> Text.height (lines |> map fst |> fontSize)
        |> Text.color Color.white
        |> f
        |> leftAligned
      in lines |> map drawLine2 |> flow down |> fullScreen topLeft
    clockText = drawLines
      ["Today is " ++ Module.dayName model.date,
       Module.monthName model.date ++ " " ++ Module.dayWithSuffix model.date,
       toString (Date.year model.date),
       "",
       "",
       "The time is " ++ Module.shortTime model.date]
    overlayOpacity = max 0.0 <| 1.0 - timeSinceMinute model.date / fadeTime
    overlayText = drawLines
      ["",
       "",
       "",
       "",
       "",
       "",
       "",
       "",
       "",
       "                        " ++ Module.shortTime model.date]
    highlighter channel key =
      if Just key == (model |> channel |> .selectedItemKey) then "*" else " "
    screen = if
      | model.channel == Guide ->
        model.guide.items
          |> map (\{key} -> highlighter .guide key ++ toString key)
          |> drawLines
      | model.channel == Lights ->
        if model.config.hueToken /= "" then
          model.lights.items
            |> map (\{key, name, on, reachable} ->
              let
                color = if
                  | not reachable -> Color.red
                  | not on -> Color.blue
                  | otherwise -> Color.white
              in
                (highlighter .lights key ++ name, Text.color color))
          |> drawLines2
        else
          ["", "", "", " Hue token is not configured", "", "", ""]
          |> drawLines
      | model.photos.displayClock -> clockText
      | otherwise -> case model.photos.selectedItemKey of
          Nothing -> drawLines
            ["",
             "",
             "  Place photos in",
             "    /home/pi/Pictures folder",
             "",
             ""]
          Just photoFilePath -> layers
            [fittedImage width height photoFilePath |> fullScreen middle,
             overlayText |> opacity overlayOpacity]
    in
      screen |> Graphics.Element.color Color.black |> Html.fromElement

freshItems newItems keyedChannel =
  let newSelectedItemKey = case (keyedChannel.selectedItemKey, keyedChannel.items, newItems) of
    (Nothing, _, {key} :: _) -> Just key
    (oldSelectedItemKey, _, _) -> oldSelectedItemKey
  in keyedChannel |> rmw.items (always newItems) |> rmw.selectedItemKey (always newSelectedItemKey)

moveKey direction keyedChannel =
  let
    maybeNextKey channel g =
      case (channel.selectedItemKey, channel.items |> g |> map .key) of
        (_, []) -> Nothing
        (Nothing, firstItemKey :: _) -> Just firstItemKey
        (Just key, firstItemKey :: moreKeys as l) -> Just (nextEntry firstItemKey l key)
  in
    {keyedChannel | selectedItemKey <- case direction of
      Stay -> keyedChannel.selectedItemKey
      Forward -> maybeNextKey keyedChannel identity
      Backward -> maybeNextKey keyedChannel List.reverse
      Rewind -> case keyedChannel.items of
        first :: _ -> Just first.key
        [] -> Nothing}

read = {modify = {write = {
  config          = \f r -> {r | config          <- f r.config         },
  channel         = \f r -> {r | channel         <- f r.channel        },
  date            = \f r -> {r | date            <- f r.date           },
  displayClock    = \f r -> {r | displayClock    <- f r.displayClock   },
  guide           = \f r -> {r | guide           <- f r.guide          },
  items           = \f r -> {r | items           <- f r.items          },
  lights          = \f r -> {r | lights          <- f r.lights         },
  photos          = \f r -> {r | photos          <- f r.photos         },
  requestQueue    = \f r -> {r | requestQueue    <- f r.requestQueue   },
  selectedItemKey = \f r -> {r | selectedItemKey <- f r.selectedItemKey}}}}

rmw = read.modify.write

addRequest request = (\l -> l ++ [request]) |> rmw.requestQueue
destroyOldestRequest = (\l -> List.tail l ? []) |> rmw.requestQueue
addLogMessage = toString >> LogMessage >> addRequest
do functions data = case functions of
  function :: moreFunctions -> do moreFunctions (function data)
  [] -> data
noChange = identity
useBool f bool = if bool then f else noChange
useString f string = if string /= "" then f string else noChange
useMaybe f maybe = case maybe of
  Just x -> f x
  Nothing -> noChange
update action rawModel =
  rawModel
  |> destroyOldestRequest
  |> (\model -> model |> (case action of
      RequestDone -> noChange
      Error error -> error |> addLogMessage
      SetConfig config -> do [always config |> rmw.config, config |> addLogMessage]
      SetPhotoItems items -> freshItems items |> rmw.photos
      SetLightItems items -> freshItems items |> rmw.lights
      Clock date -> do
        [date |> always |> rmw.date,
         isAdvancePhotoTime date |> useBool (moveKey Forward |> rmw.photos),
         isShowClockTime date |> always |> rmw.displayClock |> rmw.photos,
         isPollTime date |> useBool (do
           [GetPhotoItems |> addRequest,
            model.config.hueToken |> useString (GetLightItems >> addRequest)])]
      KeyboardPressed press -> case press of
        'e' -> always Guide |> rmw.channel
        _ -> case model.channel of
          Guide -> case press of
            'y' -> model.guide.selectedItemKey |> useMaybe (always >> rmw.channel)
            _ -> (case press of
              'j' -> Forward
              'k' -> Backward
              '0' -> Rewind
              _ -> Stay) |> moveKey |> rmw.guide
          Photos -> (case press of
            'l' -> Forward
            'h' -> Backward
            '0' -> Rewind
            _ -> Stay) |> moveKey |> rmw.photos
          Lights -> case press of
            'y' -> model.lights.selectedItemKey |> useMaybe (BlinkLight model.config.hueToken >> addRequest)
            _ -> (case press of
              'j' -> Forward
              'k' -> Backward
              '0' -> Rewind
              _ -> Stay) |> moveKey |> rmw.lights))

quote s = "\"" ++ s ++ "\""
encodeQuery = K.encode 0 << K.object << map (\(k, v) -> (k, K.string v))

responseMailbox = Signal.mailbox Nothing
port requestPort: Signal (Task () ())
port requestPort =
  let
    localServer = "http://localhost:3000/"
    hueUrl token = String.concat ["http://philips-hue/api/", token, "/lights/"]
    sendResponse = Just >> Signal.send responseMailbox.address
    get endPoint responseDecoder =
      Http.get responseDecoder endPoint
      `andThen` sendResponse
      `onError` (toString >> Error >> sendResponse)
    put endPoint body =
      Http.send Http.defaultSettings {verb = "PUT", url = endPoint, headers = [], body = body}
      `andThen` (\_ -> RequestDone |> sendResponse)
      `onError` (toString >> Error >> sendResponse)
    simple command parameter argument =
      get <| String.concat [localServer, command, "?", encodeQuery [(parameter, argument)]]
    requestDone = J.object1 (\_ -> RequestDone) <| J.list <| J.string
  in
    Signal.map (\request -> case request of
      LogMessage s ->
        simple "log-message" "message" s requestDone
      GetConfig ->
        simple "get-configuration" "none" "none"
          <| J.object1 ((\token -> {hueToken = token}) >> SetConfig) ("hueToken" := J.string)
      PressKeyboard press ->
        simple "press-key" "key" press requestDone
      GetPhotoItems ->
        simple "list-folder" "folder" "/home/pi/Pictures"
          <| J.object1 (List.sortBy .modTime >> SetPhotoItems) <| J.list <| J.tuple2 (\path modTime -> {key = path, modTime = modTime}) J.string J.float
      GetLightItems token ->
        get
          (hueUrl token)
          <| J.object1
            (map (\(id, light) -> {light | key = id}) >> List.sortBy (.name >> String.toUpper) >> SetLightItems)
            (J.keyValuePairs <| J.object3 (\name on reachable -> {name = name, on = on, reachable = reachable})
              ("name" := J.string)
              (J.at ["state", "on"] J.bool)
              (J.at ["state", "reachable"] J.bool))
      BlinkLight token id -> 
        put
          (String.concat [hueUrl token, id, "/state"])
          <| Http.string <| encodeQuery [("alert", "select")]
      NoRequest -> sendResponse RequestDone)
      (Signal.filterMap
        (\model -> case model.requestQueue of
          head :: _ -> Just head
          [] -> Nothing)
        NoRequest
        modelSignal)

keyboard = {browser = {fullScreen = "F11"}}
emptyChannel = {selectedItemKey = Nothing, items = []}
channel keys = emptyChannel |> (keys |> map (\k -> {key = k}) |> freshItems)
main = (view <| Signal.forwardTo viewMailbox.address Just)
   <~ modelSignal
    ~ Window.dimensions
viewMailbox = Signal.mailbox Nothing
modelSignal =
  Signal.foldp
    (\(Just action) model -> update action model)
    {requestQueue = [GetConfig, PressKeyboard keyboard.browser.fullScreen, GetPhotoItems],
     date = fromTime 0,
     config = {hueToken = ""},
     channel = Guide,
     guide = channel [Photos, Lights],
     photos = {emptyChannel | displayClock = False},
     lights = emptyChannel}
    (Signal.mergeMany
      [viewMailbox.signal,
       responseMailbox.signal,
       Signal.map (Just << Clock << fromTime) (Time.every Time.second),
       Signal.map (Just << KeyboardPressed << Char.fromCode) Keyboard.presses])
