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
  in keyedChannel |> updateItems (always newItems) |> updateSelectedItemKey (always newSelectedItemKey)

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

updateItems f r = {r | items <- f r.items}
updateSelectedItemKey f r = {r | selectedItemKey <- f r.selectedItemKey}
updateDisplayClock f r = {r | displayClock <- f r.displayClock}
updateConfig f r = {r | config <- f r.config}
updateChannel f r = {r | channel <- f r.channel}
updateRequestQueue f r = {r | requestQueue <- f r.requestQueue}
updateDate f r = {r | date <- f r.date}
updateGuide f r = {r | guide <- f r.guide}
updatePhotos f r = {r | photos <- f r.photos}
updateLights f r = {r | lights <- f r.lights}

addRequest request m = {m | requestQueue <- m.requestQueue ++ [request]}
log = toString >> LogMessage >> addRequest
update action rawModel =
  rawModel
  |> updateRequestQueue (List.tail >> flip (?) [])
  |> (\model -> model |> (case action of
      RequestDone -> identity
      Error error -> log error
      SetConfig config -> updateConfig (always config) >> log config
      SetPhotoItems items -> updatePhotos (freshItems items)
      SetLightItems items -> updateLights (freshItems items)
      Clock date ->
        updateDate (always date)
        >> ((if isAdvancePhotoTime date then Forward else Stay) |> moveKey |> updatePhotos)
        >> (isShowClockTime date |> always |> updateDisplayClock |> updatePhotos)
        >> (if not (isPollTime date) then identity else
             (addRequest GetPhotoItems >> addRequest (GetLightItems model.config.hueToken)))
      KeyboardPressed press -> case press of
        'e' -> always Guide |> updateChannel
        _ -> case model.channel of
          Guide -> case press of
            'y' -> model.guide.selectedItemKey ? Guide |> always |> updateChannel
            _ -> (case press of
              'j' -> Forward
              'k' -> Backward
              '0' -> Rewind
              _ -> Stay) |> moveKey |> updateGuide
          Photos -> (case press of
            'l' -> Forward
            'h' -> Backward
            '0' -> Rewind
            _ -> Stay) |> moveKey |> updatePhotos
          Lights -> case press of
            'y' -> model.lights.selectedItemKey ? "" |> BlinkLight model.config.hueToken |> addRequest
            _ -> (case press of
              'j' -> Forward
              'k' -> Backward
              '0' -> Rewind
              _ -> Stay) |> moveKey |> updateLights))

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
        simple "press-keyboard" "press" press requestDone
      GetPhotoItems ->
        simple "list-folder" "folder" "/home/pi/Pictures"
          <| J.object1 (List.sortBy .modTime >> SetPhotoItems) <| J.list <| J.tuple2 (\path modTime -> {key = path, modTime = modTime}) J.string J.float
      GetLightItems token ->
        if token /= "" then
          get
            (hueUrl token)
            <| J.object1
              (map (\(id, light) -> {light | key = id}) >> List.sortBy (.name >> String.toUpper) >> SetLightItems)
              (J.keyValuePairs <| J.object3 (\name on reachable -> {name = name, on = on, reachable = reachable})
                ("name" := J.string)
                (J.at ["state", "on"] J.bool)
                (J.at ["state", "reachable"] J.bool))
        else
          sendResponse <| Error "no hue token"
      BlinkLight token id -> 
        if token /= "" then
          put
            (String.concat [hueUrl token, id, "/state"])
            <| Http.string <| encodeQuery [("alert", "select")]
        else
          succeed ()
      NoRequest -> succeed ())
      (Signal.filterMap
        (\model -> case model.requestQueue of
          head :: _ -> Just head
          [] -> Nothing)
        NoRequest
        modelSignal)

emptyChannel = {selectedItemKey = Nothing, items = []}
channel keys = emptyChannel |> (keys |> map (\k -> {key = k}) |> freshItems)
main = (view <| Signal.forwardTo viewMailbox.address Just)
   <~ modelSignal
    ~ Window.dimensions
viewMailbox = Signal.mailbox Nothing
modelSignal =
  Signal.foldp
    (\(Just action) model -> update action model)
    {requestQueue = [GetConfig, PressKeyboard "F11", GetPhotoItems],
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
