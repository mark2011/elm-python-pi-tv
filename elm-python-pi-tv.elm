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

type alias KeyedList key recordBase = List {recordBase | key: key}
type alias KeyedChannel key recordBase = {maybeKey: Maybe key, records: KeyedList key recordBase}
type alias PhotoBaseRecord = {modTime: Float}
type alias LightBaseRecord = {name: String, on: Bool, reachable: Bool}
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
  | KeyPressed Char
  | SetPhotosList (KeyedList String PhotoBaseRecord)
  | SetLightsList (KeyedList String LightBaseRecord)
  | Error String
type Request
  = NoRequest
  | LogMessage String
  | GetConfig
  | GetPhotosList
  | GetLightsList String
  | SendKey String
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
    prefix channel key =
      if Just key == (model |> channel |> .maybeKey) then "*" else " "
    screen = if
      | model.debug -> drawLines model.log
      | model.channel == Guide ->
        model.guideChannel.records
          |> map (\{key} -> prefix .guideChannel key ++ toString key)
          |> drawLines
      | model.channel == Lights ->
        if model.config.hueToken /= "" then
          model.lightsChannel.records
            |> map (\{key, name, on, reachable} ->
              let
                color = if
                  | not reachable -> Color.red
                  | not on -> Color.blue
                  | otherwise -> Color.white
              in
                (prefix .lightsChannel key ++ name, Text.color color))
          |> drawLines2
        else
          ["", "", "", " Hue token is not configured", "", "", ""]
          |> drawLines
      | model.photosChannel.displayClock -> clockText
      | otherwise -> case model.photosChannel.maybeKey of
          Nothing -> drawLines
            ["",
             "",
             "  Place photos in",
             "    photos folder",
             "",
             ""]
          Just photoFilePath -> layers
            [fittedImage width height photoFilePath |> fullScreen middle,
             overlayText |> opacity overlayOpacity]
    in
      screen |> Graphics.Element.color Color.black |> Html.fromElement

updateRecords keyedChannel newRecords =
  let newMaybeKey = case (keyedChannel.maybeKey, keyedChannel.records, newRecords) of
    (Nothing, _, {key} :: _) -> Just key
    (x, _, _) -> x
  in {keyedChannel | records <- newRecords, maybeKey <- newMaybeKey}

moveKey keyedChannel direction =
  let
    maybeNextKey channel g =
      case (channel.maybeKey, channel.records |> g |> map .key) of
        (_, []) -> Nothing
        (Nothing, firstKey :: _) -> Just firstKey
        (Just key, firstKey :: moreKeys as l) -> Just (nextEntry firstKey l key)
  in
    {keyedChannel | maybeKey <- case direction of
      Stay -> keyedChannel.maybeKey
      Forward -> maybeNextKey keyedChannel identity
      Backward -> maybeNextKey keyedChannel List.reverse
      Rewind -> case keyedChannel.records of
        first :: _ -> Just first.key
        [] -> Nothing}

update action modelWithCompletedRequest =
  let
    model1 = {modelWithCompletedRequest | requestQueue <- List.tail modelWithCompletedRequest.requestQueue ? []}
    (newRequestQueue, newLog) = case (model1.requestQueue, model1.log) of
      ([], next :: tail) -> ([LogMessage next], tail)
      x -> x
    model = {model1 | requestQueue <- newRequestQueue, log <- newLog}
    guideChannel = model.guideChannel
    photosChannel = model.photosChannel
    lightsChannel = model.lightsChannel
    log message = model.log ++ [toString message]
  in
    case action of
      RequestDone -> model
      Error error -> {model | log <- model.log ++ [error]}
      SetConfig config -> {model | config <- config, log <- log config}
      SetPhotosList photos ->
        {model | photosChannel <- updateRecords photosChannel photos}
      SetLightsList lights ->
        {model | lightsChannel <- updateRecords lightsChannel lights}
      Clock date ->
        let
           moved = moveKey photosChannel <| if isAdvancePhotoTime date then Forward else Stay
           newPhotosChannel = {moved | displayClock <- isShowClockTime date}
        in
          {model |
           date <- date,
           requestQueue <-
             appendIf (isPollTime date) model.requestQueue [GetPhotosList, GetLightsList model.config.hueToken],
           photosChannel <- newPhotosChannel}
      KeyPressed key -> case key of
        'd' -> {model | debug <- not model.debug}
        'e' -> {model | channel <- Guide}
        _ -> case model.channel of
          Guide ->
            let f direction = {model | guideChannel <- moveKey guideChannel direction}
            in case key of
              'j' -> f Forward
              'k' -> f Backward
              'y' -> {model | channel <- guideChannel.maybeKey ? Guide}
              _ -> model
          Photos ->
            let f direction = {model | photosChannel <- moveKey photosChannel direction}
            in case key of
              '0' -> f Rewind
              'h' -> f Backward
              'l' -> f Forward
              _ -> model
          Lights -> case key of
            'y' ->
              {model | requestQueue <- model.requestQueue ++ [BlinkLight model.config.hueToken <| lightsChannel.maybeKey ? ""]}
            _ ->
              let f direction = {model | lightsChannel <- moveKey lightsChannel direction}
              in case key of
                '0' -> f Rewind
                'j' -> f Forward
                'k' -> f Backward
                _ -> model

quote s = "\"" ++ s ++ "\""
encodeQuery = K.encode 0 << K.object << map (\(k, v) -> (k, K.string v))

responseMailbox = Signal.mailbox Nothing
port requestPort: Signal (Task () ())
port requestPort =
  let
    localServer = "http://localhost:3000/"
    huePrefix token = String.concat ["http://philips-hue/api/", token, "/lights/"]
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
      SendKey key ->
        simple "send-key" "key" key requestDone
      GetPhotosList ->
        simple "list-folder" "folder" "/home/pi/Pictures"
          <| J.object1 (List.sortBy .modTime >> SetPhotosList) <| J.list <| J.tuple2 (\path modTime -> {key = path, modTime = modTime}) J.string J.float
      GetLightsList token ->
        if token /= "" then
          get
            (huePrefix token)
            <| J.object1
              (map (\(id, light) -> {light | key = id}) >> List.sortBy (.name >> String.toUpper) >> SetLightsList)
              (J.keyValuePairs <| J.object3 (\name on reachable -> {name = name, on = on, reachable = reachable})
                ("name" := J.string)
                (J.at ["state", "on"] J.bool)
                (J.at ["state", "reachable"] J.bool))
        else
          sendResponse <| Error "no hue token"
      BlinkLight token id -> 
        if token /= "" then
          put
            (String.concat [huePrefix token, id, "/state"])
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

emptyChannel = {maybeKey = Nothing, records = []}
channel keys = keys |> map (\k -> {key = k}) |> updateRecords emptyChannel
main = (view <| Signal.forwardTo viewMailbox.address Just)
   <~ modelSignal
    ~ Window.dimensions
viewMailbox = Signal.mailbox Nothing
modelSignal =
  Signal.foldp
    (\(Just action) model -> update action model)
    {debug = False,
     requestQueue = [GetConfig, SendKey "F11", GetPhotosList],
     date = fromTime 0,
     log = [],
     config = {hueToken = ""},
     channel = Guide,
     guideChannel = channel [Photos, Lights],
     photosChannel = {emptyChannel | displayClock = False},
     lightsChannel = emptyChannel}
    (Signal.mergeMany
      [viewMailbox.signal,
       responseMailbox.signal,
       Signal.map (Just << Clock << fromTime) (Time.every Time.second),
       Signal.map (Just << KeyPressed << Char.fromCode) Keyboard.presses])
