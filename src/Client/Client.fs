module Client

open Elmish
open Elmish.React
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.PowerPack.Fetch
open Fable
open Fable.Recharts
open Fable.Recharts.Props
open Shared
open Fulma
open Fable.Import.JS

type Model =
    {Data : DataPoint []
     PhotoUrl : string option}

type Msg =
    | GenerateRandomData
    | GenerateInitalized
    | GenerateFailed
    | GetPoint of DataPoint
    | LoadInitialData
    | InitDataLoaded of DataPoint []
    | InitDataLoadingFailed
    | GetRandomPhoto
    | GetRandomPhotoInitalized
    | GetRandomPhotoFailed
    | RandomPhotoLoaded of DataUrl

let init() : Model * Cmd<Msg> =
    let initialModel =
        {Data = [||]
         PhotoUrl = None}
    
    let prom = fetchAs<DataPoint []> "/api/init" (Thoth.Json.Decode.Auto.generateDecoder()) []
    let cmd = Cmd.ofPromise (fun _ -> prom) () InitDataLoaded (fun _ -> InitDataLoadingFailed)
    initialModel, cmd

let update (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> =
    match msg with
    | GetPoint pnt -> 
        {currentModel with Data =
                               [|yield! currentModel.Data
                                 yield pnt|]}, Cmd.none
    | GenerateRandomData -> 
        let cmd =
            Cmd.ofPromise (fun _ -> fetch "/api/generate" []) () (fun _ -> GenerateInitalized) (fun _ -> GenerateFailed)
        currentModel, cmd
    | InitDataLoaded d -> {currentModel with Data = d}, Cmd.none
    | GenerateInitalized -> currentModel, Cmd.none
    | GenerateFailed -> currentModel, Cmd.none
    | LoadInitialData -> currentModel, Cmd.none
    | InitDataLoadingFailed -> currentModel, Cmd.none
    | GetRandomPhoto -> 
        let cmd =
            Cmd.ofPromise (fun _ -> fetch "/api/photo" []) () (fun _ -> GetRandomPhotoInitalized) 
                (fun _ -> GetRandomPhotoFailed)
        currentModel, cmd
    | RandomPhotoLoaded d -> {currentModel with PhotoUrl = Some d.Url}, Cmd.none
    | GetRandomPhotoInitalized -> failwith "Not Implemented"
    | GetRandomPhotoFailed -> failwith "Not Implemented"

let safeComponents =
    let components =
        span [] [a [Href "https://saturnframework.github.io"] [str "Saturn"]
                 str ", "
                 a [Href "http://fable.io"] [str "Fable"]
                 str ", "
                 a [Href "https://elmish.github.io/elmish/"] [str "Elmish"]
                 str ", "
                 a [Href "https://mangelmaxime.github.io/Fulma"] [str "Fulma"]]
    p [] [strong [] [str "SAFE Template"]
          str " powered by: "
          components]

let button txt onClick =
    Button.button [Button.IsFullWidth
                   Button.Color IsPrimary
                   Button.OnClick onClick] [str txt]

let chart data =
    Recharts.lineChart [Chart.Data data
                        Chart.Width 1000.
                        Chart.Height 300.] [line [Cartesian.Type Monotone
                                                  Cartesian.DataKey "y"] []
                                            xaxis [Cartesian.DataKey "x"] []
                                            yaxis [] []]

let view (model : Model) (dispatch : Msg -> unit) =
    let image =
        if model.PhotoUrl.IsSome then img [Src model.PhotoUrl.Value]
        else nothing
    div [] 
        [Navbar.navbar [Navbar.Color IsPrimary] [Navbar.Item.div [] [Heading.h2 [] [str "SAFE Template"]]]
         Container.container [] [Content.content [] [chart model.Data
                                                     
                                                     button "Generate Random data" 
                                                         (fun _ -> dispatch GenerateRandomData)
                                                     image
                                                     button "Generate Random pics" (fun _ -> dispatch GetRandomPhoto)]]
         
         Footer.footer [] 
             [Content.content [Content.Modifiers [Modifier.TextAlignment(Screen.All, TextAlignment.Centered)]] 
                  [safeComponents]]]

let timer initial =
    let sub dispatch =
        let socket = Fable.Import.Browser.WebSocket.Create("ws://localhost:8085/ws")
        socket.addEventListener_open (fun _ -> console.log "Socket connected")
        socket.addEventListener_message (fun msg -> 
            let data = msg.data |> unbox<string>
            let pointRes = Thoth.Json.Decode.Auto.fromString<Msg<obj>> data
            match pointRes with
            | Error msg -> console.log ("Socket msg failed:", msg)
            | Ok msg when msg.Type = "point" -> 
                msg.Data
                |> unbox<DataPoint>
                |> GetPoint
                |> dispatch
            | Ok msg when msg.Type = "photo" -> 
                msg.Data
                |> unbox<DataUrl>
                |> RandomPhotoLoaded
                |> dispatch)
        socket.addEventListener_close (fun _ -> console.log "Socket closed")
    Cmd.ofSub sub
#if DEBUG

open Elmish.Debug
open Elmish.HMR
#endif


Program.mkProgram init update view
|> Program.withSubscription timer
#if DEBUG
|> Program.withConsoleTrace
|> Program.withHMR
#endif

|> Program.withReact "elmish-app"
// #if DEBUG
// |> Program.withDebugger
// #endif
|> Program.run
