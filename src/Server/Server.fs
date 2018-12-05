open System.IO
open Microsoft.AspNetCore.Builder
open FSharp.Control.Tasks.V2
open Giraffe
open Saturn
open Shared

let publicPath = Path.GetFullPath "../Client/public"
let port = 8085us
let mutable x = 4.
let random = System.Random(1234)
let mutable timer : System.Timers.Timer option = None

let webApp =
    router {
        get "/api/generate" (fun next ctx -> 
            task {
                match timer with
                | Some t -> ()
                | None -> 
                    let t = new System.Timers.Timer(1000.)
                    t.Elapsed
                    |> Event.add (fun n -> 
                           let y = random.NextDouble() * 10.
                           
                           let t =
                               {x = x
                                y = y}
                           
                           let z = Thoth.Json.Net.Encode.Auto.toString (2, t)
                           (WebSockets.sendMessageToSockets z).Wait()
                           x <- x + 1.)
                    t.Start()
                    timer <- Some t
                    ()
                return! Successful.OK "Hello world" next ctx
            })
        get "/api/init" (fun next ctx -> 
            task {
                let data =
                    [|{x = 1.
                       y = 2.}
                      {x = 2.
                       y = 5.}
                      {x = 3.
                       y = 1.}|]
                return! Successful.OK data next ctx
            })
    }

let app =
    application {
        url ("http://0.0.0.0:" + port.ToString() + "/")
        use_router webApp
        memory_cache
        use_static publicPath
        use_json_serializer (Thoth.Json.Giraffe.ThothSerializer())
        use_gzip
        app_config (fun ab -> ab.UseWebSockets().UseMiddleware<WebSockets.WebSocketMiddleware>())
    }

run app
