module Dijkstra

open Elmish
open Fable.Remoting.Client
open Shared
open Browser
open Fable.Core.JsInterop
open System
open Fable.React.DrawingCanvas

type DijkstraModel =
    { Graph : string
      Step : int
    }


type Msg =
    | SetGraph of string
    | Step of int

let init () : DijkstraModel * Cmd<Msg> =
    let model =
        { Graph = ""
          Step = 0
        }

    model, Cmd.none

let update (msg: Msg) (model: DijkstraModel) : DijkstraModel * Cmd<Msg> =
    match msg with
    | SetGraph graphStr ->
        { model with Graph = graphStr }, Cmd.none
    | Step step ->
        { model with Step = step }, Cmd.none

open Feliz
open Feliz.Bulma
open Feliz.Bulma.Bulma
open type Feliz.Html
open Fable.React
open System
open Feliz 
open Feliz.style

let getGraph (s : string) =
    let lines = s.Split("\r\n")
    (*
A B 15
B C 20
    *)
    [ for line in lines do
        let arr = line.Split(" ")
        arr.[0], ([ arr.[1], arr.[2]] |> Map.ofList)
    ]
    |> Map.ofList



let view (model: DijkstraModel) (dispatch: Msg -> unit) =
    div
        []
        [ textarea
            [ Props.Style [ Props.CSSProp.MinHeight 400]
              Props.OnChange (fun (evt : Browser.Types.Event) -> dispatch <| SetGraph evt.target?value)
            ]
            []
          drawingcanvas {
            Redraw = DrawFunction (fun ctx ->
                ctx.canvas.width <- 2000.0
                ctx.canvas.height <- 300.0
                ctx.font <- "15px Arial, plain"
                let mutable currentX = window.screen.width / 2.
                let mutable currentY = 50.
                let mutable shiftX = 200.
                let mutable shiftY = 80.
                let graph = getGraph model.Graph
                for kv in graph do
                    ()
                ctx.stroke()
            )
            Props = [ ]
          }

          h2 [] [str ($"CurrentStep: {model.Step}")]
          Bulma.button.button
            [ prop.onClick (fun _ -> dispatch (Step (model.Step + 1)))
              prop.children
                [ str $"Next"]
            ]
          Bulma.button.button
            [ prop.onClick (fun _ -> dispatch (Step (model.Step - 1)))
              prop.children
                [ str $"Previous"]
            ]


        ]








