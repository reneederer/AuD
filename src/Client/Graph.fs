module Graph

open Elmish
open Fable.Remoting.Client
open Shared
open Browser
open Fable.Core.JsInterop
open System
open Fable.React.DrawingCanvas
open Feliz.Plotly

type GraphModel =
    { Graph : string
      Step : int
    }


type Msg =
    | SetGraph of string
    | Step of int

let init () : GraphModel * Cmd<Msg> =
    let model =
        { Graph = ""
          Step = 0
        }

    model, Cmd.none

let update (msg: Msg) (model: GraphModel) : GraphModel * Cmd<Msg> =
    match msg with
    | SetGraph graphStr ->
        { model with Graph = graphStr }, Cmd.none
    | Step step ->
        { model with Step = step }, Cmd.none

open Feliz
open Feliz.Bulma
open type Feliz.Html
open Fable.React
open Feliz 

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



let view (model: GraphModel) (dispatch: Msg -> unit) =
    div
        []
        [ table
            [
            ]
            []
        ]








