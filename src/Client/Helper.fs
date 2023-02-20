module Helper

open Elmish
open Fable.Remoting.Client
open Shared
open Browser
open Fable.Core.JsInterop
open System
open Fable.React.DrawingCanvas

"
a,b,c
c,d
e,f,g
"

let getEingebettetInArray(s : string) =
    let arr = [||]
    let lines = s.Split("\r\n")
    let mutable index = 0
    [ for line in lines do
        if line.Trim() <> "" then
            let letters = line.Split(",")
            for letterIndex, letter in (letters |> Seq.indexed) do
                arr.[letterIndex] <- index
    ]

    
    
    
    
