module Synthesis

open System

let abelar a =(a>12)&&(a<3097)&&(a%12=0)
failwith "Not implemented"

let area a b = match a<0.0||b<0.0 with
                |true -> failwith "One or both inputs are negative";
                |false -> 0.5*a*b
failwith "Not implemented"

let zollo a = match a<0 with
               |true -> a*(-1)
               |false -> a*2
failwith "Not implemented"

let min a b = match a>b with
                |true -> b
                |false -> a
failwith "Not implemented"

let max a b = match a>b with
                |true -> a
                |false -> b
failwith "Not implemented"

let ofTime h m s = h*60*60+m*60+s
failwith "Not implemented"

let toTime a =
    match a<0 with
        |true -> 0,0,0
        |false ->
            let h = a/3600
            let z = a-(h*60*60)
            (h,z/60,z%60)
failwith "Not implemented"

let digits _ =
    failwith "Not implemented"

let minmax _ =
    failwith "Not implemented"

let isLeap _ =
    failwith "Not implemented"

let month _ =
    failwith "Not implemented"

let toBinary _ =
    failwith "Not implemented"

let bizFuzz _ =
    failwith "Not implemented"

let monthDay _ _ =
    failwith "Not implemented"

let coord _ =
    failwith "Not implemented"