module Synthesis

open System

let abelar a =
    (a>12)&&(a<3097)&&(a%12=0)


let area a b = 
    match a<0.0||b<0.0 with
       |true -> failwith "One or both inputs are negative";
       |false -> 0.5*a*b


let zollo a =
    match a<0 with
      |true -> a*(-1)
      |false -> a*2


let min a b = 
    match a>b with
      |true -> b
      |false -> a


let max a b = 
    match a>b with
       |true -> a
       |false -> b


let ofTime h m s =
       h*60*60+m*60+s


let toTime a =
    match a<0 with
        |true -> 0,0,0
        |false ->
            let h = a/3600
            let z = a-(h*60*60)
            (h,z/60,z%60)


let digits a =
    let rec count n acc =
        match (a>=n)||(a<=(-n)) with
            |false -> acc
            |_ -> count (n*10) (acc+1)
    count 10 1


let minmax a =
    let a,b,c,d = a
    (min a b |> min c |> min d, max a b |> max c |> max d)

    

let isLeap a =
    match a<1582 with
        |true -> failwith "Number is less than 1582"
        |_ -> match (a%4)=0&&(a%100)<>0 with
                |true -> true
                |_ -> (a%400)=0
        


let month = function
        |1 -> ("January",31)
        |2  -> ("February",28)
        |3 -> ("March",31)
        |4 -> ("April",30)
        |5 -> ("May",31)
        |6 -> ("June",30)
        |7 -> ("July",31)
        |8 -> ("August",31)
        |9 -> ("September",30)
        |10 -> ("October",31)
        |11 -> ("November",30)
        |12 -> ("December",31)
        |_ -> failwith "Integer is less than 1 or greater than 12"


let toBinary a =
    match (a<0,a=0) with
    |true,_ -> failwith "Please enter a positive number"
    |_,true -> "0"
    |false,false ->
         let rec findBinary a rez =       
              match a>0 with
                    |false -> rez
                    |true -> match a%2 with
                                |0 -> findBinary (a/2) ("0"+rez)
                                |_ -> findBinary (a/2) ("1"+rez)
         findBinary a ""
                                
                

let bizFuzz n =
     let rec numBetween counter (a,b,c) =
        match counter<=n with
            |false -> a,b,c
            |_ ->
                match counter%3=0,counter%5=0 with
                    |true,true -> numBetween (counter+1) (a+1,b+1,c+1)
                    |true,false -> numBetween (counter+1) (a+1,b,c)
                    |false,true -> numBetween (counter+1) (a,b+1,c)
                    |_,_ -> numBetween (counter+1) (a,b,c)
     numBetween 3 (0,0,0)


let monthDay days year =
    let _ = 
        match (isLeap year,(days >= 1 && days <= 365),days  >=1 && days <= 366) with
            |true,_,false -> failwith "Invalid number of days entered"
            |false,false,_ -> failwith "Invalid number of days entered"
            |_,_,_-> ()
        
    let rec myMonth counter acc =
        let month,numdays =
            match (counter=2) && ((isLeap year) = true) with
                |true -> "February", 29
                |_ -> month counter
        match acc+numdays >= days with
            |true -> month
            |_ -> myMonth (counter+1) (acc+numdays)
    myMonth 1 0
                    


let sqrt n = 
    let rec calculate guess i =
        match i with
            |10 -> guess
            |_ ->
                let g = (guess + n/guess) / 2.0
                calculate g (i+1)
    match n <= 0.0 with
        |true -> failwith "Impossible"
        |_ ->
            calculate (n/2.0) 0

let coord (inX,inY) =
      let distance = fun (x2,y2) ->
            let xx,yy = inX-x2,inY- y2
            sqrt ((xx*xx) + (yy*yy))
      let rectAn = fun (lefX,lefY) width height ->
            (inX >= lefX && inX <= lefX+height) && (inY<= lefY && inY >= lefY-width )
      (distance,rectAn)
        
   