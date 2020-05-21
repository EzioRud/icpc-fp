module ICPC
open System

let nextElem  = function 
    | [] -> "fail"
    | hd :: tl -> hd

let makeList (toConvert : string) =
    toConvert.Split(' ') |> Array.toList


let rec filter predicate = function 
    | [] -> []
    | hd :: tl ->
        match predicate hd with
        | true -> (nextElem tl)::filter predicate tl
        | false -> filter predicate tl


let rec filter2 predicate2 (input : string list) = match input with 
                                                    | [] -> []
                                                    | hd :: tl ->
                                                        match predicate2 hd with
                                                        | true -> (hd.Substring(0, (hd.Length - 1)))::filter2 predicate2 tl
                                                        | false -> filter2 predicate2 tl
    
let lastComma (sting : string) = 
    match sting.Substring((sting.Length - 1)) = "," with
            | true -> true
            | _ -> false 

let Comma1 (sting : string) = 
    match sting.Substring((sting.Length - 1)) = "," with
            | true -> true
            | _ -> false 
  
let rec commaWord (listless : string list) = match listless with  
                                                        | [] -> []
                                                        | hd :: tl ->
                                                            match  hd.Substring(hd.Length - 1) with
                                                            | "," -> hd::(hd.Substring(0, (hd.Length - 1)))::(hd.Substring(0, (hd.Length - 1))) + "."::commaWord tl
                                                            | "." -> hd::(hd.Substring(0, (hd.Length - 1)))::(hd.Substring(0, (hd.Length - 1))) + ","::commaWord tl
                                                            | _ -> hd::hd + "."::hd + ","::commaWord tl
                                                           
let findString (text : string) (items : string list) =
    items |> List.tryFind(fun item -> item.Contains(text));;


let rec check test (listin : string list) = match listin with
                                                        | [] -> []
                                                        | hd :: tl ->
                                                            match hd.Substring((hd.Length - 1)) = "," || hd.Substring((hd.Length - 1)) = "." with
                                                                | true -> hd::check test tl 
                                                                | _ -> match findString (nextElem tl) test with 
                                                                            | None -> hd::check test tl 
                                                                            | _ -> hd + ","::check test tl 


let rec check1 test (listin : string list) = match listin with 
                                                        | [] -> []
                                                        | hd :: tl -> match hd.Substring((hd.Length - 1)) = "," || hd.Substring((hd.Length - 1)) = "." with 
                                                                        | true -> hd::check1 test tl
                                                                        | _ ->
                                                                             match findString hd test with 
                                                                                | None -> hd::check1 test tl 
                                                                                | _ -> hd + ","::check1 test tl 

let rec fullStop (input : string ) =  
    match input.Substring((input.Length - 1)) = "." with
            | true -> false
            | _ -> true 

let rec letter1 (input : string) = 
    match Char.IsLetter(input, 0) with 
        | true -> false
        | _ -> true

let charElement  = function
    | [] -> ' '
    | hd :: tl -> hd

let noWhiteSpace (input : string) = 
    let charArr = input.ToCharArray();  
    let charList = charArr |> Array.toList
    let rec checkSpace  = function
            | [] -> false
            | hd :: tl ->
                match (hd = '?') || ((hd = ' ') && ((charElement tl) = ' ')) || ((hd = '.') && ((charElement tl) = '.')) || Char.IsUpper(hd) || ((hd = ',') && ((charElement tl) <> ' ')) || ((hd = ' ') && ((charElement tl) = '.')) with 
                    | true -> true 
                    | _ -> checkSpace tl

    checkSpace charList


let commaSprinkler (input:string) =
    let rec sprink inp = 
                let list1 = makeList inp 
                let CommaWords = list1 |> filter lastComma 
                let CommaWords1 = CommaWords |> commaWord
                let CommaWords2 = list1 |> filter2 Comma1 
                let input1 = check CommaWords1 list1 
                let xy = check1 CommaWords2 input1 
                let result = String.concat " " xy 
                match result = inp with  
                            | true -> result 
                            | _ -> sprink result 
    match (input.Length < 2) || (fullStop input) || (letter1 input) || (noWhiteSpace input) with 
        | true -> None 
        | _ -> Some(sprink input)

let rivers input =
    failwith "Not implemented"

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
