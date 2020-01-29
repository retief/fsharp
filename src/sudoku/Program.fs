module Sudoku

open System
open Microsoft.FSharp.Collections

let join sep (lst: seq<String>) = String.Join(sep, lst)

let flattenArray arr =
  seq {
    for x in [ 0 .. Array2D.length1 arr - 1 ] do
      for y in [ 0 .. Array2D.length2 arr - 1 ] do
        yield arr.[x, y]
  }
  |> Array.ofSeq

type Cell =
  | Filled of int
  | Empty of int Set

type Board =
  { Cells: Cell [,] }
  override this.ToString() =
    let printCell c =
      match c with
      | Filled i -> string i
      | Empty set -> Set.toList set |> sprintf "%A"

    let strings = Array2D.map printCell this.Cells
    let maxForColumn x = Array.map (fun (s: String) -> s.Length) strings.[x, *] |> Array.max
    let maxPerColumn = Array.ofList [ 0 .. 8 ] |> Array.map maxForColumn
    let justify len (str: String) = str.PadRight len

    let formatLine y =
      strings.[*, y]
      |> Array.map2 justify maxPerColumn
      |> join " "
    List.map formatLine [ 0 .. 8 ]
    |> join "\n"

let mkBoard cells = { Cells = cells }

let parseCell c =
  match c with
  | '.' -> Set.ofList [ 1 .. 9 ] |> Empty
  | _ -> int c - int '0' |> Filled


let parseIdx (rawBoard: String) x y = rawBoard.[x + y * 9] |> parseCell

let parseBoard rawBoard =
  parseIdx rawBoard
  |> Array2D.init 9 9
  |> mkBoard

let boards = Array.map parseBoard <| Boards.boards.Split("\n")

let cleanCell board x y currentCell =
  match currentCell with
  | Filled _ -> currentCell
  | Empty opts ->
      let getVal c =
        match c with
        | Filled n -> Some n
        | Empty _ -> None

      let squareX, squareY = x / 3 * 3, y / 3 * 3

      [ board.Cells.[x, *]
        board.Cells.[*, y]
        flattenArray board.Cells.[squareX..squareX + 2, squareY..squareY + 2] ]
      |> Seq.concat
      |> Seq.choose getVal
      |> Set.ofSeq
      |> (-) opts
      |> Empty

let clean board = Array2D.mapi (cleanCell board) board.Cells |> mkBoard

let set board x y v =
  let handleCell currentX currentY currentCell =
    if x = currentX && y = currentY then
      Filled v
    elif x = currentX || y = currentY || (x / 3 = currentX / 3 && y / 3 = currentY / 3) then
      match currentCell with
      | Filled _ -> currentCell
      | Empty opts -> Set.remove v opts |> Empty
    else
      currentCell
  Array2D.mapi handleCell board.Cells |> mkBoard

let shortestCell board =
  let cellLength cell =
    match cell with
    | Filled _ -> 10
    | Empty opts -> Set.count opts

  let keepShortest ((_, _, bestLen) as best) ((_, _, currentLen) as current) =
    if currentLen < bestLen then current else best

  seq {
    for x in 0 .. 8 do
      for y in 0 .. 8 do
        yield (x, y, cellLength board.Cells.[x, y])
  }
  |> Seq.fold keepShortest (-1, -1, 10)

let solve board =
  let rec iter board =
    match shortestCell board with
    | (_, _, 10) -> Some board
    | (_, _, 0) -> None
    | (x, y, len) ->
        match board.Cells.[x, y] with
        | Filled _ ->
            printfn "Shortest cell is filled?"
            None
        | Empty opts ->
            let setAndSolve = set board x y >> iter
            Seq.tryPick setAndSolve opts
  clean board |> iter

let solveAndPrint board =
  printfn "-----------------"
  match solve board with
  | None -> printfn "Solve failed"
  | Some board -> printfn "%O" board

module Main =
  [<EntryPoint>]
  let main argv =
    Seq.iter solveAndPrint boards
    0
