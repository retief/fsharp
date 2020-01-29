module Probability

open System
open Microsoft.FSharp.Collections

let update f k v m =
  match Map.tryFind k m with
  | None -> Map.add k v m
  | (Some newV) -> Map.add k (f v newV) m

let hist width lst =
  let format = sprintf "%O"
  let maxP = Seq.map snd lst |> Seq.fold max 0.0

  let maxLen =
    Seq.map
      (fst
       >> format
       >> String.length) lst
    |> Seq.fold max 0

  let formatLine (v, p) =
    let barLen =
      p / maxP * float width
      |> System.Math.Round
      |> int

    let vStr = format v
    let padding = String.replicate (maxLen - String.length vStr) " "
    let bar = String.replicate barLen "#"
    sprintf "%1.2e: %s %s$ %s" p vStr padding bar

  String.Join("\n", Seq.sortBy fst lst |> Seq.map formatLine)

type Prob<'t when 't: comparison> =
  { Probabilities: Map<'t, float> }
  member this.GetVals() = Map.toSeq this.Probabilities
  override this.ToString() = this.GetVals() |> hist 40

let mkProb probs =
  let add m (k, v) = update (+) k v m
  { Probabilities = Seq.fold add Map.empty probs }

let getVals (p: Prob<_>) = p.GetVals()

let (>>=) p f =
  [ for k, v in getVals p do
      for k', v' in f k |> getVals do
        yield k', v * v' ]
  |> mkProb

let wrap v = mkProb [ (v, float 1) ]

type ProbCE() =
  member this.Bind(v, f) = v >>= f
  member this.Return v = wrap v

let prob = ProbCE()

let map f p =
  let mapVal (v, w) = (f v, w)
  getVals p
  |> Seq.map mapVal
  |> mkProb

let weighted kws =
  let total = Seq.map snd kws |> Seq.sum
  let rationalize (k, w) = (k, float w / float total)
  Seq.map rationalize kws
  |> List.ofSeq
  |> mkProb

let fair ks = Seq.map (fun k -> (k, 1)) ks |> weighted

let die n = fair [ 1 .. n ]

let rec repeatWith f zero n p =
  if n <= 0 then
    wrap zero
  else
    prob {
      let! current = p
      let! rest = repeatWith f zero (n - 1) p
      return f rest current }

let repeat = repeatWith (fun lst v -> v :: lst |> List.sort) []

let pointCost n =
  match n with
  | _ when n < 8 -> 0
  | _ when n < 15 -> n - 8
  | 15 -> 8
  | 16 -> 10
  | 17 -> 13
  | 18 -> 16
  | _ -> 0

let statRoll diceCount =
  let sumTop vs =
    List.sort vs
    |> List.rev
    |> List.take 3
    |> List.sum
  die 6
  |> repeat diceCount
  |> map sumTop

let pointBuyOfRoll diceCount = statRoll diceCount |> map pointCost

let pointBuyOfRolls diceCount = pointBuyOfRoll diceCount |> repeatWith (+) 0 6

let advantage_ f sides bar =
  prob {
    let! v1 = die sides
    let! v2 = die sides
    return f v1 v2 >= bar }

let advantage = advantage_ max
let disadvantage = advantage_ min

let print v = printfn "%O" v

module Main =
  [<EntryPoint>]
  let main argv =
    pointBuyOfRolls 5 |> printfn "%A"
    0 // return an integer exit code
