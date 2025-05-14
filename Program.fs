module GaussianIntegers

open System.Diagnostics
open System

let customSeriesSum (a: int64) (b: int64) : int64 =
    match a = b with
    | true -> a + b
    | false -> (a + b) * 2L

let rec gcd (a: int64) (b: int64) : int64 =
    match b with
    | 0L -> a
    | _ -> gcd b (a % b)

let rec sumIntegerDivisors (limit: int) (i: int) (acc: int64) : int64 =
    match i > limit with
    | true -> acc
    | false ->
        let count = int64 (limit / i)
        let newAcc = acc + count * int64 i
        sumIntegerDivisors limit (i + 1) newAcc


let rec accumulateComplexDivisor (limit: int) (modulusSquared: int) (value: int64) (j: int) (acc: int64) : int64 =
    match modulusSquared * j > limit with
    | true -> acc
    | false ->
        let count = int64 (limit / (modulusSquared * j))
        let contribution = int64 j * value * count
        accumulateComplexDivisor limit modulusSquared value (j + 1) (acc + contribution)

let rec processImaginaryParts (limit: int) (a: int) (b: int) (acc: int64) : int64 =
    match b > a with
    | true -> acc
    | false ->
        match gcd (int64 a) (int64 b) with
        | 1L ->
            let modulusSquared = a * a + b * b
            let value = customSeriesSum (int64 a) (int64 b)
            let newAcc = accumulateComplexDivisor limit modulusSquared value 1 acc
            processImaginaryParts limit a (b + 1) newAcc
        | _ -> processImaginaryParts limit a (b + 1) acc


let rec processRealParts (limit: int) (a: int) (maxReal: int) (acc: int64) : int64 =
    match a > maxReal with
    | true -> acc
    | false ->
        let newAcc = processImaginaryParts limit a 1 acc
        processRealParts limit (a + 1) maxReal newAcc

let calculateResult (limit: int) : int64 =
    let secondLimit = int (sqrt (float limit))
    let sum1 = sumIntegerDivisors limit 1 0L
    processRealParts limit 1 secondLimit sum1

[<EntryPoint>]
let main _ =
    //let limit = 100_000_000
    //let limit = 5
    let limit = int (pown 10 8)
    let result = calculateResult limit
    Console.WriteLine("Result: {0}", result)
    0
