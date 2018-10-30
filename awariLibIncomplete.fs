module Awari
type pit = int
type board = string
type player = Player1 | Player2

// intentionally many missing implementations and additions
let b = [3;3;3;3;3;3;3;0;0;3;3;3;3;3;3]

let rec printBoard b =
  printfn "%4i%4i%4i%4i%4i%4i%4i" b.[0] b.[1] b.[2] b.[3] b.[4] b.[5] b.[6]
  printfn "%20i%20i" b.[7] b.[8]
  printfn "%4i%4i%4i%4i%4i%4i%4i" b.[9] b.[10] b.[11] b.[12] b.[13] b.[14] b.[15]

let rec isHome b p i =
  match p with
  | Player1 -> i = b.[7]
  | Player2 -> i = b.[8]


let turn (b : board) (p : player) : board =
  let rec repeat (b: board) (p: player) (n: int) : board =
    printBoard b
    let str =
      if n = 0 then
        sprintf "Player %A's move? " p
      else
        "Again? "
    let i = getMove b p str
    let (newB, finalPitsPlayer, finalPit)= distribute b p i
    if not (isHome b finalPitsPlayer finalPit)
       || (isGameOver b) then
      newB
    else
      repeat newB p (n + 1)
  repeat b p 0

let rec play (b : board) (p : player) : board =
  if isGameOver b then
    b
  else
    let newB = turn b p
    let nextP =
      if p = Player1 then
        Player2
      else
        Player1
    play newB nextP
