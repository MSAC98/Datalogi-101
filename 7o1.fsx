let rec bubble (xs:int list) =
  if List.isEmpty xs then
    []
  else
    let x = List.head xs
    x
  let ys = List.tail xs in if List.isEmpty ys then [x] else let y = List.head ys in if x < y then x :: bubble ys else y :: bubble (x::List.tail ys)

let bsort xs =
  List.fold (fun acc _ -> bubble acc) xs xs

printf "Bubblesort: %A\n" (bubble [10;9;8;7;6;5;4;3;2;1])
