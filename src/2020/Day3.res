let input =
  Node.Fs.readFileAsUtf8Sync("input/2020/day3.sample.txt")
  ->Js.String2.split("\n")
  ->Belt.Array.map(x => x->Js.String2.split(""))

// 좌표를 받으면 ex) (3, 1)
// 2차원 리스트로 변경하고
// flatten시키고
// 필요없는 칸 지우고
// # 이면 1 아니면 0으로 변환해서
// 더한다

// https://rescript-lang.org/docs/manual/latest/api/belt/array#concatmany

let countTree = (totalLine, traverse) => {
  let (right, down) = traverse

  let lineLenth = totalLine->Belt.Array.getExn(0)->Belt.Array.length

  totalLine
  ->Belt.Array.concatMany
  ->Belt.Array.keepWithIndex((_x, i) => {
    let row = i / lineLenth
    let col = mod(i, lineLenth)
    let targetCol = right * row / down

    mod(row, down) == 0 && mod(targetCol, lineLenth) == col
  })
  ->Belt.Array.map(x => x == "#" ? 1 : 0)
  ->Belt.Array.reduce(0, (acc, val) => acc + val)
  ->Belt.Int.toFloat
}

let countTreeInThisInput = input->countTree

let traverses = list{(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)}

let p1 = (3, 1)->countTreeInThisInput
p1->Js.log

let p2 =
  traverses->Belt.List.map(x => x->countTreeInThisInput)->Belt.List.reduce(1.0, (acc, item) => acc *. item)
p2->Js.log
