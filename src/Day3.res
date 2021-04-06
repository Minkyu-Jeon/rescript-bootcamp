let input =
  Node.Fs.readFileAsUtf8Sync("input/day3.sample.txt")
  ->Js.String2.split("\n")
  ->Belt.Array.map(x => x->Js.String2.split(""))

let countTree = (totalLine, moveRight) => {
  totalLine
  ->Belt.Array.mapWithIndex((i1, x) => {
    let lineLength = x->Belt.Array.length
    let tile = x
    ->Belt.Array.keepWithIndex((_, i2) => {
      i2 == mod(i1 * moveRight, lineLength)
    })
    ->Belt.Array.get(0)
    ->Belt.Option.getWithDefault(".")
    tile == "#" ? 1 : 0
  })
  ->Belt.Array.reduce(0, (acc, val) => {acc + val})
}

let multiplyTreeCount = (traverses, input, reducer) => {
  traverses
  ->Belt.List.map(x => {
    let (right, down) = x
    let newInput = input->Belt.Array.keepWithIndex((_x, i) => mod(i, down) == 0)
    newInput->reducer(right)->Belt.Int.toFloat
  })
  ->Belt.List.reduce(1.0, (acc, treeCount) => {acc *. treeCount})
}

// {right: 1, down: 1}, {right: 3, down: 1}, {right: 5, down: 1}, {right: 7, down: 1}, {right: 1, down: 2}
let traverses = list{(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)}

// Js.String2 : data-first vs Js.String : data-last

traverses->multiplyTreeCount(input, countTree)->Js.log

// list{(3, 1)}->multiplyTreeCount(input, countTree)->Js.log
