let input =
  Node.Fs.readFileAsUtf8Sync("input/2017/day1.sample.txt")
  ->Js.String2.split("")
  ->Array.map(Int.fromString)
  ->Array.keepMap(Garter.Fn.identity)

let rotateArray = array => array->Array.sliceToEnd(1)->Array.concat([array->Array.getExn(0)])

let solve = input => {
  input
  ->Array.zip(input->rotateArray)
  ->Array.map(((a, b)) => b - a == 0 ? a : 0)
  ->Garter.Math.sum_int
}

input->solve->Js.log


let solve2 = (input) => {
  let len = input->Array.length
  input
  ->Array.slice(~offset=0, ~len=len / 2)
  ->Array.zip(input->Array.slice(~offset=len / 2, ~len=len / 2))
  ->Array.reduce(0, (acc, (a, b)) => {
    acc + (a == b ? a + b : 0)
  })
}

input->solve2->Js.log