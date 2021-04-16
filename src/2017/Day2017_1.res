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
