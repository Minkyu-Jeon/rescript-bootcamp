let parseInputByNewline = filename => Node.Fs.readFileAsUtf8Sync(filename)->Js.String2.split("\n")

let parseInputByNewlineMapToInt = filename =>
  filename
  ->parseInputByNewline
  ->Belt.Array.map(Belt.Int.fromString)
  ->Belt.Array.keepMap(Garter.Fn.identity)

let multiply_int = (x) => Belt.Array.reduce(x, 1, (acc, item) => acc * item)