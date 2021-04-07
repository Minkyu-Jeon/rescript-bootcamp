let input =
  Node.Fs.readFileAsUtf8Sync("input/day5.sample.txt")
  ->Js.String2.split("\n")
  ->Belt.Array.map(x => x->Js.String2.split(""))

type seatRange = {
  max: int,
  min: int,
}

let mapDirectivesToSeatID = (input, maxRow, maxCol) => {
  input->Belt.Array.map(x => {
    let fbPositionDirective = x->Belt.Array.slice(~offset=0, ~len=6)
    let lrPositionDirective = x->Belt.Array.slice(~offset=7, ~len=2)
    let lastFbDirective = x->Belt.Array.getExn(6)
    let lastLrDirective = x->Belt.Array.getExn(9)

    let rowResult = fbPositionDirective->Belt.Array.reduce({min: 0, max: maxRow}, (acc, char) => {
      if char == "F" {
        {...acc, max: (acc.min + acc.max) / 2}
      } else if char == "B" {
        {...acc, min: (acc.min + acc.max) / 2 + 1}
      } else {
        acc
      }
    })

    let colResult = lrPositionDirective->Belt.Array.reduce({min: 0, max: maxCol}, (acc, char) => {
      if char == "L" {
        {...acc, max: (acc.min + acc.max) / 2}
      } else if char == "R" {
        {...acc, min: (acc.min + acc.max) / 2 + 1}
      } else {
        acc
      }
    })

    let row = lastFbDirective == "F" ? rowResult.min : rowResult.max
    let col = lastLrDirective == "L" ? colResult.min : colResult.max

    row * 8 + col
  })
}

let calcurateMySeatID = input => {
  let length = input->Belt.Array.length - 2
  
  input
  ->Belt.Array.slice(~offset=0, ~len=length)
  ->Belt.Array.mapWithIndex((i, seatID) => {
    let nextSeatID = input->Belt.Array.getExn(i + 1)
    if nextSeatID - seatID == 2 {
      nextSeatID - 1
    } else {
      0
    }
  })
  ->Belt.Array.keep(x => {x > 0})
  ->Belt.Array.get(0)
  ->Belt.Option.getWithDefault(0)
}

let result =
  input
  ->mapDirectivesToSeatID(127, 7)
  ->Belt.SortArray.stableSortBy(Pervasives.compare)
  ->calcurateMySeatID

result->Js.log
