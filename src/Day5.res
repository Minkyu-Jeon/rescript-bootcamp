let input =
  Node.Fs.readFileAsUtf8Sync("input/day5.sample.txt")
  ->Js.String2.split("\n")
  ->Belt.Array.map(x => x->Js.String2.split(""))

type seatRange = {
  max: int,
  min: int,
}

let updateRecord = (minmaxRecord, char) => {
  if char == "F" || char == "L" {
    {...minmaxRecord, max: (minmaxRecord.min + minmaxRecord.max) / 2}
  } else {
    {...minmaxRecord, min: (minmaxRecord.min + minmaxRecord.max) / 2 + 1}
  }
}

let toSeatId = (maxRow, maxCol, x) => {
  let fbPositionDirective = x->Belt.Array.slice(~offset=0, ~len=6)
  let lrPositionDirective = x->Belt.Array.slice(~offset=7, ~len=2)

  let lastFbDirective = x->Belt.Array.getExn(6)
  let lastLrDirective = x->Belt.Array.getExn(9)

  let rowResult = fbPositionDirective->Belt.Array.reduce({min: 0, max: maxRow}, updateRecord)
  let colResult = lrPositionDirective->Belt.Array.reduce({min: 0, max: maxCol}, updateRecord)

  let row = lastFbDirective == "F" ? rowResult.min : rowResult.max
  let col = lastLrDirective == "L" ? colResult.min : colResult.max

  row * 8 + col
}

let mapSeatIDAndSortArray = input => {
  let toThisPlaneSeatId = toSeatId(127, 7) // tacit / point-free
  input->Belt.Array.map(toThisPlaneSeatId)->Belt.SortArray.stableSortBy((a, b) => b->compare(a))
}

let toSeatIDPair = seatIds => {
  let length = seatIds->Belt.Array.length - 1
  let subSeatIds = seatIds->Belt.Array.slice(~offset=1, ~len=length)

  seatIds->Belt.Array.zip(subSeatIds)
}

let toDiffAndSeatID = ((seat1, seat2)) => (seat1 - seat2, seat1 - 1)

let p1 = input->mapSeatIDAndSortArray->Belt.Array.getExn(0)

p1->Js.log

let p2 =
  input
  ->mapSeatIDAndSortArray
  ->toSeatIDPair
  ->Belt.Array.map(toDiffAndSeatID)
  ->Belt.Array.keep(((diff, _)) => diff == 2)
  ->Belt.Array.get(0)
  ->Belt.Option.mapWithDefault(0, ((_, seatID)) => seatID)

p2->Js.log
