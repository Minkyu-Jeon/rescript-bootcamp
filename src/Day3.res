let input = Node.Fs.readFileAsUtf8Sync("input/day3.sample.txt");

let rec countTree = (totalLine, index, treeCount) => {
    switch totalLine {
        | [[]] => treeCount
        | _ => {
            let line = totalLine->Belt.Array.get(0)
            switch line {
                | None => treeCount
                | Some(value) => {
                    let lineLength = value->Belt.Array.length
                    let rest = totalLine->Belt.Array.sliceToEnd(1)
                    let nextIndex = index + 3
                    if value->Belt.Array.get(index) == Some("#") {
                        countTree(rest, mod(nextIndex, lineLength), treeCount + 1)
                    }
                    else {
                        countTree(rest, mod(nextIndex, lineLength), treeCount)
                    }
                }
            }
        }
    }
}


// Js.String2 : data-first vs Js.String : data-last

input->Js.String2.split("\n")
    ->Belt.Array.map(x => x->Js.String2.split(""))
    ->countTree(0, 0)
    ->Js.log
