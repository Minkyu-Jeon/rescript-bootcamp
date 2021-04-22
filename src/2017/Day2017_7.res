type data = {weight: int, children: Belt_SetString.t}

let parse = input => {
  input
  ->Array.map(x => x->Js.String2.replaceByRe(%re("/[\(\),\->]/g"), "")->Js.String2.split(" "))
  ->Array.map(x => x->Array.keep(x => x != "")->List.fromArray)
  ->Array.reduce(Map.String.empty, (acc, item) => {
    let d = switch item {
    | list{_, weight, ...children} =>
      Some({
        weight: weight->Int.fromString->Option.getExn,
        children: Set.String.fromArray(children->List.toArray),
      })
    | _ => None
    }

    d->Option.mapWithDefault(acc, d => acc->Map.String.set(item->List.headExn, d))
  })
}

let input = Util.parseInputByNewline("input/2017/day7.sample.txt")->parse

let solve1 = input => {
  let parents =
    input->Map.String.reduce(Set.String.empty, (acc, _, v) => acc->Set.String.union(v.children))
  input->Map.String.keysToArray->Array.getBy(key => parents->Set.String.has(key) == false)
}

let p1 = input->Map.String.keep((_, v) => !(v.children->Set.String.isEmpty))->solve1
p1->Js.log

let mm = () => {
  let m = ref(Map.String.empty)

  let rec addAllChildren = (input, key, totalAcc) => {
    switch m.contents->Map.String.get(key) {
    | Some(value) => value
    | None => {
        // 해당 키 기준으로 하위 항목들을 모두 더해줌
        let item = input->Map.String.getExn(key)
        let children = item.children

        switch children->Set.String.isEmpty {
        | true => item.weight
        | false => {
            let value =
              item.weight +
              children->Set.String.reduce(totalAcc, (acc, key) => {
                acc + input->addAllChildren(key, totalAcc)
              })

            m := m.contents->Map.String.set(key, value)
            value
          }
        }
      }
    }
  }

  addAllChildren
}
let memoizedAddAllChildren = mm()

let rec addAllChildren = (input, key, totalAcc) => {
  // 해당 키 기준으로 하위 항목들을 모두 더해줌
  let item = input->Map.String.getExn(key)
  let children = item.children

  switch children->Set.String.isEmpty {
  | true => item.weight
  | false =>
    item.weight +
    children->Set.String.reduce(totalAcc, (acc, key) => {
      acc + input->addAllChildren(key, totalAcc)
    })
  }
}

let allSome = arr => {
  let keptMap = arr->List.keepMap(Garter.Fn.identity)
  switch keptMap->List.length == arr->List.length {
  | true => Some(keptMap)
  | false => None
  }
}

let parseFrequencyMap = arr => {
  let frequency =
    arr
    ->Array.reduce(Map.Int.empty, (acc, (sum, _)) =>
      acc->Map.Int.set(sum, acc->Map.Int.getWithDefault(sum, 0) + 1)
    )
    ->Map.Int.reduce(Map.Int.empty, (acc, k, v) => acc->Map.Int.set(v, k))

  frequency->Map.Int.size <= 1 ? None : Some(frequency)
}

let getLeastFrequentDiffPair = (arr: array<(int, string)>) => {
  arr
  ->parseFrequencyMap
  ->Option.flatMap(freq => {
    list{freq->Map.Int.maximum, freq->Map.Int.minimum}
    ->allSome
    ->Option.flatMap(l => {
      switch l {
      | list{(_, maxVal), (_, minVal)} =>
        arr->Map.Int.fromArray->Map.Int.get(minVal)->Option.map(key => (key, minVal - maxVal))
      | _ => None
      }
    })
  })
}

// memoizedAddAllChildren: 1187회 호출
// addAllChildren: 1415회 호출
let rec solve2 = (key, diff) => {
  let item = input->Map.String.getExn(key)
  let children = item.children->Set.String.toArray

  let leastFrequentItem =
    children->Array.map(x => (input->addAllChildren(x, 0), x))->getLeastFrequentDiffPair

  switch leastFrequentItem {
  | None => item.weight - diff
  | Some((newKey, diff)) => newKey->solve2(diff)
  }
}

let p2 = p1->Option.map(x => solve2(x, 0))

p2->Js.log
