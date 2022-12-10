module Day6

let tuning =
    let buffer = Utils.getString 6

    let markerLen = 14

    let un i =
        let recent = buffer[i-markerLen .. i]
        recent |> Seq.countBy id |> Seq.length

    1 + (buffer
    |> Seq.mapi (fun i x -> un i)
    |> Seq.findIndex (fun i -> i=markerLen))