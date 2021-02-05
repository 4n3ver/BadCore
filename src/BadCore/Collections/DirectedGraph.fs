namespace BadCore.Collections

[<Struct>]
type DirectedGraph<'k when 'k: comparison> = private DirectedGraph of Map<'k, Map<'k, int>>

module DirectedGraph =
    let private key = Pair.value1
    let private value = Pair.value2
    let private mapValue = Pair.map2

    let create (vertices: ('k * ('k * int) seq) seq): DirectedGraph<'k> when 'k: comparison =
        vertices
        |> Seq.map (mapValue Map.ofSeq)
        |> Map.ofSeq
        |> DirectedGraph

    let directNeighbors (key: 'k) (DirectedGraph graph: DirectedGraph<'k>): ('k * int) seq when 'k: comparison =
        graph
        |> Map.tryFind key
        |> Option.map Map.toSeq
        |> Option.defaultValue Seq.empty

    let connected (key: 'k) (graph: DirectedGraph<'k>): ('k * int) seq when 'k: comparison =
        let rec visit queue visited =
            if (Seq.isEmpty queue) then
                visited
            else
                let visitNeighbor (k, _) = directNeighbors k graph
                let visited = Seq.append queue visited
                let queue = Seq.collect visitNeighbor queue
                visit queue visited

        visit (seq { (key, 1) }) Seq.empty
