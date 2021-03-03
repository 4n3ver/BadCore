namespace BadCore.Collections


type Weight = int

[<Struct>]
type DirectedGraph<'Vertex when 'Vertex: comparison> = private DirectedGraph of Map<'Vertex, Map<'Vertex, Weight>>

module DirectedGraph =
    let create (vertices: ('Vertex * ('Vertex * Weight) seq) seq): DirectedGraph<'Vertex> when 'Vertex: comparison =
        vertices
        |> Seq.map (Pair.map2 Map.ofSeq)
        |> Map.ofSeq
        |> DirectedGraph

    let directNeighbors (vertex: 'Vertex) (DirectedGraph graph: DirectedGraph<'Vertex>): ('Vertex * Weight) seq when 'Vertex: comparison =
        graph
        |> Map.tryFind vertex
        |> Option.map Map.toSeq
        |> Option.defaultValue Seq.empty

    let connected (vertex: 'Vertex) (graph: DirectedGraph<'Vertex>): ('Vertex * Weight) seq when 'Vertex: comparison =
        let rec visit queue visited =
            if (Seq.isEmpty queue) then
                visited
            else
                let visitNeighbor (k, _) = directNeighbors k graph
                let visited = Seq.append queue visited
                let queue = Seq.collect visitNeighbor queue
                visit queue visited

        visit (seq { (vertex, 1) }) Seq.empty
