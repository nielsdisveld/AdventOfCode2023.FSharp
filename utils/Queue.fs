namespace Utils

module Queue =
    type Queue<'T> =
        | Queue of 'T list * 'T list

        member this.IsEmpty =
            match this with
            | Queue([], []) -> true
            | _ -> false

        member this.Enqueue e =
            match this with
            | Queue(fs, bs) -> (Queue(e :: fs, bs)): Queue<'T>

        member this.EnqueueMany es =
            es |> Seq.fold (fun (q: Queue<_>) e -> q.Enqueue e) this

        member this.Dequeue =
            match this with
            | Queue([], []) -> failwith "Empty queue."
            | Queue(fs, b :: bs) -> b, Queue(fs, bs)
            | Queue(fs, []) ->
                let bs = fs |> List.rev
                bs.Head, Queue([], bs.Tail)

        static member Empty = (Queue([], []): Queue<'T>)
