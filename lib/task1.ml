
   open Types

   (* Function to calculate the score of a throw *)
   let throw_score (t : throw) : int =
     match t with
     | S n -> n
     | D n -> 2 * n
     | T n -> 3 * n
   
  
   (* Double Throws *)
   let double_throws = List.init 20 (fun x -> D (x + 1)) @ [D 25]
   
   (* Possible Throws *)
   let init_throws = List.init 20 (fun x -> S (x + 1)) @ [S 25] @ double_throws @ List.init 20 (fun x -> T (x + 1))
   
   let compute_checkouts (target : int) : checkouts =
     let rec aux pts remaining throws_type history =
       (* Passámos do número de pts *)
       if pts < 0 then []
       (* Atingimos o target *)
       else if pts = 0 then
         match history with
         | [] -> [] (* Não há lançamentos *)
         | last_throw :: _ ->
             (match last_throw with
             | D _ -> [List.rev history] (* Último throw é D, invertemos porque quando construímos history está ao contrário *)
             | _ -> []) (* É inválido *)
       (* Já não há lançamentos *)
       else if remaining = 0 then []
       else
         throws_type
         |> List.fold_left (fun acc throw ->
              let new_pts = pts - throw_score throw in
              let new_history = throw :: history in
              let next_throws = if remaining = 1 then double_throws else init_throws in
              (* Se o throw for o último, só podemos usar D *)
              acc @ aux new_pts (remaining - 1) next_throws new_history
              (* Concatenar o acc às seguintes combinações da chamada recursiva *)
            ) []
     in
     (* Caso base *)
     aux target 3 init_throws []
   