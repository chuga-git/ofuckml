open Printf

type token = LAngle | RAngle | Plus | Minus | Dot | Comma | LSquare | RSquare

let lexer (text : string) : token list =
  let lex acc = function
    | '<' -> LAngle :: acc
    | '>' -> RAngle :: acc
    | '-' -> Minus :: acc
    | '+' -> Plus :: acc
    | '.' -> Dot :: acc
    | ',' -> Comma :: acc
    | '[' -> LSquare :: acc
    | ']' -> RSquare :: acc
    | _ -> acc
  in
  String.to_seq text |> List.of_seq |> List.fold_left lex [] |> List.rev

let eval (tokens : token list) =
  let cells = Array.make 30_000 0 in
  let instrs = Array.of_list tokens in

  let rec jump_right ip nest =
    match instrs.(ip) with
    | LSquare -> jump_right (ip + 1) (nest + 1)
    | RSquare ->
        let v = nest - 1 in
        if v = 0 then ip + 1 else jump_right (ip + 1) (nest - 1)
    | _ -> jump_right (ip + 1) nest
  in

  let rec jump_left ip nest =
    match instrs.(ip) with
    | RSquare -> jump_left (ip - 1) (nest + 1)
    | LSquare ->
        let v = nest - 1 in
        if v = 0 then ip + 1 else jump_left (ip - 1) (nest - 1)
    | _ -> jump_left (ip - 1) nest
  in

  let rec run ip dp =
    if ip < Array.length instrs && ip >= 0 then
      match instrs.(ip) with
      | Dot ->
          cells.(dp) |> Char.chr |> printf "%c";
          run (ip + 1) dp
      | Plus ->
          cells.(dp) <- cells.(dp) + 1;
          run (ip + 1) dp
      | Minus ->
          cells.(dp) <- cells.(dp) - 1;
          run (ip + 1) dp
      | Comma -> cells.(dp) <- input_byte stdin
      | RAngle -> run (ip + 1) (dp + 1)
      | LAngle -> run (ip + 1) (dp - 1)
      | LSquare ->
          if cells.(dp) = 0 then run (jump_right (ip + 1) 1) dp
          else run (ip + 1) dp
      | RSquare ->
          if cells.(dp) <> 0 then run (jump_left (ip - 1) 1) dp
          else run (ip + 1) dp
    else if ip = Array.length instrs then printf "EOF\n"
    else printf "Suspicious termination at ip = %d\n" ip
  in
  run 0 0

let () =
  if Array.length Sys.argv <> 2 then print_endline "Usage: ./ofuckml <file>"
  else
    let ic = open_in Sys.argv.(1) in
    let text = In_channel.input_all ic in
    text |> lexer |> eval
