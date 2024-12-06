(* read \n delimited lines from a file *)
let read_lines file =
    let contents = In_channel.with_open_bin file In_channel.input_all in
    String.split_on_char '\n' contents

let str_is_not_empty s = String.length s > 0

let parse_line line = 
    let split = String.split_on_char ' ' line in
    let filtered = List.filter str_is_not_empty split in
    List.map int_of_string filtered

let%test "parse_line" =
    List.equal Int.equal [0; 1; 3; 22; 7] (parse_line "0 1 3 22 7")

let%test "parse_line_extra_whitespace" =
    List.equal Int.equal [3; 4] (parse_line "3   4  ")

let parse_lines lines =
    List.map parse_line (List.filter str_is_not_empty lines)

let sort_and_split_lists: int list list -> int list * int list = fun parsed_lines ->
    let left_list = ref [] in
    let right_list = ref [] in
    let f: int list -> unit = fun l ->
        assert (List.length l = 2);
        let arr = Array.of_list l in
        left_list := arr.(0)::!left_list;
        right_list := arr.(1)::!right_list; in
    List.iter f parsed_lines;
    (List.sort compare !left_list, List.sort compare !right_list)

let solver parsed_lines =
    let (left, right) = sort_and_split_lists parsed_lines in
    let process_pair: int -> int * int -> int = fun acc pair ->
        let (l, r) = pair in
        acc + Int.abs (l - r) in
    let zipped = List.combine left right in
    List.fold_left process_pair 0 zipped

let run filename = 
    let lines = read_lines filename in
    let parsed = parse_lines lines in
    solver parsed

let run_on_string str =
    let lines = String.split_on_char '\n' str in
    let parsed = parse_lines lines in
    solver parsed

let sample_test_input = "3   4
4   3
2   5
1   3
3   9
3   3
"

let%test "sample_input_solver" =
    run_on_string sample_test_input = 11

