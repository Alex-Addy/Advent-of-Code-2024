let usage_msg = "aoc [-verbose] <day_number>"
let verbose = ref false
let day_number = ref 50

let anon_fun arg =
        day_number := int_of_string arg

let speclist = 
        [("-verbose", Arg.Set verbose, "Output debug information")]

let () =
        Arg.parse speclist anon_fun usage_msg;
        let result = match !day_number with
                  01 -> Lib.Day01.run "input/day01.txt"
                | _ -> raise (Invalid_argument "day not yet implemented") in
        Printf.printf "Result for day %d: %d\n" !day_number result

