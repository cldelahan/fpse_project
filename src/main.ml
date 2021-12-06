(*
  Main file for throw-away testing.
  Author: Conner Delahanty
  Date: 12/1/21
*)

open Graph

(* Main function *)
let () = 
  (*
  let instance = Broql.empty in
  Broql.add_node instance "N1" ~json:"{name: \"Conner\", age: \"22\"}";
  Broql.add_node instance "N2" ~json:"{name: \"Vini\", age: \"22\"}";
  Broql.save instance "./db.broql"
  *)
  let instance = Broql.load "./db.broql" in
  let att = Broql.get_attr instance ~name:"name" "N1" in
  match att with
  | None -> Printf.printf "%s\n" "No attribute found"
  | Some (str) -> Printf.printf "%s\n" str
