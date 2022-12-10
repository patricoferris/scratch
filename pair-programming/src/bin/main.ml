let () =
  let name = "Alice" in
  let person = Person.v name 42 in
  Person.to_file "person.txt" person