let p = {
  Sequent.top = [];
  Sequent.bottom = {
    assumptions = [Formula.Atom 3];
    conclusions = [Formula.Atom 3];
  };
  inference = Sequent.Logical Sequent.Axiom;
};;

print_endline "Hello world";;

print_endline (Sequent.display_proof p);;
