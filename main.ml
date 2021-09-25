let p = Sequent.proof {
  top = [];
  bottom = Sequent.sequent {
    assumptions = [Formula.Atom 3];
    conclusions = [Formula.Atom 3];
  };
  inference = Sequent.Logical Sequent.Axiom;
};

print_endline "Hello world";;

Sequent.display_proof p;;
