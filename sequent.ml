type sequent = {
    var_before : int list;
    assumptions : Formula.formula list;
    var_after : int list;
    conclusions : Formula.formula list;
};;

type structural = TODO;;
type inference = Theorem of Formula.axiom_request
    | Structural of structural;;

type proof = {
    top : sequent list;
    bottom : sequent;
    inference : inference;
};;