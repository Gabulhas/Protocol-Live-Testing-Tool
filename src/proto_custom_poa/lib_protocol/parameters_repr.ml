type t = {
  (*Define the parameters to initialize the protocol with*)
  constants : Constants_repr.parametric;
}
[@@deriving encoding]
