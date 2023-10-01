type parametric = {
    (*Define the parameters to initialize the protocol with, and can be access as constants*)

}[@@deriving encoding]

type fixed = {

(*Constants that aren't parametric/are generic to the protocol*)
}[@@deriving encoding]


type t = {fixed : fixed; parametric : parametric}
[@@deriving encoding]

let all_of_parametric parametric = {fixed; parametric}
