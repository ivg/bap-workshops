open Core_kernel.Std
open Bap.Std

(** Term is a building block of the
    {{!sema}Intermediate Representation} of the binary program.

    This module provides functions that are overloaded for
    different term classes. Term class is denoted with an explicit
    instance of type [('a,'b)cls], where ['a] stands for the parent
    term and ['b] for the child term.

    {2 Example}

    Give a block

    {[# let b = Blk.create ();;]}
    {v val b : Blk.t =
    00000003: v}


    We can append a definition to it with an overloaded
    [Term.append]

    {[# let b = Term.append def_t b d_1;;]}
    {v val b : blk term =
    00000003:
    00000001: x := y + z
    v}

    Update a value of a definition in the block:


    {[# let b = Term.update def_t b d_2;;]}
    {v val b : blk term =
    00000003:
    00000001: x := true
    v}

*)

(** term type  *)
type 'a t = 'a term

(* {BEGIN: general-functions} *)
type 'a t = 'a term
(** [clone term] creates an object with a fresh new identifier
    that has the same contents as [term], i.e., that is
    syntactically the same. *)
val clone : 'a t -> 'a t

(** [same x y] returns true if [x] and [y] represents the same
    entity, i.e., [Tid.(tid x = tid y)] *)
val same : 'a t -> 'a t -> bool

(** [tid entity] returns the unique identifier of the [entity]  *)
val tid : 'a t -> tid

(* {END} *)
(* {BEGIN: accessors} *)

(** [length t p] returns the amount of terms of type [t] in the
    parent term [p]
    Term.length blk_t sub 

 *)
val length : ('a,'b) cls -> 'a t -> int

(** [find t p id] is [Some c] if term [p] has a subterm of type [t]
    such that [tid c = id].  *)
val find : ('a,'b) cls -> 'a t -> tid -> 'b t option

(** [find t p id] like {!find} but raises [Not_found] if nothing
    is found.  *)
val find_exn : ('a,'b) cls -> 'a t -> tid -> 'b t

(* {END} *)
(* {BEGIN: enumerators} *)

(** [enum ?rev t p] enumerate all subterms of type [t] of the
    given term [p] 

    Term.enum ~rev:true def_t blk
*)
val enum : ?rev:bool -> ('a,'b) cls -> 'a t -> 'b t seq

(** [to_sequence ?rev t p] is a synonym for [enum]. *)
val to_sequence : ?rev:bool -> ('a,'b) cls -> 'a t -> 'b t seq

(* {END} *)
(* {BEGIN: updaters} *)
(** [update t p c] if term [p] contains a term with id equal to
    [tid c] then return [p] with this term substituted with [p] *)
val update : ('a,'b) cls -> 'a t -> 'b t -> 'a t

(** [change t p id f] if [p] contains subterm with of a given kind
    [t] and identifier [id], then apply [f] to this
    subterm. Otherwise, apply [f] to [None]. If [f] return [None],
    then remove this subterm (given it did exist), otherwise,
    update parent with a new subterm.  *)
val change : ('a,'b) cls -> 'a t -> tid -> ('b t option -> 'b t option) -> 'a t

(** [append t ~after:this p c] returns the [p] term with [c]
    appended after [this] term. If [after] is not specified, then
    append [def] to the end of the parent term (if it makes sense,
    otherwise it is just added).  If [this] doesn't occur in the
    [p] term then do nothing. The term tid is preserved.  *)
val append : ('a,'b) cls -> ?after:tid -> 'a t -> 'b t -> 'a t

(** [prepend t ~before:this p c] returns the [p] with [c] inserted
    before [this] term. If [before] is left unspecified, then
    insert the [c] at the beginning of the [p] if it is a
    sequence, otherwise just insert. If [this] is specified but
    doesn't occur in the [p] then [p] is returned as is.  In all
    cases, the returned term has the same [tid] as [p]. *)
val prepend : ('a,'b) cls -> ?before:tid -> 'a t -> 'b t -> 'a t

(** [remove t p id] returns a term that doesn't contain element
    with the given [id] 
    
    Term.remove (def_t : (blk,def) cls) blk (Term.tid def) 
*)
val remove : ('a,_) cls -> 'a t -> tid -> 'a t

(* {END} *)
    
(* {BEGIN: mappers} *)


(** [map t p ~f] returns term [p] with all subterms of type [t]
    mapped with function [f] *)
val map : ('a,'b) cls -> 'a t -> f:('b t -> 'b t) -> 'a t

(** [filter_map t p ~f] returns term [p] with all subterms of type
    [t] filter_mapped with function [f], i.e., all terms for which
    function [f] returned [Some thing] are substituted by the
    [thing], otherwise they're removed from the parent term *)
val filter_map : ('a,'b) cls -> 'a t -> f:('b t -> 'b t option) -> 'a t

(** [concat_map t p ~f] substitute subterm [c] of type [t] in
    parent term [p] with [f c]. If [f c] is an empty list, then
    [c] doesn't occur in a new parent term, if [f c] is a
    singleton list, then [c] is substituted with the [f c], like
    in [map]. If [f c] is a list of [n] elements, then in the
    place of [c] this [n] elements are inserted.  *)
val concat_map : ('a,'b) cls -> 'a t -> f:('b t -> 'b t list) -> 'a t

(** [filter t p ~f] returns a term [p] with subterms [c] for which
    [f c = false] removed. *)
val filter : ('a,'b) cls -> 'a t -> f:('b t -> bool) -> 'a t

(* {END} *)
(* {BEGIN: navigators} *)

(** [first t p] returns the first subterm of type [t] of a given
    parent [p] *)
val first : ('a,'b) cls -> 'a t -> 'b t option

(** [last t p] returns a last subterm of type [t] of a given
    parent [p] *)
val last  : ('a,'b) cls -> 'a t -> 'b t option

(** [next t p id] returns a term that preceeds a term with a given
    [id], if such exists.  *)
val next : ('a,'b) cls -> 'a t -> tid -> 'b t option

(** [next t p id] returns a term that is after a term with a given
    [id], if such exists.  *)
val prev : ('a,'b) cls -> 'a t -> tid -> 'b t option

(** [after t ?rev p tid] returns all subterms in term [p] that
    occur after a term with a given [tid]. if [rev] is [true] or
    omitted then terms are returned in the evaluation
    order. Otherwise they're reversed. If there is no term with a
    given [tid], then an empty sequence is returned. *)
val after : ('a,'b) cls -> ?rev:bool -> 'a t -> tid -> 'b t seq

(** [before t ?rev p tid] returns all term that occurs before
    defintion with given [tid] in blk. If there is no such
    definition, then the sequence will be empty.  @param rev has
    the same meaning as in {!after}.  *)
val before : ('a,'b) cls -> ?rev:bool -> 'a t -> tid -> 'b t seq

(* {END} *)
(* {BEGIN: attributes} *)

(** {2 Attributes}

    Terms attribute set can be extended, using {{!Value}universal
    values}. A value of type ['a tag] is used to denote an
    attribute of type ['a] with the name [Value.Tag.name tag].

    With the provided interface Term can be treated as an
    extensible record.
*)

(** [set_attr term attr value] attaches an [value] to attribute
    [attr] in [term] *)
val set_attr : 'a t -> 'b tag -> 'b -> 'a t

(** [get_attr term attr] returns a value of the given [attr] in
    [term] *)
val get_attr : 'a t -> 'b tag -> 'b option

(** [has_attr term attr] is [true] if [term] has attribute [attr]  *)
val has_attr : 'a t -> 'b tag -> bool

(** [del_attr term attr] deletes attribute [attr] from [term]  *)
val del_attr : 'a t -> 'b tag -> 'a t

(* {END} *)
