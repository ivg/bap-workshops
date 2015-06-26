open Bap.Std
       
(** Program.  *)
module Program : sig
  (* {BEGIN: program-term} *)
  (** Program is a collection of function terms. *)
  type t = program term
  include Regular with type t := t

  (** [create ()] creates an empty program. *)
  val create : unit -> t

  (** [lift roots] takes a set of function starts and a whole
      program cfg and returns a program term. *)
  val lift : symtab -> program term

  (** [lookup t program id] is like {{!find}find} but performs deep
      lookup in the whole [program] for a term with a given [id].
      This function is memoized, so it has amortized O(1)
      complexity, with a wostcase complexity of $O(N)$, where $N$ is
      the total amount of terms in program.  *)
  val lookup : (_,'b) cls -> t -> tid -> 'b term option

  (** [parent t program id] is [Some p] iff [find t p id <> None]  *)
  val parent : ('a,'b) cls -> t -> tid -> 'a term option

  (* {END} *)

  (* {BEGIN: program-builder}  *)
  (** Program builder.  *)
  module Builder : sig
    type t
    (** Initializes an empty builder.  *)
    val create : ?tid:tid  -> ?subs:int -> unit -> t

    (** [add_sub builder sub] appends a subroutine term to the
        program.  *)
    val add_sub : t -> sub term -> unit

    (** fixes the result  *)
    val result : t -> program term
  end
  (* {END} *)
end

(** Subroutine.  *)
module Sub : sig
  (* {BEGIN: sub-term} *)
  (** Subroutine is a set of blocks.  The first block of a function is
      considered an entry block.  *)
  type t = sub term
  include Regular with type t := t

  (** [create ?name ()] creates an empty subroutine with an optional
      name. *)
  val create : ?name:string -> unit -> t

  (** [lift entry] takes an basic block of assembler instructions,
      as an entry and lifts it to the subroutine term.  *)
  val lift : ?bound:(addr -> bool) -> block -> sub term

  (** [name sub] returns a subroutine name  *)
  val name : t -> string

  (** updates subroutine name *)
  val with_name : t -> string -> t

  (* {END} *)
  (* {BEGIN: sub-builder} *)
  (** Subroutine builder *)
  module Builder : sig
    type t
    (** initializes empty subroutine builder.  *)
    val create : ?tid:tid -> ?args:int -> ?blks:int -> ?name:string -> unit -> t
    (** appends a block to a subroutine  *)
    val add_blk : t -> blk term -> unit
    (** appends an argument  *)
    val add_arg : t -> arg term -> unit

    (** returns current result  *)
    val result : t -> sub term
  end
  (* {END} *)
end

(** Block.  *)
module Blk : sig
  (** Logically block consists of a set of {{!Phi}phi nodes}, a
      sequence of {{!Def}definitions} and a sequence of out-coming
      edges, aka {{!Jmp}jumps}. A colloquial term for this three
      entities is a {e block element}.

      The order of Phi-nodes can be specified in any order, as
      the executes simultaneously . Definitions are stored in the
      order of execution. Jumps are specified in the order in which
      they should be taken, i.e., jmp_n is taken only after
      jmp_n-1 and if and only if the latter was not taken. For
      example, if block ends with N jumps, where each n-th jump
      have destination named t_n and condition c_n then it
      would have the semantics as per the following OCaml program:

      {v
        if c_1 then jump t_1 else
        if c_2 then jump t_2 else
        if c_N then jump t_N else
        stop
      v}
  *)

  (* {BEGIN: blk-term-general} *)
  type t = blk term

  include Regular with type t := t

  (** Union type for all element types  *)
  type elt = [
    | `Def of def term
    | `Phi of phi term
    | `Jmp of jmp term
  ]

  (** [create ()] creates a new empty block.  *)
  val create : unit -> t

  (** [lift block] takes a basic block of assembly instructions and
      lifts it to a list of blk terms. The first term in the list
      is the entry. *)
  val lift : block -> blk term list

  (* where is documention??? *)
  val from_insn : insn -> blk term list

    (** [elts ~rev blk] return all elements of the [blk].  if [rev] is
      false or left unspecified, then elements are returned in the
      following order: $\Phi$-nodes, defs (in normal order), jmps in
      the order in which they will be taken.  If [rev] is true, the
      order will be the following: all jumps in the opposite order,
      then definitions in the opposite order, and finally
      $\Phi$-nodes. *)
  val elts : ?rev:bool -> t -> elt seq

  (* {END}  *)

  (* {BEGIN: blk-split} *)

  (** [split_while blk ~f] splits [blk] into two block: the first
      block holds all definitions for which [f p] is true and has
      the same tid as [blk]. The second block is freshly created and
      holds the rest definitions (if any). All successors of the
      [blk] become successors of the second block, which becomes the
      successor of the first block.

      Note: if [f def] is [true] for all blocks, then the second
      block will not contain any definitions, i.e., the result would
      be the same as of {{!split_bot}split_bot} function. *)
  val split_while : t -> f:(def term -> bool) -> t * t

  (** [split_after blk def] creates two new blocks, where the first
      block contains all defintions up to [def] inclusive, the
      second contains the rest.

      Note: if def is not in a [blk] then the first block will contain
      all the defintions, and the second block will be empty.  *)
  val split_after : t -> def term -> t * t

  (** [split_before blk def] is like {{!split_after}split_after} but
      [def] will fall into the second [blk] *)
  val split_before : t -> def term -> t * t

  (** [split_top blk] returns two blocks, where first block shares
      the same tid as [blk] and has all $\Phi$-nodes of [blk], but
      has only one destination, namely the second block. Second
      block has new tidentity, but inherits all definitions and
      jumps from the [blk]. *)
  val split_top : t -> t * t

  (** [split_op blk] returns two blocks, where first block shares
      the same tid as [blk], has all $\Phi$-nodes and definitions
      from [blk], but has only one destination, namely the second
      block. Second block has new tidentity, all jumps from the
      [blk]. *)
  val split_bot : t -> t * t

  (* {END} *)

  (* {BEGIN: blk-helpers} *)
  (** [map_exp b ~f] applies function [f] for each expression in
      block [b]. By default function [f] will be applied to all
      values of type [exp], including right hand sides of
      phi-nodes, definitions, jump conditions and targets. [skip]
      parameter allows to skip expressions from a given term kind.*)
  val map_exp :
    ?skip:[`phi | `def | `jmp] list -> (** defaults to [[]]  *)
    t -> f:(exp -> exp) -> t

  (** [map_lhs blk ~f] applies [f] to every left hand side variable
      in def and phi subterms of [blk]. Parameter [skip] allows to
      ignore particular terms.  E.g.,
      [map_lhs ~skip:[`phi] ~f:(substitute vars)].*)
  val map_lhs :
    ?skip:[`phi | `def ] list -> (** defaults to [[]]  *)
    t -> f:(var -> var) -> t

  (** [find_var blk var] finds a last definition of a variable [var]
      in a block [blk].  *)
  val find_var : t -> var -> [
      | `Phi of phi term
      | `Def of def term
    ] option

  (** [defines_var blk x] true if there exists such phi term or def
      term with left hand side equal to [x]  *)
  val defines_var : t -> var -> bool

  (** [uses_var blk x] true if variable [x] occurs on the right
      hand side of any phi or def term.  *)
  val uses_var : t -> var -> bool

  (** [dominated blk by:dom def] if [def] is dominated by [dom] in
      [blk].  *)
  (* Do you like this name? Probably it is bad...
     what about [is_before], [is_after] *)
  val dominated : t -> by:tid -> tid -> bool

  (* {END} *)

  (* {BEGIN: blk-builder} *)
  (** Builder interface.  *)
  module Builder : sig
    (** This interface provides an efficient way to build new
        blocks. It is also useful, when rebuilding existing block,
        as it allows to specify the [tid] of the block. It is a user
        responsibility to preserve the uniqueness of tidentifiers
        throughout the program instance.  *)
    type t

    (** [create ~tid ~phis ~defs ~jmp ()] creates a block builder.
        If [tid] parameter is specified, then the new block will
        have this tid. If any of [phis], [defs] or [jmps] parameters
        are specified, the provtided number would be used as a hint
        of the expected amount of the corresponding entries. Since
        it is the hint, it can mismatch with the actual size. The
        hint must be a positive number.  *)
    val create : ?tid:tid -> ?phis:int -> ?defs:int -> ?jmps:int -> unit -> t

    (** [init blk] creates a builder based on an existing
        block. If [copy_phis], [copy_defs] or [copy_jmps] is [true]
        (defaults to [false]), then prepopulate builder with
        corresponding terms from block [blk]. If [same_tid] is true
        (default), then a resulting block will have the same [tid]
        as block [blk]. Otherwise, a fresh new [tid] will be created. *)
    val init :
      ?same_tid :bool ->       (** defaults to [true]  *)
      ?copy_phis:bool ->       (** defaults to [false] *)
      ?copy_defs:bool ->       (** defaults to [false] *)
      ?copy_jmps:bool ->       (** defaults to [false] *)
      blk term -> t

    (** appends a definition  *)
    val add_def : t -> def term -> unit
    (** appends a jump  *)
    val add_jmp : t -> jmp term -> unit
    (** appends a phi node  *)
    val add_phi : t -> phi term -> unit
    (** appends generic element *)
    val add_elt : t -> elt -> unit
    (** returns current result  *)
    val result  : t -> blk term
  end
  (* {END} *)
end

(** Definition.  *)
module Def : sig
  (* {BEGIN: def-term} *)
  (** The definition is an assignment. The left hand side of an
      assignment is a variable, and the right side is an expression.

      The definition is the only way for a block to perform some
      side effects.
  *)

  type t = def term
  include Regular with type t := t

  (** [create x exp] creates definition [x := exp]  *)
  val create : var -> exp -> t

  (** returns the left hand side of a definition  *)
  val lhs : t -> var
  (** returns the right hand side of a definition  *)
  val rhs : t -> exp

  (** updates the lhs of definition  *)
  val with_lhs : t -> var -> t
  (** updates the right hand side of a definition  *)
  val with_rhs : t -> exp -> t

  (** [map_exp def f] applies [f] to a [rhs] of [def] and returns
      an updated definition. *)
  val map_exp : t -> f:(exp -> exp) -> t
  (* {END} *)
end

(** A control transfer operation.  *)
module Jmp : sig
  (* {BEGIN: jmp-description} *)
  (** Jmp is the only way to transfer control from block to block.
      Jumps are guarded with conditions. The jump should be taken
      only if its condition is evaluated to true.
      When control flow reaches an end of a block it should take the
      first jump with true condition. If there is no such jump, then
      program stops.

      Jumps are further subdivided into categories:
      - goto - is a local control transfer instruction. The label
        can be only local to subroutine;
      - call - transfer a control to another subroutine. A call
        contains a continuation, i.e., a label to which we're hoping
        to return after subroutine returns the control to us. Of
        course, called subroutine can in general return to another
        position, or not to return at all.
      - ret - performs a return from subroutine
      - int - calls to interrupt subroutine. If interrupt returns,
        then continue with the provided label.

  *)
  (* {END} *)
  type t = jmp term
  include Regular with type t := t

  (* {BEGIN: jmp-create} *)
  (** [create ?cond kind] creates a jump of given kind  *)
  val create : ?cond:exp -> jmp_kind -> t

  (** [create_call ?cond target] transfer control to subroutine
      [target] *)
  val create_call : ?cond:exp -> call  -> t

  (** [create_goto ?cond label] local jump  *)
  val create_goto : ?cond:exp -> label -> t

  (** [create_ret ?cond label] return from a procedure  *)
  val create_ret  : ?cond:exp -> label -> t

  (** [create_int ?cond int_number return] call interrupt subroutine  *)
  val create_int  : ?cond:exp -> int -> tid -> t

  (* {END} *)

  (* {BEGIN: jmp-accessors} *)
  (** [kind jmp] evaluates to a kind of jump  *)
  val kind : t -> jmp_kind

  (** [cond jmp] returns the jump guard condition  *)
  val cond : t -> exp

  (** [map_exp jmp ~f] applies [f] to each expression in a [jmp],
      e.g., conditions and indirect labels.  *)
  val map_exp : t -> f:(exp -> exp) -> t

  (** updated jump's guard condition  *)
  val with_cond : t -> exp -> t
  (** updated jump's kind  *)
  val with_kind : t -> jmp_kind -> t

  (* {END} *)
end

(** PHI-node  *)
module Phi : sig
  (* {BEGIN: phi-term-start} *)
  (** Phi nodes are used to represent a set of values, that can be
      assigned to a given variable depending on a taken control flow
      path Phi nodes should occur only in blocks that has more than
      one incoming edge, i.e., in blocks to which there is a transfer
      of control flow from more than one block.

      Each element of a phi-node corresponds to a particular
      incoming edge. *)
  type t = phi term
  include Regular with type t := t
    
  (** [create var label exp] creates a phi-node that associates a
      variable [var] with an expression [exp]. This expression
      should be selected if a control flow enters a block, that owns
      this phi-node from a block labeled with [label]. Example,
      [create x loop_header y].*)
  val create : var -> tid -> exp -> t

  (** [of_list var bindings] creates a phi-node, that for each pair
      of [label,exp] in the [bindings] list associates variable [var]
      with expression [exp] if control flow reaches this point via block
      labeled with [label].  *)
  val of_list : var -> (tid * exp) list -> t

  (* {END} *)
  (* {BEGIN: phi-term-interface} *)
  (** [values phi] enumerate all possible values.  *)
  val values : t -> (tid * exp) seq

  (** [lhs phi] returns a variable associated with a phi node  *)
  val lhs : t -> var

  (** [with_lhs phi var] updates a left hand side of [phi] with
      [var] *)
  val with_lhs : t -> var -> t

  (** [map_exp t ~f] applies [f] to all expressions on the right
      hand side of a phi-node [t] *)
  val map_exp : t -> f:(exp -> exp) -> t

  (** [update phi label exp] associates expression [exp] with a
      control flow path labeled with [label].  *)
  val update : t -> tid -> exp -> t

  (** [select phi label] takes the value corresponding to a control
      flow path marked with [label].   *)
  val select : t -> tid -> exp option

  (** [select_or_unknown phi label] is [exp] if
      [select phi label = Some exp], otherwise returns a
      [Bil.unknown] expression.     *)
  val select_or_unknown : t -> tid -> exp

  (** [remove_def id] removes definition with a given [id]  *)
  val remove : t -> tid -> t
  (* {END} *)

end

(** Subroutine argument.  *)
module Arg : sig

  (* {BEGIN: arg-term} *)
  (** In the IR model subroutines are not functions, that has a return
      value, but a more general subroutine that has a set of
      arguments, that can be used for  input, output or both
      purposes. *)

  type t = arg term
  include Regular with type t := t

  (** [create ?intent ?name typ] creates an argument. If intent is
      not specified it is left unkown.   *)
  val create : ?intent:intent -> ?name:string -> typ -> t

  (** [var arg] returns a variable associated with the argument.  *)
  val var : t -> var

  (* {END} *)

  (* {BEGIN: arg-term-intent} *)
  (** argument intention  *)
  type intent =
    | In                        (** input argument  *)
    | Out                       (** output argument *)
    | Both                      (** input/output    *)
  with bin_io, compare, sexp
  
  (** [intent arg] returns the argument intent. The [None] value
      denontes unknown intent.  *)
  val intent : t -> intent option

  (** [with_intent intent arg] updates argument intent  *)
  val with_intent : t -> intent -> t

  (** removes the intent from an argument  *)
  val with_unknown_intent : t -> t
  (* {END} *)
end

(** A control transfer to another subroutine.  *)
module Call : sig
  (* {BEGIN: call-term} *)
  (** calls have two-fold representation. From the intra-procedural
      point of view call is a transfer of control to the next
      address with a side effect of calling to other
      subroutine. From the iter-procedural point of view, call is
      transfer of control from caller to the callee, that may or may
      not result in a return to the caller side.  Thus each call is
      represented by two labels. The [target] label points to the
      procedure that is called, the [return] label denotes a block
      to which the control flow should (but mustn't) continue when
      called subroutine returns.  *)

  type t = call
  (* but call is not a term, it is a support data structure *)
  include Regular with type t := t


  (** [create ?return ~target ()] creates a call to the [target]
      subroutine. If [return] is not provided, that it is assumed that
      subroutine doesn't return. *)
  val create : ?return:label -> target:label -> unit -> t

  (** returns the target of the call  *)
  val target : t -> label

  (** returns call continuation  *)
  val return : t -> label option

  (** updates target  *)
  val with_target : t -> label -> t

  (** updates return continuation *)
  val with_return : t -> label -> t

  (** marks call as a "noreturn"  *)
  val with_noreturn : t -> t
  (* {END} *)

end

(** Target of a control flow transfer.  *)
module Label : sig
  (* {BEGIN: label} *)
  (** Labels can be direct, that are known to us. Or indirect, that
      are arbitrary expressions.  *)

  type t = label
  include Regular with type t := t

  (** [create ()] creates a new label with a freshly generated
      identifier.  *)
  val create : unit -> t

  (** [direct label] creates a direct label with a given identifier.  *)
  val direct : tid -> t

  (** [indirect exp] creates a label that is resolved to an
      expression [exp] *)
  val indirect : exp -> t

  (** updates the label  *)
  val change : ?direct:(tid -> tid) -> ?indirect:(exp -> exp) -> t -> t

  (* {END} *)
end
