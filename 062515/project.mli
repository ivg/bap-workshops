open Core_kernel.Std
open Bap.Std       
       
(** A project groups together all the information recovered from
    the underlying binary. It is also used for exchanging
    information between {{!section:project}passes}.  *)

type t

(* {BEGIN: from_file} *)
(** [from_file filename] creates a project from a binary file. The
    file must be in a format, supportable by some of our loader plugins,
    e.g., ELF, COFF, MACH-O, etc.

    @param on_warning is a function that will be called if some
    non-critical problem has occurred during loading file;

    @param name is a naming function, that allows to specify a
    name for a function starting at give address. If [None] is
    provided, then the default naming scheme will be used, i.e.,
    [sub_ADDR].

    @param backend allows to choose loader plugin

    @param roots allows to provide starting approximation of the
    roots for recursive disassembling procedure. Each root should
    be a start of a function.*)
val from_file :
  ?on_warning:(Error.t -> unit Or_error.t) ->
  ?backend:string ->
  ?name:(addr -> string option) ->
  ?roots:addr list ->
  string -> t Or_error.t

(* {END}  *)
(* {BEGIN: from_image} *)

(** [from_image image] is like {!from_file} but accepts already
    loaded image of a binary file *)
val from_image :
  ?name:(addr -> string option) ->
  ?roots:addr list ->
  image -> t Or_error.t

(* {END}  *)
(* {BEGIN: from_mem} *)

(** [from_mem arch mem] creates a project directly from a memory
    object. Parameters [name] and [roots] has the same meaning as
    in {!from_file}  *)
val from_mem :
  ?name:(addr -> string option) ->
  ?roots:addr list ->
  arch -> mem -> t Or_error.t

(* {END}  *)
(* {BEGIN: from_string} *)

(** [from_string arch string] creates a memory object from a
    provided string and uses {!from_mem} to create a project.  *)
val from_string :
  ?base:addr ->
  ?name:(addr -> string option) ->
  ?roots:addr list ->
  arch -> string -> t Or_error.t

(* {END}  *)
(* {BEGIN: from_bigstring} *)

(** [from_bigstring arch bigstring] is the same as {!from_string}
    but accepts a value of type [bigstring] *)
val from_bigstring :
  ?base:addr ->
  ?name:(addr -> string option) ->
  ?roots:addr list ->
  arch -> Bigstring.t -> t Or_error.t

(* {END}  *)
(* {BEGIN: accessors} *)


(** [arch project] reveals the architecture of a loaded file  *)
val arch : t -> arch

(** [disasm project] returns results of disassembling  *)
val disasm : t -> disasm

(** [program project] returns a program lifted into {{!sema}IR}  *)
val program : t -> program term

(** [symbols t] returns reconstructed symbol table  *)
val symbols : t -> symtab

(** [memory t] returns the memory as an interval tree marked with
    arbitrary values.   *)
val memory : t -> value memmap

(* {END}  *)
(* {BEGIN: updaters} *)

(** [with_program project program] updates a project program *)
val with_program : t -> program term -> t

(** [with_symbols project symbols] updates [project] symbols  *)
val with_symbols : t -> symtab -> t

(** [tag_memory project region tag value] tags given [region] of
    memory in [project] with a given [tag] and [value]. Example:
    [Project.tag_memory project tained color red]
*)
val tag_memory : t -> mem -> 'a tag -> 'a -> t

(** [substitute p region tag value] is like
    {{!tag_memory}tag_memory}, but it will also apply
    substitutions in the provided string value, as per OCaml
    standard library's [Buffer.add_substitute] function.

    Example: {[
      Project.substitute project comment "$symbol starts at $symbol_addr"
    ]}

    The following substitutions are supported:

    - [$section{_name,_addr,_min_addr,_max_addr}] - name of region of file
    to which it belongs. For example, in ELF this name will
    correspond to the section name

    - [$symbol{_name,_addr,_min_addr,_max_addr}] - name or address
    of the symbol to which this memory belongs

    - [$asm] - assembler listing of the memory region

    - [$bil] - BIL code of the tagged memory region

    - [$block{_name,_addr,_min_addr,_max_addr}] - name or address of a basic
    block to which this region belongs

    - [$min_addr, $addr] - starting address of a memory region

    - [$max_addr] - address of the last byte of a memory region. *)
val substitute : t -> mem -> string tag -> string -> t

(** [with_memory project] updates project memory. It is
    recommended to use {!tag_memory} and {!substitute} instead of this
    function, if possible.  *)
val with_memory : t -> value memmap -> t

(* {END}  *)
(* {BEGIN: data-storage} *)
(** {3 Extensible record}

    Project can also be viewed as an extensible record, where one
    can store arbitrary values. Example,
    {[
      let p = Project.set project color `green
    ]}
    This will set field [color] to a value [`green].*)

(** [set project field value] sets a [field] to a give value. If
    [field] was already set, then new value overrides the old
    one. Otherwise the field is added.  *)
val set : t -> 'a tag -> 'a -> t

(** [get project field] returns the value of the [field] if it
    exists *)
val get : t -> 'a tag -> 'a option

(** [has project field] checks whether field exists or not. Useful
    for fields of type unit, that actually isomorphic to bool fields,
    e.g., [if Project.has project mark]
*)
val has : t -> 'a tag -> bool

(* {END}  *)

(** {3 Registering and running passes}

    To add new pass one of the following [register_*] functions
    should be called.*)

type 'a register = ?deps:string list -> string -> 'a -> unit

(** An error that can occur when loading or running pass.
    - [Not_loaded name] pass with a given [name] wasn't loaded for
      some reason. This is a very unlikely error, indicating
      either a logic error in the plugin system implementation or
      something very weird, that we didn't expect.

    - [Is_duplicate name] more than one plugin were registered
      under this [name], either it is the same plugin or a name clash
      between to different we don't know.

    - [Not_found name] when we tried to load plugin with a given
      [name] we failed to find it in our search paths.

    - [Doesn't_register name] the plugin with a given [name]
      doesn't register a pass with the same name.

    - [Load_failed (name,problem)] a [problem] has occured, when
      we tried to dynamically link a plugin with a given [name]

    - [Runtime_error (name,exn)] when plugin with a given [name]
      was ran it raised an [exn].

*)
type error =
  | Not_loaded of string
  | Is_duplicate of string
  | Not_found of string
  | Doesn't_register of string
  | Load_failed of string * Error.t
  | Runtime_error of string * exn
with sexp_of

(** raised when a pass failed to load or to run. Note: this
    exception is raised only from two functions in this module, that
    state this in their documentation and has [_exn] suffix in their
    name. *)
exception Pass_failed of error with sexp

(* {BEGIN: register_pass_with_args} *)

(** [register_pass_with_args name pass] registers [pass] that
    requires command line arguments. The arguments will be passed
    in the first parameter of the [pass] function.

    Parameter [deps] is list of dependencies. Each dependency is a
    name of a pass, that should be run before the [pass]. If
    dependency pass is not registered it will be auto-loaded (See
    {!run_passes}) *)
val register_pass_with_args : (string array -> t -> t) register

(* {END} *)
(* {BEGIN: register_pass} *)
(** [register_pass ?deps name pass] registers project transformation,
    that doesn't require command line arguments.
    (See {!register_pass_with_args})*)
val register_pass : (t -> t) register
(* {END} *)
(* {BEGIN: register_pass'} *)
(** [register_pass ?deps name pass] registers [pass] that doesn't modify
    the project effect and is run only for side effect.
    (See {!register_pass_with_args})  *)
val register_pass': (t -> unit) register
(* {END} *)
(* {BEGIN: register_pass_with_args'} *)
(** [register_pass_with_args' name pass] register a [pass] that
    requires arguments for a side effect.
    (See {!register_pass_with_args}) *)
val register_pass_with_args' : (string array -> t -> unit) register
(* {END} *)

(** [run_passes ?library ?argv project] folds [project] over all
    registered passes. Passes are invoked in the order of
    registration.

    If pass has dependencies, then they will be run before the
    pass. The dependencies will be auto-loaded if needed. The
    auto-loading procedure will look for the file with the
    specified name (modulo hyphens) and [".plugin"] extension in
    current folder and all folders specified with [library]
    argument. If nothing found, then it will search for plugins of
    system ["bap.pass"] using findlib. If the dependency cannot be
    satisfied [run_pass] will terminate with error.

    If pass requires command line arguments then they will be
    provided to a pass as a first parameter. The arguments will be
    extracted from [argv] array (which defaults to [Sys.argv]) by
    removing all arguments that doesn't start with
    [--name-]. Then, from all command arguments that are left, the
    [--name-] prefix is substituted with [--]. For example, if
    [argv] contained [ [| "bap"; "-lcallgraph";
    "--callgraph-help"|]] then pass that registered itself under
    [callgraph] name will receive the following array of arguments
    [ [| "callgraph"; --help |] ]. That means, that plugins can't
    accept arguments that are anonymous or short options.

    Note: currently only the following syntax is supported:
     [--plugin-name-agrument-name=value], the following IS NOT
     supported [--plugin-name-argument-name value].
*)
val run_passes : ?library:string list -> ?argv:string array -> t -> t Or_error.t


(** [passes ?library ()] returns a transitive closure of all
    passes registered in the system so far.   *)
val passes : ?library:string list -> unit -> string list Or_error.t


(** [run_passes_exn proj] is the same as {!run_passes}, but raises
    an exception on error. Useful to provide custom error
    handling/printing.
    @raise Pass_failed if failed to load, or if plugin failed at runtime.
*)
val run_passes_exn : ?library:string list -> ?argv:string array -> t -> t


(** [passes_exn proj] is the same as {!passes}, but raises
    an exception on error. Useful to provide custom error
    handling/printing.
    @raise Pass_failed if failed to load some plugin *)
val passes_exn : ?library:string list -> unit -> string list

