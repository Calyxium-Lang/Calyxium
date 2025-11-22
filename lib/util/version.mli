(** Version information and system detection utilities. *)

val version : int * int * int
(** The current version of the software as a triple [(major, minor, patch)]. *)

val codename : string
(** A human-readable codename for this software version. *)

val detect_system : unit -> string
(** [detect_system ()] returns a string describing the current operating system
    or environment. *)

val version_string : unit -> string
(** [version_string ()] returns the software version as a human-readable string,
    e.g., "1.2.3 (Codename)". *)
