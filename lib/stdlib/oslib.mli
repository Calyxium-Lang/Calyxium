module Os : sig
  val getenv : string -> string option
  val setenv : string -> string -> unit
  val unlink : string -> unit
  val system : string -> int
end
