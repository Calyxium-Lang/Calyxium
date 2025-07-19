module Thread = struct
  external spawn_thread : (unit -> unit) -> unit = "spawn_thread"
end
