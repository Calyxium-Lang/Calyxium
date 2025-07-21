module Os = struct
  let getenv = Sys.getenv_opt
  let setenv key value = Unix.putenv key value
  let unlink = Sys.remove
  let system = Sys.command
end
